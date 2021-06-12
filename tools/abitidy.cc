// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// -*- Mode: C++ -*-
//
// Copyright (C) 2021 Google, Inc.
//
// Author: Giuliano Procida

/// @file
///
/// This file contains ABI XML manipulation routines and a main driver.
///
/// The libxml Tree API is used. The XPath API is not used as it proved
/// to be many times slower than direct traversal but only slightly more
/// convenient.

#include <fcntl.h>
#include <unistd.h>

#include <array>
#include <cassert>
#include <cstring>
#include <functional>
#include <ios>
#include <iostream>
#include <map>
#include <optional>
#include <set>
#include <sstream>
#include <string>
#include <unordered_map>
#include <unordered_set>
#include <vector>

#include <libxml/globals.h>
#include <libxml/parser.h>
#include <libxml/tree.h>

/// Convenience typedef referring to a namespace scope.
using namespace_scope = std::vector<std::string>;

/// Cast a C string to a libxml string.
///
/// @param str the C string (pointer)
///
/// @return the same thing, as a type compatible with the libxml API
static const xmlChar*
to_libxml(const char* str)
{
  return reinterpret_cast<const xmlChar*>(str);
}

/// Cast a libxml string to C string.
///
/// @param str the libxml string (pointer)
///
/// @return the same thing, as a type compatible with the C library API
static const char*
from_libxml(const xmlChar* str)
{
  return reinterpret_cast<const char*>(str);
}

/// Remove a node from its document and free its storage.
///
/// @param node the node to remove
static void
remove_node(xmlNodePtr node)
{
  xmlUnlinkNode(node);
  xmlFreeNode(node);
}

/// Remove an XML element and any immediately preceding comment.
///
/// @param node the element to remove
static void
remove_element(xmlNodePtr node)
{
  xmlNodePtr previous_node = node->prev;
  if (previous_node && previous_node->type == XML_COMMENT_NODE)
    remove_node(previous_node);
  remove_node(node);
}

/// Move a node to an element.
///
/// @param node the node to move
///
/// @param destination the destination element
static void
move_node(xmlNodePtr node, xmlNodePtr destination)
{
  xmlUnlinkNode(node);
  xmlAddChild(destination, node);
}

/// Move an XML element and any immediately preceding comment to another
/// element.
///
/// @param node the element to remove
///
/// @param destination the destination element
static void
move_element(xmlNodePtr node, xmlNodePtr destination)
{
  xmlNodePtr previous_node = node->prev;
  if (previous_node && previous_node->type == XML_COMMENT_NODE)
    move_node(previous_node, destination);
  move_node(node, destination);
}

/// Get child nodes of given node.
///
/// @param node the node whose children to fetch
///
/// @return a vector of child nodes
static std::vector<xmlNodePtr>
get_children(xmlNodePtr node)
{
  std::vector<xmlNodePtr> result;
  for (xmlNodePtr child = node->children; child; child = child->next)
    result.push_back(child);
  return result;
}

/// Fetch an attribute from a node.
///
/// @param node the node
///
/// @param name the attribute name
///
/// @return the attribute value, if present
static std::optional<std::string>
get_attribute(xmlNodePtr node, const char* name)
{
  std::optional<std::string> result;
  xmlChar* attribute = xmlGetProp(node, to_libxml(name));
  if (attribute)
    {
      result = from_libxml(attribute);
      xmlFree(attribute);
    }
  return result;
}

/// Store an attribute value.
///
/// @param node the node
///
/// @param name the attribute name
///
/// @param value the attribute value, optionally
static void
set_attribute(xmlNodePtr node, const char* name,
              const std::optional<std::string>& value)
{
  if (value)
    xmlSetProp(node, to_libxml(name), to_libxml(value.value().c_str()));
  else
    xmlUnsetProp(node, to_libxml(name));
}

/// Remove text nodes, recursively.
///
/// This simplifies subsequent analysis and manipulation. Removing and
/// moving elements will destroy formatting anyway. The only remaining
/// node types should be elements and comments.
///
/// @param node the node to process
static void
strip_text(xmlNodePtr node)
{
  if (node->type == XML_TEXT_NODE)
    remove_node(node);
  else if (node->type == XML_ELEMENT_NODE)
    for (xmlNodePtr child : get_children(node))
      strip_text(child);
}

/// Add text before / after a node.
///
/// @param node the node
///
/// @param after whether the next should go after
///
/// @param text the text
static void
add_text(xmlNodePtr node, bool after, const std::string& text)
{
  xmlNodePtr text_node = xmlNewTextLen(to_libxml(text.data()), text.size());
  if (after)
    xmlAddNextSibling(node, text_node);
  else
    xmlAddPrevSibling(node, text_node);
}

/// Format an XML elementy by adding internal indentation and newlines.
///
/// This makes the XML readable.
///
/// @param indentation what to add to the line indentation prefix
///
/// @param prefix the current line indentation prefix
///
/// @param node the node to format
static void
format_xml(const std::string& indentation, std::string prefix, xmlNodePtr node)
{
  std::vector<xmlNodePtr> children = get_children(node);
  if (children.empty())
    return;

  // The ordering of operations here is incidental. The outcomes we want
  // are: 1. an extra newline after the opening tag and indentation of
  // the closing tag to match, and 2. indentation and newline for each
  // child.
  add_text(children[0], false, "\n");
  add_text(children[children.size() - 1], true, prefix);
  prefix += indentation;
  for (xmlNodePtr child : children)
    {
      add_text(child, false, prefix);
      format_xml(indentation, prefix, child);
      add_text(child, true, "\n");
    }
}

/// Rewrite attributes using single quotes.
///
/// libxml uses double quotes but libabigail uses single quotes.
///
/// Note that libabigail does not emit attributes *containing* single
/// quotes and if it did it would escape them as &quot; which libxml
/// would in turn preserve. However, the code here will handle all forms
/// of quotes, conservatively.
///
/// Annotation comments can contain single quote characters so just
/// checking for any single quotes at all is insufficiently precise.
///
/// @param start a pointer to the start of the XML text
///
/// @param limit a pointer to just past the end of the XML text
static void
adjust_quotes(xmlChar* start, xmlChar* limit)
{
  const std::string open{"<!--"};
  const std::string close{"-->"};
  while (start < limit)
    {
      // Look for a '<'
      start = std::find(start, limit, '<');
      if (start == limit)
        break;
      if (start + open.size() < limit
          && std::equal(open.begin(), open.end(), start))
        {
          // Have a comment, skip to the end.
          start += open.size();
          xmlChar* end = std::search(start, limit, close.begin(), close.end());
          if (end == limit)
            break;
          start = end + close.size();
        }
      else
        {
          // Have some tag, search for the end.
          start += 1;
          xmlChar* end = std::find(start, limit, '>');
          if (end == limit)
            break;
          // In general, inside a tag we could find either ' or " being
          // used to quote attributes and the other quote character
          // being used as part of the attribute data. However, libxml's
          // xmlDocDump* functions use " to quote attributes and it's
          // safe to substitute this quote character with ' so long as '
          // does not appear within the attribute data.
          if (std::find(start, end, '\'') == end)
            for (xmlChar* c = start; c < end; ++c)
              if (*c == '"')
                *c = '\'';
          start = end + 1;
        }
    }
}

static const std::set<std::string> DROP_IF_EMPTY = {
  "elf-variable-symbols",
  "elf-function-symbols",
  "namespace-decl",
  "abi-instr",
  "abi-corpus",
  "abi-corpus-group",
};

/// Drop empty elements, if safe to do so, recursively.
///
/// @param element node to process
static void
drop_empty(xmlNodePtr node)
{
  if (node->type != XML_ELEMENT_NODE)
    return;
  for (xmlNodePtr child : get_children(node))
    drop_empty(child);
  // Do not drop the root element, even if empty.
  if (node->parent->type == XML_DOCUMENT_NODE)
    return;
  if (!node->children && DROP_IF_EMPTY.count(from_libxml(node->name)))
    remove_element(node);
}

/// Handle unreachable elements.
///
/// Reachability is defined to be union of contains, containing and
/// refers-to relationships for types, declarations and symbols. The
/// roots for reachability are the ELF elements in the ABI.
///
/// @param prune whether to prune unreachable elements
///
/// @param report whether to report untyped symbols
///
/// @param document the XML document to process
///
/// @return the number of untyped symbols
static size_t
handle_unreachable(bool prune, bool report, xmlDocPtr document)
{
  // ELF symbol names.
  std::set<std::string> elf_symbols;

  // Simple way of allowing two kinds of nodes: false=>type,
  // true=>symbol.
  using vertex_t = std::pair<bool, std::string>;

  // Graph vertices.
  std::set<vertex_t> vertices;
  // Graph edges.
  std::map<vertex_t, std::set<vertex_t>> edges;

  // Keep track of type / symbol nesting so we can identify contains,
  // containing and refers-to relationships.
  std::vector<vertex_t> stack;

  // Process an XML node, adding a vertex and possibly some edges.
  std::function<void(xmlNodePtr)> process_node = [&](xmlNodePtr node) {
    // We only care about elements and not comments, at this stage.
    if (node->type != XML_ELEMENT_NODE)
      return;

    // Is this an ELF symbol?
    if (strcmp(from_libxml(node->name), "elf-symbol") == 0)
      {
        const auto name = get_attribute(node, "name");
        if (name)
          elf_symbols.insert(name.value());
        // Early return is safe, but not necessary.
        return;
      }

    // Is this a type? Note that the same id may appear multiple times.
    const auto id = get_attribute(node, "id");
    if (id)
      {
        vertex_t type_vertex{false, id.value()};
        vertices.insert(type_vertex);
        const auto naming_typedef_id = get_attribute(node, "naming-typedef-id");
        if (naming_typedef_id)
          {
            // This is an odd one, there can be a backwards link from an
            // anonymous type to the typedef that refers to it. We could
            // either remove these links when pruning a typedef but it's
            // simpler just to pull in the typedef, even if nothing else
            // refers to it.
            vertex_t naming_typedef_vertex{false, naming_typedef_id.value()};
            edges[type_vertex].insert(naming_typedef_vertex);
          }
        if (!stack.empty())
          {
            // Parent<->child dependencies; record dependencies both ways to
            // avoid holes in XML types and declarations.
            const auto& parent = stack.back();
            edges[parent].insert(type_vertex);
            edges[type_vertex].insert(parent);
          }
        // Record the type.
        stack.push_back(type_vertex);
      }

    // Is this a (declaration expected to be linked to a) symbol?
    const auto symbol = get_attribute(node, "mangled-name");
    if (symbol)
      {
        vertex_t symbol_vertex{true, symbol.value()};
        vertices.insert(symbol_vertex);
        if (!stack.empty())
          {
            // Parent<->child dependencies; record dependencies both ways to
            // avoid making holes in XML types and declarations.
            //
            // Symbols exist outside of the type hierarchy, so choosing to make
            // them depend on a containing type scope and vice versa is
            // conservative and probably not necessary.
            const auto& parent = stack.back();
            edges[parent].insert(symbol_vertex);
            edges[symbol_vertex].insert(parent);
          }
        // Record the symbol.
        stack.push_back(symbol_vertex);
        // In practice there will be at most one symbol on the stack; we could
        // verify this here, but it wouldn't achieve anything.
      }

    // Being both would make the stack ordering ambiguous.
    if (id && symbol)
      {
        std::cerr << "cannot handle element which is both type and symbol\n";
        exit(1);
      }

    // Is there a reference to another type?
    const auto type_id = get_attribute(node, "type-id");
    if (type_id && !stack.empty())
      {
        // The enclosing type or symbol refers to another type.
        const auto& parent = stack.back();
        vertex_t type_id_vertex{false, type_id.value()};
        edges[parent].insert(type_id_vertex);
      }

    // Process recursively.
    for (auto child : get_children(node))
      process_node(child);

    // Restore the stack.
    if (symbol)
      stack.pop_back();
    if (id)
      stack.pop_back();
  };

  // Traverse the whole XML document and build a graph.
  for (xmlNodePtr node = document->children; node; node = node->next)
    process_node(node);

  // Simple DFS.
  std::set<vertex_t> seen;
  std::function<void(vertex_t)> dfs = [&](vertex_t vertex) {
    if (!seen.insert(vertex).second)
      return;
    auto it = edges.find(vertex);
    if (it != edges.end())
      for (auto to : it->second)
        dfs(to);
  };

  // Count of how many symbols are untyped.
  size_t untyped = 0;

  // Traverse the graph, starting from the ELF symbols.
  for (const auto& symbol : elf_symbols)
    {
      vertex_t symbol_vertex{true, symbol};
      if (vertices.count(symbol_vertex))
        {
          dfs(symbol_vertex);
        }
      else
        {
          if (report)
            std::cerr << "no declaration found for ELF symbol " << symbol << '\n';
          ++untyped;
        }
    }

  // This is a DFS with early stopping.
  std::function<void(xmlNodePtr)> remove_unseen = [&](xmlNodePtr node) {
    if (node->type != XML_ELEMENT_NODE)
      return;

    // Return if we know that this is a type to keep or drop in its
    // entirety.
    const auto id = get_attribute(node, "id");
    if (id)
      {
        if (!seen.count(vertex_t{false, id.value()}))
          remove_element(node);
        return;
      }

    // Return if we know that this is a declaration to keep or drop in
    // its entirety. Note that var-decl and function-decl are the only
    // elements that can have a mangled-name attribute.
    const char* node_name = from_libxml(node->name);
    if (strcmp(node_name, "var-decl") == 0
        || strcmp(node_name, "function-decl") == 0)
      {
        const auto symbol = get_attribute(node, "mangled-name");
        if (!(symbol && seen.count(vertex_t{true, symbol.value()})))
          remove_element(node);
        return;
      }

    // Otherwise, this is not a type, declaration or part thereof, so
    // process child elements.
    for (auto child : get_children(node))
      remove_unseen(child);
  };

  if (prune)
    // Traverse the document, removing unseen elements.
    for (xmlNodePtr node = document->children; node; node = node->next)
      remove_unseen(node);

  return untyped;
}

static const std::map<std::string, std::string> ANONYMOUS_TYPE_NAMES = {
  {"enum-decl", "__anonymous_enum__"},
  {"class-decl", "__anonymous_struct__"},
  {"union-decl", "__anonymous_union__"},
};

/// Normalise anonymous type names by removing the numerical suffix.
///
/// Anonymous type names take the form __anonymous_foo__N where foo is
/// one of enum, struct or union and N is an optional numerical suffix.
/// The suffices are senstive to processing order and do not convey
/// useful ABI information. They can cause spurious harmless diffs and
/// make XML diffing and rebasing harder.
///
/// @param node the XML node to process
static void
normalise_anonymous_type_names(xmlNodePtr node)
{
  if (node->type != XML_ELEMENT_NODE)
    return;

  const auto it = ANONYMOUS_TYPE_NAMES.find(from_libxml(node->name));
  if (it != ANONYMOUS_TYPE_NAMES.end())
    if (const auto attribute = get_attribute(node, "name"))
      {
        const auto& anon = it->second;
        const auto& name = attribute.value();
        // __anonymous_foo__123 -> __anonymous_foo__
        if (!name.compare(0, anon.size(), anon) &&
            name.find_first_not_of("0123456789", anon.size()) == name.npos)
          set_attribute(node, "name", anon);
      }

  for (auto child : get_children(node))
    normalise_anonymous_type_names(child);
}

/// Set of attributes that should be excluded from consideration when comparing
/// XML elements.
///
/// These attributes are omitted with --no-show-locs without changing the
/// meaning of the ABI. They can also sometimes vary between duplicate type
/// definitions.
static const std::unordered_set<std::string> IRRELEVANT_ATTRIBUTES = {
  {"filepath"},
  {"line"},
  {"column"},
};

/// Determine whether one XML element is a subtree of another.
///
/// XML elements representing types are sometimes emitted multiple
/// times, identically. Also, member typedefs are sometimes emitted
/// separately from their types, resulting in duplicate XML fragments.
///
/// Both these issues can be resolved by first detecting duplicate
/// occurrences of a given type id and then checking to see if there's
/// an instance that subsumes the others which can then be eliminated.
///
/// @param left the first element to compare
///
/// @param right the second element to compare
///
/// @return whether the first element is a subtree of the second
bool
sub_tree(xmlNodePtr left, xmlNodePtr right)
{
  // Node names must match.
  const char* left_name = from_libxml(left->name);
  const char* right_name = from_libxml(right->name);
  if (strcmp(left_name, right_name) != 0)
    return false;
  // Attributes may be missing on the left, but must match otherwise.
  for (auto p = left->properties; p; p = p->next)
  {
    const char* attribute_name = from_libxml(p->name);
    if (IRRELEVANT_ATTRIBUTES.count(attribute_name))
      continue;
    // EXCEPTION: libabigail emits the access specifier for the type
    // it's trying to "emit in scope" rather than for what may be a
    // containing type; so allow member-type attribute access to differ.
    if (strcmp(left_name, "member-type") == 0
        && strcmp(attribute_name, "access") == 0)
      continue;
    const auto left_value = get_attribute(left, attribute_name);
    assert(left_value);
    const auto right_value = get_attribute(right, attribute_name);
    if (!right_value || left_value.value() != right_value.value())
      return false;
  }
  // The left subelements must be a subsequence of the right ones.
  xmlNodePtr left_child = xmlFirstElementChild(left);
  xmlNodePtr right_child = xmlFirstElementChild(right);
  while (left_child && right_child)
    {
      if (sub_tree(left_child, right_child))
        left_child = xmlNextElementSibling(left_child);
      right_child = xmlNextElementSibling(right_child);
    }
  return !left_child;
}

/// Elminate non-conflicting / report conflicting type definitions.
///
/// This function can eliminate exact type duplicates and duplicates where there
/// is at least one maximal definition. It can report the remaining, conflicting
/// duplicate definitions.
///
/// If a type has duplicate definitions in multiple namespace scopes, these
/// should not be reordered. This function reports how many such types it finds.
///
/// @param eliminate whether to eliminate non-conflicting duplicates
///
/// @param report whether to report conflicting duplicate definitions
///
/// @param node the XML node to process
///
/// @return the number of types defined in multiple namespace scopes
size_t handle_duplicate_types(bool eliminate, bool report, xmlNodePtr node)
{
  // map of type-id to pair of set of namespace scopes and vector of xmlNodes
  std::unordered_map<
      std::string,
      std::pair<
          std::set<namespace_scope>,
          std::vector<xmlNodePtr>>> types;
  namespace_scope namespaces;

  // find all type occurrences
  std::function<void(xmlNodePtr)> dfs = [&](xmlNodePtr node) {
    if (node->type != XML_ELEMENT_NODE)
      return;
    const char* node_name = from_libxml(node->name);
    std::optional<std::string> namespace_name;
    if (strcmp(node_name, "namespace-decl") == 0)
      namespace_name = get_attribute(node, "name");
    if (namespace_name)
      namespaces.push_back(namespace_name.value());
    if (strcmp(node_name, "abi-corpus-group") == 0
        || strcmp(node_name, "abi-corpus") == 0
        || strcmp(node_name, "abi-instr") == 0
        || namespace_name)
      {
        for (auto child : get_children(node))
          dfs(child);
      }
    else
      {
        const auto id = get_attribute(node, "id");
        if (id)
          {
            auto& info = types[id.value()];
            info.first.insert(namespaces);
            info.second.push_back(node);
          }
      }
    if (namespace_name)
      namespaces.pop_back();
  };
  dfs(node);

  size_t scope_conflicts = 0;
  for (const auto& [id, scopes_and_definitions] : types)
    {
      const auto& [scopes, definitions] = scopes_and_definitions;

      if (scopes.size() > 1)
        {
          if (report)
            std::cerr << "conflicting scopes found for type '" << id << "'\n";
          ++scope_conflicts;
          continue;
        }

      const size_t count = definitions.size();
      if (count <= 1)
        continue;

      // Find a potentially maximal candidate by scanning through and retaining
      // the new definition if it's a supertree of the current candidate.
      std::vector<bool> ok(count);
      size_t candidate = 0;
      ok[candidate] = true;
      for (size_t ix = 1; ix < count; ++ix)
        if (sub_tree(definitions[candidate], definitions[ix]))
          {
            candidate = ix;
            ok[candidate] = true;
          }

      // Verify it is indeed maximal by scanning the definitions not already
      // known to be subtrees of the candidate.
      bool bad = false;
      for (size_t ix = 0; ix < count; ++ix)
        if (!ok[ix] && !sub_tree(definitions[ix], definitions[candidate]))
          {
            bad = true;
            break;
          }
      if (bad)
        {
          if (report)
            std::cerr << "conflicting definitions found for type '" << id
                      << "'\n";
          continue;
        }

      if (eliminate)
        // Remove all but the maximal definition.
        for (size_t ix = 0; ix < count; ++ix)
          if (ix != candidate)
            remove_element(definitions[ix]);
    }

  return scope_conflicts;
}

static const std::set<std::string> INSTR_VARIABLE_ATTRIBUTES = {
  "path",
  "comp-dir-path",
  "language",
};

/// Collect elements of an abi-instr by namespace.
///
/// Namespaces are not returned but are recursively traversed with the
/// namespace stack being maintained. Other elements are associated with
/// the current namespace.
///
/// @param node the node to traverse
///
/// @param namesapces the current stack of namespaces
///
/// @param child elements grouped by namespace scope
static std::map<namespace_scope, std::vector<xmlNodePtr>>
get_children_by_namespace(xmlNodePtr node)
{
  std::map<namespace_scope, std::vector<xmlNodePtr>> result;
  namespace_scope scope;

  std::function<void(xmlNodePtr)> process = [&](xmlNodePtr node) {
    if (node->type != XML_ELEMENT_NODE)
      return;
    std::optional<std::string> namespace_name;
    const char* node_name = from_libxml(node->name);
    if (strcmp(node_name, "namespace-decl") == 0)
      namespace_name = get_attribute(node, "name");
    if (namespace_name)
      {
        scope.push_back(namespace_name.value());
        for (auto child : get_children(node))
          process(child);
        scope.pop_back();
      }
    else
      result[scope].push_back(node);
  };

  for (auto child : get_children(node))
    process(child);
  return result;
}

/// Sort namespaces, types and declarations.
///
/// This loses annotations (XML comments) on namespace-decl elements.
/// It would have been a fair amount of extra work to preserve them.
///
/// @param node the XML node to process
static void
sort_namespaces_types_and_declarations(xmlNodePtr node)
{
  if (node->type != XML_ELEMENT_NODE)
    return;
  const char* node_name = from_libxml(node->name);
  if (strcmp(node_name, "abi-corpus-group") == 0)
    {
      // Process each corpus in a corpus group separately.
      for (auto child : get_children(node))
        sort_namespaces_types_and_declarations(child);
      return;
    }
  if (strcmp(node_name, "abi-corpus") != 0)
    // Standalone abi-instr is another possibility.
    return;

  // We have a corpus to sort, get its instrs.
  std::vector<xmlNodePtr> instrs;
  for (auto child : get_children(node))
    if (strcmp(from_libxml(child->name), "abi-instr") == 0)
      instrs.push_back(child);
  if (instrs.empty())
    return;

  // Collect the attributes of all the instrs.
  std::map<std::string, std::set<std::string>> attributes;
  // Collect the child elements of all the instrs, *ordered* by namespace scope
  // with types before declarations, types by id, declarations by name and
  // duplicates preserving original order.
  std::map<namespace_scope,
           std::array<std::map<std::string,
                               std::vector<xmlNodePtr>>, 2>> children;
  for (auto instr : instrs)
    {
      for (auto p = instr->properties; p; p = p->next)
        {
          // This is horrible. There should be a better way.
          const char* attribute_name = from_libxml(p->name);
          const auto attribute_value = get_attribute(instr, attribute_name);
          assert(attribute_value);
          attributes[attribute_name].insert(attribute_value.value());
        }
      for (const auto& [scope, elements] : get_children_by_namespace(instr))
        {
          for (auto child : elements)
            {
              const auto id = get_attribute(child, "id");
              if (id)
                {
                  children[scope][0][id.value()].push_back(child);
                  continue;
                }
              const auto name = get_attribute(child, "name");
              if (name)
                {
                  children[scope][1][name.value()].push_back(child);
                  continue;
                }
              std::cerr << "child element is not a type or a declaration\n";
              return;
            }
        }
    }

  // Create and attach a replacement instr and populate its attributes.
  xmlNodePtr replacement =
      xmlAddChild(node, xmlNewNode(nullptr, to_libxml("abi-instr")));
  for (const auto& attribute : attributes)
    {
      const char* attribute_name = attribute.first.c_str();
      const auto& attribute_values = attribute.second;
      if (attribute_values.size() == 1)
        set_attribute(replacement, attribute_name, *attribute_values.begin());
      else if (INSTR_VARIABLE_ATTRIBUTES.count(attribute_name))
        set_attribute(replacement, attribute_name, "various");
      else
        {
          std::cerr << "unexpectedly variable abi-instr attribute '"
                    << attribute_name << "'\n";
          remove_node(replacement);
          return;
        }
    }

  // Move the children to the replacement instr, creating namespace
  // elements as needed.
  std::map<namespace_scope, xmlNodePtr> namespace_elements;
  // Start by recording the global scope's element.
  namespace_elements.insert({{}, replacement});
  std::function<xmlNodePtr(const namespace_scope&)> get_namespace_element =
      [&](const namespace_scope& scope) {
        auto insertion = namespace_elements.insert({scope, nullptr});
    if (insertion.second)
      {
        // Lookup failed so scope cannot be empty.
        namespace_scope truncated = scope;
        truncated.pop_back();
        xmlNodePtr parent = get_namespace_element(truncated);
        // We can now create an XML element in the right place.
        xmlNodePtr child = xmlNewNode(nullptr, to_libxml("namespace-decl"));
        set_attribute(child, "name", scope.back());
        xmlAddChild(parent, child);
        insertion.first->second = child;
      }
    return insertion.first->second;
  };
  for (const auto& [scope, types_and_declarations] : children)
    {
      xmlNodePtr namespace_element = get_namespace_element(scope);
      for (const auto& types_or_declarations : types_and_declarations)
        for (const auto& [_, elements] : types_or_declarations)
          for (auto element : elements)
            move_element(element, namespace_element);
    }

  // Check the original instrs are now all empty and remove them.
  for (auto instr : instrs)
    if (get_children_by_namespace(instr).empty())
      remove_node(instr);
    else
      std::cerr << "original abi-instr has residual child elements\n";
}

/// Main program.
///
/// Read and write ABI XML, with optional extra processing passes.
///
/// @param argc argument count
///
/// @param argv atgument vector
///
/// @return exit status
int
main(int argc, char* argv[])
{
  // Defaults.
  const char* opt_input = nullptr;
  const char* opt_output = nullptr;
  int opt_indentation = 2;
  bool opt_normalise_anonymous = false;
  bool opt_prune_unreachable = false;
  bool opt_report_untyped = false;
  bool opt_eliminate_duplicates = false;
  bool opt_report_conflicts = false;
  bool opt_sort = false;
  bool opt_drop_empty = false;

  // Process command line.
  auto usage = [&]() -> int {
    std::cerr << "usage: " << argv[0]
              << " [-i|--input file]"
              << " [-o|--output file]"
              << " [-I|--indentation n]"
              << " [-a|--all]"
              << " [-n|--[no-]normalise-anonymous]"
              << " [-p|--[no-]prune-unreachable]"
              << " [-u|--[no-]report-untyped]"
              << " [-e|--[no-]eliminate-duplicates]"
              << " [-c|--[no-]report-conflicts]"
              << " [-s|--[no-]sort]"
              << " [-d|--[no-]drop-empty]"
              << '\n';
    return 1;
  };
  int opt_index = 1;
  auto get_arg = [&]() {
    if (opt_index < argc)
      return argv[opt_index++];
    exit(usage());
  };
  while (opt_index < argc)
    {
      const std::string arg = get_arg();
      if (arg == "-i" || arg == "--input")
        opt_input = get_arg();
      else if (arg == "-o" || arg == "--output")
        opt_output = get_arg();
      else if (arg == "-I" || arg == "--indentation")
        {
          std::istringstream is(get_arg());
          is >> std::noskipws >> opt_indentation;
          if (!is || !is.eof() || opt_indentation < 0)
            exit(usage());
        }
      else if (arg == "-a" || arg == "--all")
        opt_normalise_anonymous = opt_prune_unreachable
                                = opt_report_untyped
                                = opt_eliminate_duplicates
                                = opt_report_conflicts
                                = opt_sort
                                = opt_drop_empty
                                = true;
      else if (arg == "-n" || arg == "--normalise-anonymous")
        opt_normalise_anonymous = true;
      else if (arg == "--no-normalise-anonymous")
        opt_normalise_anonymous = false;
      else if (arg == "-p" || arg == "--prune-unreachable")
        opt_prune_unreachable = true;
      else if (arg == "--no-prune-unreachable")
        opt_prune_unreachable = false;
      else if (arg == "-u" || arg == "--report-untyped")
        opt_report_untyped = true;
      else if (arg == "--no-report-untyped")
        opt_report_untyped = false;
      else if (arg == "-e" || arg == "--eliminate-duplicates")
        opt_eliminate_duplicates = true;
      else if (arg == "--no-eliminate-duplicates")
        opt_eliminate_duplicates = false;
      else if (arg == "-c" || arg == "--report-conflicts")
        opt_report_conflicts = true;
      else if (arg == "--no-report-conflicts")
        opt_report_conflicts = false;
      else if (arg == "-s" || arg == "--sort")
        opt_sort = true;
      else if (arg == "--no-sort")
        opt_sort = false;
      else if (arg == "-d" || arg == "--drop-empty")
        opt_drop_empty = true;
      else if (arg == "--no-drop-empty")
        opt_drop_empty = false;
      else
        exit(usage());
    }

  // Open input for reading.
  int in_fd = STDIN_FILENO;
  if (opt_input)
    {
      in_fd = open(opt_input, O_RDONLY);
      if (in_fd < 0)
        {
          std::cerr << "could not open '" << opt_input << "' for reading: "
                    << strerror(errno) << '\n';
          exit(1);
        }
    }

  // Read the XML.
  xmlParserCtxtPtr parser_context = xmlNewParserCtxt();
  xmlDocPtr document
      = xmlCtxtReadFd(parser_context, in_fd, nullptr, nullptr, 0);
  if (!document)
    {
      std::cerr << "failed to parse input as XML\n";
      exit(1);
    }
  xmlFreeParserCtxt(parser_context);
  close(in_fd);

  // Strip text nodes to simplify other operations.
  for (xmlNodePtr node = document->children; node; node = node->next)
    strip_text(node);

  // Normalise anonymous type names.
  if (opt_normalise_anonymous)
    for (xmlNodePtr node = document->children; node; node = node->next)
      normalise_anonymous_type_names(node);

  // Prune unreachable elements and/or report untyped symbols.
  size_t untyped_symbols = 0;
  if (opt_prune_unreachable || opt_report_untyped)
    untyped_symbols += handle_unreachable(
        opt_prune_unreachable, opt_report_untyped, document);

  // Eliminate complete duplicates and extra fragments of types.
  // Report conflicting type defintions.
  // Record whether there are namespace scope conflicts.
  size_t scope_conflicts = 0;
  if (opt_eliminate_duplicates || opt_report_conflicts || opt_sort)
    for (xmlNodePtr node = document->children; node; node = node->next)
      scope_conflicts += handle_duplicate_types(
          opt_eliminate_duplicates, opt_report_conflicts, node);

  // Sort namespaces, types and declarations.
  if (opt_sort)
    {
      if (scope_conflicts)
        std::cerr << "found type definition scope conflicts, skipping sort\n";
      else
        for (xmlNodePtr node = document->children; node; node = node->next)
          sort_namespaces_types_and_declarations(node);
    }

  // Drop empty elements.
  if (opt_drop_empty)
    for (xmlNodePtr node = document->children; node; node = node->next)
      drop_empty(node);

  // Reformat XML for human consumption.
  for (xmlNodePtr node = document->children; node; node = node->next)
    format_xml(std::string(opt_indentation, ' '), std::string(), node);

  // Open output for writing.
  int out_fd = STDOUT_FILENO;
  if (opt_output)
    {
      out_fd = open(opt_output, O_CREAT | O_TRUNC | O_WRONLY,
                    S_IRUSR | S_IWUSR | S_IRGRP | S_IWGRP | S_IROTH | S_IWOTH);
      if (out_fd < 0)
        {
          std::cerr << "could not open '" << opt_output << "' for writing: "
                    << strerror(errno) << '\n';
          exit(1);
        }
    }

  // Write the XML.
  //
  // First to memory, as we need to do a little post-processing.
  xmlChar* out_data;
  int out_size;
  xmlDocDumpMemory(document, &out_data, &out_size);
  // Remove the XML declaration as this is not currently accepted by abidiff.
  xmlChar* out_limit = out_data + out_size;
  while (out_data < out_limit && *out_data != '\n')
    ++out_data;
  if (out_data < out_limit)
    ++out_data;
  // Adjust quotes to match abidw.
  adjust_quotes(out_data, out_limit);
  // And now to a file.
  size_t count = out_limit - out_data;
  if (write(out_fd, out_data, count) != count)
    {
      std::cerr << "could not write output: " << strerror(errno) << '\n';
      exit(1);
    }
  if (close(out_fd) < 0)
    {
      std::cerr << "could not close output: " << strerror(errno) << '\n';
      exit(1);
    }

  // Free libxml document.
  xmlFreeDoc(document);
  return 0;
}
