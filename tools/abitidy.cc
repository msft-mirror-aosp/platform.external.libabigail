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

#include <cstring>
#include <functional>
#include <ios>
#include <iostream>
#include <map>
#include <optional>
#include <set>
#include <sstream>
#include <string>
#include <vector>

#include <libxml/globals.h>
#include <libxml/parser.h>
#include <libxml/tree.h>

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

/// Prune unreachable elements.
///
/// Reachability is defined to be union of contains, containing and
/// refers-to relationships for types, declarations and symbols. The
/// roots for reachability are the ELF elements in the ABI.
///
/// @param document the XML document to process
static void
prune_unreachable(xmlDocPtr document)
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
    if (!strcmp(from_libxml(node->name), "elf-symbol"))
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

  // Traverse the graph, starting from the ELF symbols.
  for (const auto& symbol : elf_symbols)
    {
      vertex_t symbol_vertex{true, symbol};
      if (vertices.count(symbol_vertex))
        dfs(symbol_vertex);
      else
        std::cerr << "no declaration found for ELF symbol " << symbol << '\n';
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
    if (!strcmp(node_name, "var-decl") || !strcmp(node_name, "function-decl"))
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

  // Traverse the document, removing unseen elements.
  for (xmlNodePtr node = document->children; node; node = node->next)
    remove_unseen(node);
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
  const char* opt_input = NULL;
  const char* opt_output = NULL;
  int opt_indentation = 2;
  bool opt_drop_empty = false;
  bool opt_prune_unreachable = false;

  // Process command line.
  auto usage = [&]() -> int {
    std::cerr << "usage: " << argv[0]
              << " [-i|--input file]"
              << " [-o|--output file]"
              << " [-I|--indentation n]"
              << " [-a|--all]"
              << " [-d|--[no-]drop-empty]"
              << " [-p|--[no-]prune-unreachable]"
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
      const char* arg = get_arg();
      if (!strcmp(arg, "-i") || !strcmp(arg, "--input"))
        opt_input = get_arg();
      else if (!strcmp(arg, "-o") || !strcmp(arg, "--output"))
        opt_output = get_arg();
      else if (!strcmp(arg, "-I") || !strcmp(arg, "--indentation"))
        {
          std::istringstream is(get_arg());
          is >> std::noskipws >> opt_indentation;
          if (!is || !is.eof() || opt_indentation < 0)
            exit(usage());
        }
      else if (!strcmp(arg, "-a") || !strcmp(arg, "--all"))
        opt_drop_empty = opt_prune_unreachable = true;
      else if (!strcmp(arg, "-d") || !strcmp(arg, "--drop-empty"))
        opt_drop_empty = true;
      else if (!strcmp(arg, "--no-drop-empty"))
        opt_drop_empty = false;
      else if (!strcmp(arg, "-p") || !strcmp(arg, "--prune-unreachable"))
        opt_prune_unreachable = true;
      else if (!strcmp(arg, "--no-prune-unreachable"))
        opt_prune_unreachable = false;
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
  xmlDocPtr document = xmlCtxtReadFd(parser_context, in_fd, NULL, NULL, 0);
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

  // Prune unreachable elements.
  if (opt_prune_unreachable)
    prune_unreachable(document);

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
