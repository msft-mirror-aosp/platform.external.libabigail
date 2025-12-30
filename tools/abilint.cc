// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// -*- Mode: C++ -*-
//
// Copyright (C) 2013-2026 Red Hat, Inc.
//
// Author: Dodji Seketeli

/// @file
///
/// This is a program aimed at checking that a binary instrumentation
/// (bi) file is well formed and valid enough.  It acts by loading an
/// input bi file and saving it back to a temporary file.  It then
/// runs a diff on the two files and expects the result of the diff to
/// be empty.

#include "config.h"
#include <argp.h>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <fstream>
#include <iostream>
#include <memory>
#include <string>
#include <vector>
#include "abg-config.h"
#include "abg-tools-utils.h"
#include "abg-ir.h"
#include "abg-corpus.h"
#include "abg-reader.h"
#include "abg-comparison.h"
#include "abg-dwarf-reader.h"
#ifdef WITH_CTF
#include "abg-ctf-reader.h"
#endif
#include "abg-writer.h"
#include "abg-suppression.h"

using std::string;
using std::cerr;
using std::cin;
using std::cout;
using std::ostream;
using std::ofstream;
using std::vector;
using std::unordered_set;
using std::unique_ptr;
using abigail::ir::environment;
using abigail::comparison::corpus_diff;
using abigail::comparison::corpus_diff_sptr;
using abigail::comparison::diff;
using abigail::comparison::diff_sptr;
using abigail::comparison::diff_context;
using abigail::comparison::diff_context_sptr;
using abigail::tools_utils::emit_prefix;
using abigail::tools_utils::check_file;
using abigail::tools_utils::file_type;
using abigail::tools_utils::guess_file_type;
using abigail::tools_utils::temp_file;
using abigail::tools_utils::temp_file_sptr;
using abigail::suppr::suppression_sptr;
using abigail::suppr::suppressions_type;
using abigail::suppr::read_suppressions;
using abigail::type_base;
using abigail::type_or_decl_base;
using abigail::type_base_sptr;
using abigail::type_or_decl_base_sptr;
using abigail::corpus;
using abigail::corpus_sptr;
using abigail::abixml::read_translation_unit_from_file;
using abigail::abixml::read_translation_unit_from_istream;
using abigail::abixml::read_corpus_from_abixml;
using abigail::abixml::read_corpus_from_abixml_file;
using abigail::abixml::read_corpus_group_from_input;
using abigail::abixml::create_reader;
#ifdef WITH_SHOW_TYPE_USE_IN_ABILINT
using abigail::abixml::get_types_from_type_id;
using abigail::abixml::get_artifact_used_by_relation_map;
#endif

using abigail::xml_writer::write_translation_unit;
using abigail::xml_writer::write_context_sptr;
using abigail::xml_writer::create_write_context;
using abigail::xml_writer::write_corpus;
using abigail::xml_writer::write_corpus_to_archive;

using namespace abigail;

struct options
{
  string			file_path;
  bool				read_from_stdin;
  bool				read_tu;
  bool				diff;
  bool				abidiff;
  bool				noout;
  bool				annotate;
  bool				load_all_types;
  bool				do_log;
#ifdef WITH_CTF
  bool				use_ctf;
#endif
  string			di_root_path;
  vector<string>		suppression_paths;
  string			headers_dir;
  vector<string>		header_files;
#if WITH_SHOW_TYPE_USE_IN_ABILINT
  string			type_id_to_show;
#endif

  options()
    : read_from_stdin(false),
      read_tu(false),
      diff(false),
      abidiff(false),
      noout(false),
      annotate(false),
      load_all_types(false),
      do_log(false)
#ifdef WITH_CTF
    ,
      use_ctf(false)
#endif
  {}
};//end struct options;

#ifdef WITH_SHOW_TYPE_USE_IN_ABILINT
/// A tree node representing the "use" relation between an artifact A
/// (e.g, a type) and a set of artifacts {A'} that use "A" as in "A"
/// is a sub-type of A'.
///
/// So the node contains the artifact A and a vector children nodes
/// that contain the A' artifacts that use A.
struct artifact_use_relation_tree
{
  artifact_use_relation_tree *root_node = nullptr;
  /// The parent node of this one.  Is nullptr if this node is the root
  /// node.
  artifact_use_relation_tree *parent = nullptr;
  /// The artifact contained in this node.
  type_or_decl_base* artifact = nullptr;
  /// The vector of children nodes that carry the artifacts that
  /// actually use the 'artifact' above.  In other words, the
  /// 'artifact" data member above is a sub-type of each artifact
  /// contained in this vector.
  vector<unique_ptr<artifact_use_relation_tree>> artifact_users;
  /// This is the set of artifacts that have been added to the tree.
  /// This is useful to ensure that all artifacts are added just once
  /// in the tree to prevent infinite loops.
  unordered_set<type_or_decl_base *> artifacts;

  /// The constructor of the tree node.
  ///
  /// @param the artifact to consider.
  artifact_use_relation_tree(type_or_decl_base* t)
    : artifact (t)
  {
    ABG_ASSERT(t && !artifact_in_tree(t));
    record_artifact(t);
  }

  /// Add a user artifact node for the artifact carried by this node.
  ///
  /// The artifact carried by the current node is a sub-type of the
  /// artifact carried by the 'user' node being added.
  ///
  /// @param user a tree node that carries an artifact that uses the
  /// artifact carried by the current node.
  void
  add_artifact_user(artifact_use_relation_tree *user)
  {
    ABG_ASSERT(user && !artifact_in_tree(user->artifact ));
    artifact_users.push_back(unique_ptr<artifact_use_relation_tree>(user));
    user->parent = this;
    record_artifact(user->artifact);
  }

  /// Move constructor.
  ///
  /// @param o the source of the move.
  artifact_use_relation_tree(artifact_use_relation_tree &&o)
  {
    parent = o.parent;
    artifact = o.artifact;
    artifact_users = std::move(o.artifact_users);
    artifacts = std::move(o.artifacts);
  }

  /// Move assignment operator.
  ///
  /// @param o the source of the assignment.
  artifact_use_relation_tree& operator=(artifact_use_relation_tree&& o)
  {
    parent = o.parent;
    artifact = o.artifact;
    artifact_users = std::move(o.artifact_users);
    artifacts = std::move(o.artifacts);
    return *this;
  }

  /// Test if the current node is a leaf node.
  ///
  /// @return true if the artifact carried by the current node has no
  /// user artifacts.
  bool
  is_leaf() const
  {return artifact_users.empty();}

  /// Test if the current node is a root node.
  ///
  /// @return true if the current artifact uses no other artifact.
  bool
  is_root() const
  {return parent == nullptr;}

  /// Test wether a given artifact has been added to the tree.
  ///
  /// Here, the tree means the tree that the current tree node is part
  /// of.
  ///
  /// An artifact is considered as having been added to the tree if
  /// artifact_use_relation_tree::record_artifact has been invoked on
  /// it.
  ///
  /// @param artifact the artifact to consider.
  ///
  /// @return true iff @p artifact is present in the tree.
  bool
  artifact_in_tree(type_or_decl_base *artifact)
  {
    artifact_use_relation_tree *root_node = get_root_node();
    ABG_ASSERT(root_node);
    return root_node->artifacts.find(artifact) != root_node->artifacts.end();
  }

  /// Record an artifact as being added to the current tree.
  ///
  /// Note that this function assumes the artifact is not already
  /// present in the tree containing the current tree node.
  ///
  /// @param artifact the artifact to consider.
  void
  record_artifact(type_or_decl_base *artifact)
  {
    ABG_ASSERT(!artifact_in_tree(artifact));
    artifact_use_relation_tree *root_node = get_root_node();
    ABG_ASSERT(root_node);
    root_node->artifacts.insert(artifact);
  }

  /// Get the root node of the current tree.
  ///
  /// @return the root node of the current tree.
  artifact_use_relation_tree*
  get_root_node()
  {
    if (root_node)
      return root_node;

    if (parent == nullptr)
      return this;

    root_node = parent->get_root_node();
    return root_node;
  }

  artifact_use_relation_tree(const artifact_use_relation_tree&) = delete;
  artifact_use_relation_tree& operator=(const artifact_use_relation_tree&) = delete;
}; // end struct artifact_use_relation_tree

/// Fill an "artifact use" tree from a map that associates a type T
/// (or artifact) to artifacts that use T as a sub-type.
///
/// @param artifact_use_rel the map that establishes the relation
/// between a type T and the artifacts that use T as a sub-type.
///
/// @parm tree output parameter.  This function will fill up this tree
/// from the information carried in @p artifact_use_rel.  Each node of
/// the tree contains an artifact A and its children nodes contain the
/// artifacts A' that use A as a sub-type.
static void
fill_artifact_use_tree(const std::unordered_map<type_or_decl_base*,
						vector<type_or_decl_base*>>& artifact_use_rel,
		       artifact_use_relation_tree& tree)
{
  auto r = artifact_use_rel.find(tree.artifact);
  if (r == artifact_use_rel.end())
    return;

  // Walk the users of "artifact", create a tree node for each one of
  // them, and add them as children node of the current tree node
  // named 'tree'.
  for (auto user : r->second)
    {
      if (tree.artifact_in_tree(user))
	// The artifact has already been added to the tree, so skip it
	// otherwise we can loop for ever.
	continue;

      artifact_use_relation_tree *user_tree =
	new artifact_use_relation_tree(user);

      // Now add the new user node as a child of the current tree
      // node.
      tree.add_artifact_user(user_tree);

      // Recursively fill the newly created tree node.
      fill_artifact_use_tree(artifact_use_rel, *user_tree);
    }
}

/// construct an "artifact use tree" for a type designated by a "type-id".
/// (or artifact) to artifacts that use T as a sub-type.
///
/// Each node of the "artifact use tree" contains a type T and its
/// children nodes contain the artifacts A' that use T as a sub-type.
/// The root node is the type designed by a given type-id.
///
/// @param ctxt the abixml read context to consider.
///
/// @param type_id the type-id of the type to construct the "use tree"
/// for.
static unique_ptr<artifact_use_relation_tree>
build_type_use_tree(abigail::fe_iface &iface,
		    const string& type_id)
{
  unique_ptr<artifact_use_relation_tree> result;
  vector<type_base_sptr>* types = get_types_from_type_id(iface, type_id);
  if (!types)
    return result;

  std::unordered_map<type_or_decl_base*, vector<type_or_decl_base*>>*
    artifact_use_rel = get_artifact_used_by_relation_map(iface);
  if (!artifact_use_rel)
    return result;

  type_or_decl_base_sptr type = types->front();
  unique_ptr<artifact_use_relation_tree> use_tree
    (new artifact_use_relation_tree(type.get()));

  fill_artifact_use_tree(*artifact_use_rel, *use_tree);

  result = std::move(use_tree);
  return result;
}

/// Emit a visual representation of a "type use trace".
///
/// The trace is vector of strings.  Each string is the textual
/// representation of a type.  The next element in the vector is a
/// type using the previous element, as in, the "previous element is a
/// sub-type of the next element".
///
/// This is a sub-routine of emit_artifact_use_trace.
///
/// @param the trace vector to emit.
///
/// @param out the output stream to emit the trace to.
static void
emit_trace(const vector<string>& trace, ostream& out)
{
  if (trace.empty())
    return;

  if (!trace.empty())
    // Make the beginning of the trace line of the usage of a given
    // type be easily recognizeable by a "pattern".
    out << "===";

  for (auto element : trace)
    out << "-> " << element << " ";

  if (!trace.empty())
    // Make the end of the trace line of the usage of a given type be
    // easily recognizeable by another "pattern".
    out << " <-~~~";

  out << "\n";
}

/// Walk a @ref artifact_use_relation_tree to emit a "type-is-used-by"
/// trace.
///
/// The tree carries the information about how a given type is used by
/// other types.  This function walks the tree by visiting a node
/// carrying a given type T, and then the nodes for which T is a
/// sub-type.  The function accumulates a trace made of the textual
/// representation of the visited nodes and then emits that trace on
/// an output stream.
///
/// @param artifact_use_tree the tree to walk.
///
/// @param trace the accumulated vector of the textual representations
/// of the types carried by the visited nodes.
///
/// @param out the output stream to emit the trace to.
static void
emit_artifact_use_trace(const artifact_use_relation_tree& artifact_use_tree,
			vector<string>& trace, ostream& out)
{
  type_or_decl_base* artifact = artifact_use_tree.artifact;
  if (!artifact)
    return;

  string repr = artifact->get_pretty_representation();
  trace.push_back(repr);

  if (artifact_use_tree.artifact_users.empty())
    {
      // We reached a leaf node.  This means that no other artifact
      // uses the artifact carried by this leaf node.  So, we want to
      // emit the trace accumulated to this point.

      // But we only want to emit the usage traces that end up with a
      // function of variable that have an associated ELF symbol.
      bool do_emit_trace = false;
      if (is_decl(artifact))
	{
	  if (abigail::ir::var_decl* v = is_var_decl(artifact))
	    if (v->get_symbol()
		|| is_at_global_scope(v)
		|| !v->get_linkage_name().empty())
	      do_emit_trace = true;
	  if (abigail::ir::function_decl* f = is_function_decl(artifact))
	    if (f->get_symbol()
		|| is_at_global_scope(f)
		|| !f->get_linkage_name().empty())
	      do_emit_trace = true;
	}

      // OK now, really emit the trace.
      if (do_emit_trace)
	emit_trace(trace, out);

      trace.pop_back();
      return;
    }

  for (const auto &user : artifact_use_tree.artifact_users)
    emit_artifact_use_trace(*user, trace, out);

  trace.pop_back();
}

/// Walk a @ref artifact_use_relation_tree to emit a "type-is-used-by"
/// trace.
///
/// The tree carries the information about how a given type is used by
/// other types.  This function walks the tree by visiting a node
/// carrying a given type T, and then the nodes for which T is a
/// sub-type.  The function then emits a trace of how the root type is
/// used.
///
/// @param artifact_use_tree the tree to walk.
///
/// @param out the output stream to emit the trace to.
static void
emit_artifact_use_trace(const artifact_use_relation_tree& artifact_use_tree,
			ostream& out)
{
  vector<string> trace;
  emit_artifact_use_trace(artifact_use_tree, trace, out);
}

/// Show how a type is used.
///
/// The type to consider is designated by a type-id string that is
/// carried by the options data structure.
///
/// @param iface the abixml reader interface to consider.
///
/// @param the type_id of the type which usage to analyse.
static bool
show_how_type_is_used(abigail::fe_iface &iface, const string& type_id)
{
  if (type_id.empty())
    return false;

  unique_ptr<artifact_use_relation_tree> use_tree =
    build_type_use_tree(iface, type_id);
  if (!use_tree)
    return false;

  // Now walk the use_tree to emit the type use trace
  if (use_tree->artifact)
    {
      std::cout << "Type ID '"
		<< type_id << "' is for type '"
		<< use_tree->artifact->get_pretty_representation()
		<< "'\n"
		<< "The usage graph for that type is:\n";
      emit_artifact_use_trace(*use_tree, std::cout);
    }
  return true;
}
#endif // WITH_SHOW_TYPE_USE_IN_ABILINT

/// Check that the suppression specification files supplied are
/// present.  If not, emit an error on stderr.
///
/// @param opts the options instance to use.
///
/// @return true if all suppression specification files are present,
/// false otherwise.
static bool
maybe_check_suppression_files(const options& opts)
{
  for (vector<string>::const_iterator i = opts.suppression_paths.begin();
       i != opts.suppression_paths.end();
       ++i)
    if (!check_file(*i, cerr, "abidiff"))
      return false;

  return true;
}

/// Set suppression specifications to the @p reader used to load
/// the ABI corpus from the ELF/DWARF file.
///
/// These suppression specifications are going to be applied to drop
/// some ABI artifacts on the floor (while reading the ELF/DWARF file
/// or the native XML ABI file) and thus minimize the size of the
/// resulting ABI corpus.
///
/// @param read_ctxt the read context to apply the suppression
/// specifications to.  Note that the type of this parameter is
/// generic (class template) because in practise, it can be either an
/// abigail::dwarf_reader::reader type or an
/// abigail::abixml::reader type.
///
/// @param opts the options where to get the suppression
/// specifications from.
static void
set_suppressions(abigail::fe_iface& reader, const options& opts)
{
  suppressions_type supprs;
  for (vector<string>::const_iterator i = opts.suppression_paths.begin();
       i != opts.suppression_paths.end();
       ++i)
    read_suppressions(*i, supprs);

  suppression_sptr suppr =
    abigail::tools_utils::gen_suppr_spec_from_headers(opts.headers_dir,
						      opts.header_files);
  if (suppr)
    supprs.push_back(suppr);

  reader.add_suppressions(supprs);
}

/// Initialize the context use for driving ABI comparison.
///
/// @param ctxt the context to initialize.
static void
set_diff_context(diff_context_sptr& ctxt)
{
  ctxt->default_output_stream(&cerr);
  ctxt->error_output_stream(&cerr);
  // Filter out changes that are not meaningful from an ABI
  // standpoint, from the diff output.
  ctxt->switch_categories_off
    (abigail::comparison::ACCESS_CHANGE_CATEGORY
     | abigail::comparison::COMPATIBLE_TYPE_CHANGE_CATEGORY
     | abigail::comparison::HARMLESS_DECL_NAME_CHANGE_CATEGORY);
}


/// Set the options of the reader.
///
/// @param reader the reader to consider.
///
/// @param opts the options to use.
static void
set_reader_options(abigail::fe_iface& reader, const options& opts)
{
  set_suppressions(reader, opts);
  reader.options().load_all_types = opts.load_all_types;
  reader.options().do_log = opts.do_log;
}

enum option_key
{
  OPT_ANNOTATE = 256,
#ifdef WITH_CTF
  OPT_CTF,
#endif
  OPT_DEBUG_INFO_DIR,
  OPT_DIFF,
  OPT_ABIDIFF,
  OPT_HD,
  OPT_HF,
  OPT_LOAD_ALL_TYPES,
  OPT_NOOUT,
#ifdef WITH_SHOW_TYPE_USE_IN_ABILINT
  OPT_SHOW_TYPE_USE,
#endif
  OPT_STDIN,
  OPT_SUPPR,
  OPT_TU,
  OPT_VERBOSE
};

static const struct argp_option argp_options[] =
{
  { "abidiff", OPT_ABIDIFF, 0, 0,
    "perform an ABI diff between the memory model of the input and "
    "the memory model of the file saved to disk and read back "
    "into memory", 0 },
  { "annotate", OPT_ANNOTATE, 0, 0,
    "annotate the ABI artifacts emitted in the output", 0 },
#ifdef WITH_CTF
  { "ctf", OPT_CTF, 0, 0,
    "use CTF instead of DWARF in ELF files", 0 },
#endif
  { "debug-info-dir", OPT_DEBUG_INFO_DIR, "PATH", 0,
    "the path under which to look for debug info for the elf <abi-file>", 0 },
  { "diff", OPT_DIFF, 0, 0,
    "for xml inputs, perform a text diff between "
    "the input and the memory model saved back to disk", 0 },
  { "headers-dir", OPT_HD, "PATH", 0,
    "the path to headers of the elf file", 0 },
  { "hd", OPT_HD, "PATH", OPTION_ALIAS, 0, 0 },
  { "header-file", OPT_HF, "PATH", 0,
    "the path to one header of the elf file", 0 },
  { "load-all-types", OPT_LOAD_ALL_TYPES, 0, 0,
    "read all types including those not reachable from exported declarations", 0 },
  { "hf", OPT_HF, "PATH", OPTION_ALIAS, 0, 0 },
  { "noout", OPT_NOOUT, 0, 0,
    "do not display anything on stdout", 0 },
#ifdef WITH_SHOW_TYPE_USE_IN_ABILINT
  { "show-type-use", OPT_SHOW_TYPE_USE, "TYPE-ID", 0,
    "show how a type is used from the abixml file", 0 },
#endif
  { "stdin", OPT_STDIN, 0, 0,
    "read abi-file content from stdin", 0 },
  { "suppressions", OPT_SUPPR, "PATH", 0,
    "specify a suppression file", 0 },
  { "suppr", OPT_SUPPR, "PATH", OPTION_ALIAS, 0, 0 },
  { "tu", OPT_TU, 0, 0,
    "expect a single translation unit file", 0 },
  { "verbose", OPT_VERBOSE, 0, 0,
    "show verbose messages about internal stuff", 0 },
  { 0, 0, 0, 0, 0, 0 }
};

static error_t
parse_opt(int key, char* arg, struct argp_state* state)
{
  options& opts = *static_cast<options*>(state->input);
  const string argument = arg ? string(arg) : string();

  switch (key)
    {
    case OPT_ANNOTATE:
      opts.annotate = true;
      break;

#ifdef WITH_CTF
    case OPT_CTF:
      opts.use_ctf = true;
      break;
#endif

    case OPT_DEBUG_INFO_DIR:
      opts.di_root_path =
	abigail::tools_utils::make_path_absolute(argument);
      break;

    case OPT_DIFF:
      opts.diff = true;
      break;

    case OPT_ABIDIFF:
      opts.abidiff = true;
      break;

    case OPT_HD:
      opts.headers_dir = argument;
      break;

    case OPT_HF:
      opts.header_files.push_back(argument);
      break;

    case OPT_LOAD_ALL_TYPES:
      opts.load_all_types = true;
      break;

    case OPT_NOOUT:
      opts.noout = true;
      break;

#ifdef WITH_SHOW_TYPE_USE_IN_ABILINT
    case OPT_SHOW_TYPE_USE:
      opts.type_id_to_show = argument;
      break;
#endif

    case OPT_STDIN:
      opts.read_from_stdin = true;
      break;

    case OPT_SUPPR:
      opts.suppression_paths.push_back(argument);
      break;

    case OPT_TU:
      opts.read_tu = true;
      break;

    case OPT_VERBOSE:
      opts.do_log = true;
      break;

    case ARGP_KEY_ARG:
      if (opts.file_path.empty())
	opts.file_path = argument;
      else
	argp_usage(state);
      break;

    case ARGP_KEY_END:
#ifdef WITH_SHOW_TYPE_USE_IN_ABILINT
      if (!opts.type_id_to_show.empty() && opts.file_path.empty())
	emit_prefix("abilint", cout)
	  << "WARNING: --show-type-use <type-id> "
	  "must be accompanied with an abixml file\n";

      if (opts.file_path.empty() && opts.type_id_to_show.empty())
	opts.read_from_stdin = true;
#else
      if (opts.file_path.empty())
	opts.read_from_stdin = true;
#endif
      if (opts.read_from_stdin && !opts.file_path.empty())
	emit_prefix("abilint", cout)
	  << "WARNING: The '--stdin' option is used. The "
	  << opts.file_path << " will be ignored automatically\n";
      break;

    default:
      return ARGP_ERR_UNKNOWN;
    }

  return 0;
}

static const char* argp_args_doc = "[<abi-file>]";
static const char* argp_doc =
  "Read an ABI instrumentation file in native XML format and check its "
  "integrity.";

static const struct argp abilint_argp =
{
  argp_options,
  parse_opt,
  argp_args_doc,
  argp_doc,
  0,
  0,
  0
};

/// Version printing hook to be passed to ARGP.
///
/// @param stream the output stream.
///
/// @param state the ARGP state.
void
print_abilint_version(FILE *stream, struct argp_state* /*state*/)
{
  fprintf(stream, "abilint %s\n",
	  abigail::tools_utils::get_library_version_string().c_str());
}

/// Parse the command line
///
/// @param argc number of args
///
/// @param argv the array of arguments.
///
/// @param opts the options set as result of command line parsing.
bool
parse_command_line(int argc, char* argv[], options& opts)
{
  argp_program_bug_address = "<libabigail@sourceware.org>";
  argp_program_version_hook = print_abilint_version;

  if (argp_parse(&abilint_argp, argc, argv, 0, 0, &opts) != 0)
    return false;
  return true;
}

/// Given a corpus (or a corpus group), write it as ABIXML, read it
/// back into another corpus and compare the resulting two corpora.
///
/// The result of the comparison should be the empty set.
///
/// @param write_ctxt the write context to use for writing the corpus
/// to ABIXML.
///
/// @param corp the input corpus (or corpus group) to serialize to
/// ABIXML.
///
/// @param env the environment used for computing.
/// @param opts the options passed to the main program.
///
/// @param argv the vector of arguments of the main program.
///
/// @return 0 if the self comparison did yield the empty set, 1
/// otherwise.  If the comparison does (wronly) yield a result, that
/// result if emitted on std::cerr.
static int
perform_self_comparison(const corpus_sptr& corp,
			const translation_unit_sptr& tu,
			environment& env, char* argv[])
{
  // Save the abi in abixml format in a temporary file, read
  // it back, and compare the ABI of what we've read back
  // against the ABI of the input ELF file.
  temp_file_sptr tmp_file = temp_file::create();
  const write_context_sptr& write_ctxt = create_write_context(env, tmp_file->get_stream());

  abigail::ir::corpus_group_sptr corp_group = is_corpus_group(corp);

  if (corp_group)
    write_corpus_group(*write_ctxt, corp_group, 0);
  else if (corp)
    write_corpus(*write_ctxt, corp, 0);
  else if (tu)
    write_translation_unit(*write_ctxt, *tu, 0);

  tmp_file->get_stream().flush();

  abigail::fe_iface_sptr rdr = create_reader(tmp_file->get_path(), env);

  abigail::fe_iface::status sts;
  corpus_sptr corp2;
  corpus_group_sptr corp_group2;
  translation_unit_sptr tu2;

  if (corp_group)
    corp_group2 = abixml::read_corpus_group_from_input(*rdr);
  else if (corp)
    corp2 = rdr->read_corpus(sts);
  else if (tu)
    {
      abigail::fe_iface_sptr rdr2 =
	  abigail::abixml::create_reader(tmp_file->get_path(), env);
      tu2 = abigail::abixml::read_translation_unit(*rdr2);
    }

  if (!corp2 && !corp_group2 && !tu2)
    {
      emit_prefix(argv[0], cerr)
	<< "Could not read temporary XML representation of "
	"ABIXML file back\n";
      return 1;
    }

  diff_context_sptr ctxt(new diff_context);
  set_diff_context(ctxt);

  diff_sptr diff;
  corpus_diff_sptr corpus_diff;

  if (corp_group2)
    corpus_diff = compute_diff(corp_group, corp_group2, ctxt);
  else if (corp2)
    corpus_diff = compute_diff(corp, corp2, ctxt);
  else if (tu2)
    diff = compute_diff(tu, tu2, ctxt);

  bool has_error = corpus_diff ? corpus_diff->has_changes() : diff->has_changes();
  if (has_error)
    {
      corpus_diff ? corpus_diff->report(cerr) : diff->report(cerr);
      return 1;
    }
  return 0;
}

static int
load_corpus_and_write_abixml(char* argv[],
			     environment& env,
			     options& opts)
{
  abigail::translation_unit_sptr tu;
  abigail::corpus_sptr corp;
  abigail::corpus_group_sptr group;
  abigail::fe_iface::status s = abigail::fe_iface::STATUS_OK;
  string di_root_path;
  file_type type = guess_file_type(opts.file_path);
  abigail::fe_iface_sptr rdr;

  switch (type)
    {
    case abigail::tools_utils::FILE_TYPE_UNKNOWN:
      emit_prefix(argv[0], cerr)
	<< "Unknown file type given in input: " << opts.file_path
	<< "\n";
      return 1;
    case abigail::tools_utils::FILE_TYPE_NATIVE_BI:
      {
	rdr = abigail::abixml::create_reader(opts.file_path, env);
	set_reader_options(*rdr, opts);
	tu = abigail::abixml::read_translation_unit(*rdr);
      }
      break;
    case abigail::tools_utils::FILE_TYPE_ELF:
    case abigail::tools_utils::FILE_TYPE_AR:
      {
	di_root_path = opts.di_root_path;
	vector<string> di_roots;
	di_roots.push_back(di_root_path);
	abigail::elf_based_reader_sptr rdr;
#ifdef WITH_CTF
	if (opts.use_ctf)
	  rdr =
	    abigail::ctf::create_reader(opts.file_path,
					di_roots, env);
	else
#endif
	  rdr =
	    abigail::dwarf::create_reader(opts.file_path,
					  di_roots, env);
	set_reader_options(*rdr, opts);
	corp = rdr->read_corpus(s);
      }
      break;
    case abigail::tools_utils::FILE_TYPE_XML_CORPUS:
      {
	rdr = abigail::abixml::create_reader(opts.file_path, env);
	assert(rdr);
	set_reader_options(*rdr, opts);
	corp = rdr->read_corpus(s);
	break;
      }
    case abigail::tools_utils::FILE_TYPE_XML_CORPUS_GROUP:
      {
	rdr = abigail::abixml::create_reader(opts.file_path, env);
	assert(rdr);
	set_reader_options(*rdr, opts);
	group = read_corpus_group_from_input(*rdr);
      }
      break;
    case abigail::tools_utils::FILE_TYPE_RPM:
    case abigail::tools_utils::FILE_TYPE_SRPM:
    case abigail::tools_utils::FILE_TYPE_DEB:
    case abigail::tools_utils::FILE_TYPE_DIR:
    case abigail::tools_utils::FILE_TYPE_TAR:
    case abigail::tools_utils::FILE_TYPE_XZ:
      break;
    }

  if (!tu && !corp && !group)
    {
      emit_prefix(argv[0], cerr)
	<< "failed to read " << opts.file_path << "\n";
      if (!(s & abigail::fe_iface::STATUS_OK))
	{
	  if (s & abigail::fe_iface::STATUS_DEBUG_INFO_NOT_FOUND)
	    {
	      cerr << "could not find the debug info";
	      if(di_root_path.empty())
		emit_prefix(argv[0], cerr)
		  << " Maybe you should consider using the "
		  "--debug-info-dir1 option to tell me about the "
		  "root directory of the debuginfo? "
		  "(e.g, --debug-info-dir1 /usr/lib/debug)\n";
	      else
		emit_prefix(argv[0], cerr)
		  << "Maybe the root path to the debug "
		  "information is wrong?\n";
	    }
	  if (s & abigail::fe_iface::STATUS_NO_SYMBOLS_FOUND)
	    emit_prefix(argv[0], cerr)
	      << "could not find the ELF symbols in the file "
	      << opts.file_path
	      << "\n";
	}
      return 1;
    }

  using abigail::tools_utils::temp_file;
  using abigail::tools_utils::temp_file_sptr;

  temp_file_sptr tmp_file = temp_file::create();
  if (!tmp_file)
    {
      emit_prefix(argv[0], cerr) << "failed to create temporary file\n";
      return 1;
    }

  std::ostream& of = opts.diff ? tmp_file->get_stream() : cout;
  const write_context_sptr ctxt = create_write_context(env, of);

  bool is_ok = true;

  if (tu)
    {
      if (!opts.noout)
	{
	  set_annotate(*ctxt, opts.annotate);
	  if (opts.abidiff)
	    return perform_self_comparison(nullptr, tu, env, argv);
	  is_ok = write_translation_unit(*ctxt, *tu, 0);
	}
    }
  else
    {
      if (type == abigail::tools_utils::FILE_TYPE_XML_CORPUS
	  || type == abigail::tools_utils::FILE_TYPE_XML_CORPUS_GROUP
	  || type == abigail::tools_utils::FILE_TYPE_ELF)
	{
	  if (!opts.noout)
	    {
	      if (opts.abidiff)
		return perform_self_comparison(corp, nullptr, env, argv);

	      set_annotate(*ctxt, opts.annotate);
	      if (corp)
		is_ok = write_corpus(*ctxt, corp, 0);
	      else if (group)
		is_ok = write_corpus_group(*ctxt, group, 0);
	    }
	}
    }

  of.flush();

  if (!is_ok)
    {
      string output =
	(type == abigail::tools_utils::FILE_TYPE_NATIVE_BI)
	? "translation unit"
	: "ABI corpus";
      emit_prefix(argv[0], cerr)
	<< "failed to write the translation unit "
	<< opts.file_path << " back\n";
    }

  if (is_ok
      && opts.diff
      && ((type == abigail::tools_utils::FILE_TYPE_XML_CORPUS)
	  ||type == abigail::tools_utils::FILE_TYPE_XML_CORPUS_GROUP
	  || type == abigail::tools_utils::FILE_TYPE_NATIVE_BI))
    {
      string cmd = "diff -u " + opts.file_path + " " + tmp_file->get_path();
      if (system(cmd.c_str()))
	is_ok = false;
    }

#ifdef WITH_SHOW_TYPE_USE_IN_ABILINT
  if (is_ok
      && !opts.type_id_to_show.empty())
    {
      ABG_ASSERT(rdr);
      show_how_type_is_used(*rdr, opts.type_id_to_show);
    }
#endif
  return is_ok ? 0 : 1;
}

/// Reads a bi (binary instrumentation) file, saves it back to a
/// temporary file and run a diff on the two versions.
int
main(int argc, char* argv[])
{
  abigail::tools_utils::initialize();

  options opts;
  if (!parse_command_line(argc, argv, opts))
    {
      char* prog_name = (char*)"abilint";
      argp_help(&abilint_argp, stderr, ARGP_HELP_USAGE, prog_name);
      return 1;
    }

  if (!maybe_check_suppression_files(opts))
    return 1;

  abigail::ir::environment env;
  if (opts.read_from_stdin)
    {
      if (!cin.good())
	return 1;

      if (opts.read_tu)
	{
	  abigail::translation_unit_sptr tu =
	    read_translation_unit_from_istream(&cin, env);

	  if (!tu)
	    {
	      emit_prefix(argv[0], cerr)
		<< "failed to read the ABI instrumentation from stdin\n";
	      return 1;
	    }

	  if (!opts.noout)
	    {
	      const write_context_sptr& ctxt
		  = create_write_context(env, cout);
	      set_annotate(*ctxt, opts.annotate);
	      write_translation_unit(*ctxt, *tu, 0);
	    }
	  return 0;
	}
      else
	{
	  abigail::fe_iface_sptr rdr =
	    abigail::abixml::create_reader(&cin, env);
	  assert(rdr);
	  set_reader_options(*rdr, opts);
	  abigail::fe_iface::status sts;
	  corpus_sptr corp = rdr->read_corpus(sts);
	  if (!opts.noout)
	    {
	      const write_context_sptr& ctxt
		  = create_write_context(env, cout);
	      set_annotate(*ctxt, opts.annotate);
	      write_corpus(*ctxt, corp, /*indent=*/0);
	    }
	  return 0;
	}
    }
  else if (!opts.file_path.empty())
    return load_corpus_and_write_abixml(argv, env, opts);

  return 1;
}
