// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// -*- Mode: C++ -*-
//
// Copyright (C) 2017-2026 Red Hat, Inc.
//
// Author: Dodji Seketeli

/// @file
///
/// The source code of the Kernel Module Interface Diff tool.

#include "config.h"
#include <sys/types.h>
#include <dirent.h>
#include <argp.h>
#include <string>
#include <vector>
#include <iostream>

#include "abg-config.h"
#include "abg-tools-utils.h"
#include "abg-corpus.h"
#include "abg-dwarf-reader.h"
#include "abg-reader.h"
#include "abg-comparison.h"

using std::string;
using std::vector;
using std::ostream;
using std::cout;
using std::cerr;
using abg_compat::optional;

using namespace abigail::tools_utils;
using namespace abigail::ir;
using namespace abigail;

using abigail::comparison::diff_context_sptr;
using abigail::comparison::diff_context;
using abigail::comparison::translation_unit_diff_sptr;
using abigail::comparison::corpus_diff;
using abigail::comparison::corpus_diff_sptr;
using abigail::comparison::compute_diff;
using abigail::comparison::get_default_harmless_categories_bitmap;
using abigail::comparison::get_default_harmful_categories_bitmap;
using abigail::suppr::suppression_sptr;
using abigail::suppr::suppressions_type;
using abigail::suppr::read_suppressions;
using abigail::tools_utils::guess_file_type;
using abigail::tools_utils::file_type;

/// The options of this program.
struct options
{
  bool			verbose;
  bool			perform_change_categorization;
  bool			leaf_changes_only;
  bool			show_hexadecimal_values;
  bool			show_offsets_sizes_in_bits;
  bool			show_impacted_interfaces;
  optional<bool>	exported_interfaces_only;
#ifdef WITH_CTF
  bool			use_ctf;
#endif
#ifdef WITH_BTF
  bool			use_btf;
#endif
  string		kernel_dist_root1;
  string		kernel_dist_root2;
  string		vmlinux1;
  string		vmlinux2;
  vector<string>	kabi_whitelist_paths;
  vector<string>	suppression_paths;
  suppressions_type	read_time_supprs;
  suppressions_type	diff_time_supprs;
  shared_ptr<char>	di_root_path1;
  shared_ptr<char>	di_root_path2;

  options()
    : verbose(),
      perform_change_categorization(true),
      leaf_changes_only(true),
      show_hexadecimal_values(true),
      show_offsets_sizes_in_bits(false),
      show_impacted_interfaces(false)
#ifdef WITH_CTF
      ,
      use_ctf(false)
#endif
#ifdef WITH_BTF
    ,
      use_btf(false)
#endif
  {}
}; // end struct options.

enum option_key
{
  OPT_ALLOW_NON_EXPORTED_INTERFACES = 256,
#ifdef WITH_BTF
  OPT_BTF,
#endif
#ifdef WITH_CTF
  OPT_CTF,
#endif
  OPT_D1,
  OPT_D2,
  OPT_EXPORTED_INTERFACES_ONLY,
  OPT_FULL_IMPACT = 'f',
  OPT_IMPACTED_INTERFACES = 'i',
  OPT_KMI_WHITELIST = 'w',
  OPT_NO_CHANGE_CATEGORIZATION = 'x',
  OPT_SHOW_BITS = OPT_EXPORTED_INTERFACES_ONLY + 1,
  OPT_SHOW_BYTES,
  OPT_SHOW_DEC,
  OPT_SHOW_HEX,
  OPT_SUPPR,
  OPT_VERBOSE,
  OPT_VMLINUX1,
  OPT_VMLINUX2,
};

static const struct argp_option argp_options[] =
{
  { "allow-non-exported-interfaces", OPT_ALLOW_NON_EXPORTED_INTERFACES, 0, 0,
    "analyze interfaces that might not be exported", 0 },
#ifdef WITH_BTF
  { "btf", OPT_BTF, 0, 0,
    "use BTF instead of DWARF in ELF files", 0 },
#endif
#ifdef WITH_CTF
  { "ctf", OPT_CTF, 0, 0,
    "use CTF instead of DWARF in ELF files", 0 },
#endif
  { "debug-info-dir1", OPT_D1, "PATH", 0,
    "the root for the debug info of the first kernel", 0 },
  { "d1", OPT_D1, "PATH", OPTION_ALIAS, 0, 0 },
  { "debug-info-dir2", OPT_D2, "PATH", 0,
    "the root for the debug info of the second kernel", 0 },
  { "d2", OPT_D2, "PATH", OPTION_ALIAS, 0, 0 },
  { "exported-interfaces-only", OPT_EXPORTED_INTERFACES_ONLY, 0, 0,
    "analyze exported interfaces only", 0 },
  { "full-impact", OPT_FULL_IMPACT, 0, 0,
    "show the full impact of changes on top-most interfaces", 0 },
  { "impacted-interfaces", OPT_IMPACTED_INTERFACES, 0, 0,
    "show interfaces impacted by ABI changes", 0 },
  { "linux-kernel-abi-whitelist", OPT_KMI_WHITELIST, "PATH", 0,
    "path to a linux kernel abi whitelist", 0 },
  { "no-change-categorization", OPT_NO_CHANGE_CATEGORIZATION, 0, 0,
    "don't perform categorization of changes, for speed purposes", 0 },
  { "show-bits", OPT_SHOW_BITS, 0, 0,
    "show size and offsets in bits", 0 },
  { "show-bytes", OPT_SHOW_BYTES, 0, 0,
    "show size and offsets in bytes", 0 },
  { "show-dec", OPT_SHOW_DEC, 0, 0,
    "show size and offset in decimal", 0 },
  { "show-hex", OPT_SHOW_HEX, 0, 0,
    "show size and offset in hexadecimal", 0 },
  { "suppressions", OPT_SUPPR, "PATH", 0,
    "specify a suppression file", 0 },
  { "suppr", OPT_SUPPR, "PATH", OPTION_ALIAS, 0, 0 },
  { "verbose", OPT_VERBOSE, 0, 0,
    "display verbose messages", 0 },
  { "vmlinux1", OPT_VMLINUX1, "PATH", 0,
    "the path to the first vmlinux", 0 },
  { "l1", OPT_VMLINUX1, "PATH", OPTION_ALIAS, 0, 0 },
  { "vmlinux2", OPT_VMLINUX2, "PATH", 0,
    "the path to the second vmlinux", 0 },
  { "l2", OPT_VMLINUX2, "PATH", OPTION_ALIAS, 0, 0 },
  { 0, 0, 0, 0, 0, 0 }
};

static error_t
parse_opt(int key, char* arg, struct argp_state* state)
{
  options& opts = *static_cast<options*>(state->input);
  const string argument = arg ? string(arg) : string();

  switch (key)
    {
    case OPT_ALLOW_NON_EXPORTED_INTERFACES:
      opts.exported_interfaces_only = false;
      break;

#ifdef WITH_BTF
    case OPT_BTF:
      opts.use_btf = true;
      break;
#endif

#ifdef WITH_CTF
    case OPT_CTF:
      opts.use_ctf = true;
      break;
#endif

    case OPT_D1:
      opts.di_root_path1 =
	abigail::tools_utils::make_path_absolute(argument.c_str());
      break;

    case OPT_D2:
      opts.di_root_path2 =
	abigail::tools_utils::make_path_absolute(argument.c_str());
      break;

    case OPT_EXPORTED_INTERFACES_ONLY:
      opts.exported_interfaces_only = true;
      break;

    case OPT_FULL_IMPACT:
      opts.leaf_changes_only = false;
      break;

    case OPT_IMPACTED_INTERFACES:
      opts.show_impacted_interfaces = true;
      break;

    case OPT_KMI_WHITELIST:
      opts.kabi_whitelist_paths.push_back(argument);
      break;

    case OPT_NO_CHANGE_CATEGORIZATION:
      opts.perform_change_categorization = false;
      break;

    case OPT_SHOW_BITS:
      opts.show_offsets_sizes_in_bits = true;
      break;

    case OPT_SHOW_BYTES:
      opts.show_offsets_sizes_in_bits = false;
      break;

    case OPT_SHOW_DEC:
      opts.show_hexadecimal_values = false;
      break;

    case OPT_SHOW_HEX:
      opts.show_hexadecimal_values = true;
      break;

    case OPT_SUPPR:
      opts.suppression_paths.push_back(argument);
      break;

    case OPT_VERBOSE:
      opts.verbose = true;
      break;

    case OPT_VMLINUX1:
      opts.vmlinux1 = argument;
      break;

    case OPT_VMLINUX2:
      opts.vmlinux2 = argument;
      break;

    case ARGP_KEY_ARG:
      if (opts.kernel_dist_root1.empty())
	opts.kernel_dist_root1 = argument;
      else if (opts.kernel_dist_root2.empty())
	opts.kernel_dist_root2 = argument;
      else
	argp_usage(state);
      break;

    case ARGP_KEY_END:
      if (opts.kernel_dist_root1.empty() || opts.kernel_dist_root2.empty())
	{
	  std::cerr << state->argv[0]
		    << ": "
		    << "Two kernel trees are required\n";
	  argp_usage(state);
	}
      break;

    default:
      return ARGP_ERR_UNKNOWN;
    }

  return 0;
}

static const char* argp_args_doc =
  "<kernel-tree-dir1> <kernel-tree-dir2>";
static const char* argp_doc =
  "Compare the ABI of kernel module interfaces of two Linux kernel trees.";

static const struct argp kmidiff_argp =
{
  argp_options,
  parse_opt,
  argp_args_doc,
  argp_doc,
  0,
  0,
  0
};

static void
print_kmidiff_version(FILE* stream, struct argp_state* /*state*/)
{
  fprintf(stream, "kmidiff %s\n",
	  abigail::tools_utils::get_library_version_string().c_str());
}

/// Parse the command line of the program.
///
/// @param argc the number of arguments on the command line, including
/// the program name.
///
/// @param argv the arguments on the command line, including the
/// program name.
///
/// @param opts the options resulting from the command line parsing.
///
/// @return true iff the command line parsing went fine.
bool
parse_command_line(int argc, char* argv[], options& opts)
{
  argp_program_version_hook = print_kmidiff_version;
  argp_program_bug_address = "<libabigail@sourceware.org>";

  if (argp_parse(&kmidiff_argp, argc, argv, 0, 0, &opts) != 0)
    return false;
  return true;
}

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

  for (vector<string>::const_iterator i =
	 opts.kabi_whitelist_paths.begin();
       i != opts.kabi_whitelist_paths.end();
       ++i)
    if (!check_file(*i, cerr, "abidiff"))
      return false;

  return true;
}

/// Setup the diff context from the program's options.
///
/// @param ctxt the diff context to consider.
///
/// @param opts the options to set the context.
static void
set_diff_context(diff_context_sptr ctxt, const options& opts)
{
  ctxt->default_output_stream(&cout);
  ctxt->error_output_stream(&cerr);
  ctxt->show_relative_offset_changes(true);
  ctxt->show_redundant_changes(false);
  ctxt->show_locs(true);
  ctxt->show_linkage_names(false);
  ctxt->show_symbols_unreferenced_by_debug_info
    (true);
  ctxt->perform_change_categorization(opts.perform_change_categorization);
  ctxt->show_leaf_changes_only(opts.leaf_changes_only);
  ctxt->show_impacted_interfaces(opts.show_impacted_interfaces);
  ctxt->show_hex_values(opts.show_hexadecimal_values);
  ctxt->show_offsets_sizes_in_bits(opts.show_offsets_sizes_in_bits);

  ctxt->switch_categories_off(get_default_harmless_categories_bitmap());

  if (!opts.diff_time_supprs.empty())
    ctxt->add_suppressions(opts.diff_time_supprs);
}

/// Set the options of the @abigail::fe_iface reader from the command
/// line provided options.
///
/// @param opts the ABI reader front-end interface options to set.
///
/// @param opts the command line provided options.
static void
set_fe_iface_options(abigail::fe_iface::options_type& opts,
		     const options o)
{
  opts.load_in_linux_kernel_mode = true;
  opts.load_all_types =
    o.exported_interfaces_only.has_value()
    ? !o.exported_interfaces_only
    : false;

  opts.do_log = o.verbose;
}

/// Print information about the kernel (and modules) binaries found
/// under a given directory.
///
/// Note that this function actually look for the modules iff the
/// --verbose option was provided.
///
/// @param root the directory to consider.
///
/// @param opts the options to use during the search.
static void
print_kernel_dist_binary_paths_under(const string& root, const options &opts)
{
  string vmlinux;
  vector<string> modules;

  if (opts.verbose)
    if (get_binary_paths_from_kernel_dist(root, /*debug_info_root_path*/"",
					  vmlinux, modules))
       {
	 cout << "Found kernel binaries under: '" << root << "'\n";
	 if (!vmlinux.empty())
	   cout << "[linux kernel binary]\n"
		<< "        '" << vmlinux << "'\n";
	 if (!modules.empty())
	   {
	     cout << "[linux kernel module binaries]\n";
	     for (vector<string>::const_iterator p = modules.begin();
		  p != modules.end();
		  ++p)
	       cout << "        '" << *p << "' \n";
	   }
	 cout << "\n";
       }
}

int
main(int argc, char* argv[])
{
  options opts;
  if (!parse_command_line(argc, argv, opts))
    return 1;

  if (!maybe_check_suppression_files(opts))
    return 1;

  environment env;

  if (opts.exported_interfaces_only.has_value())
    env.analyze_exported_interfaces_only(*opts.exported_interfaces_only);

  corpus_group_sptr group1, group2;
  string debug_info_root_dir;
  corpus::origin requested_fe_kind = corpus::DWARF_ORIGIN;
  abigail::fe_iface::options_type abi_reader_options(env);
  set_fe_iface_options(abi_reader_options, opts);

#ifdef WITH_CTF
  if (opts.use_ctf)
    requested_fe_kind = corpus::CTF_ORIGIN;
#endif
#ifdef WITH_BTF
  if (opts.use_btf)
    requested_fe_kind = corpus::BTF_ORIGIN;
#endif

  if (!opts.kernel_dist_root1.empty())
    {
      file_type ftype = guess_file_type(opts.kernel_dist_root1);
      if (ftype == FILE_TYPE_DIR)
	{
	  debug_info_root_dir = opts.di_root_path1.get()
	    ? opts.di_root_path1.get()
	    : "";

	  group1 =
	    build_corpus_group_from_kernel_dist_under(opts.kernel_dist_root1,
						      debug_info_root_dir,
						      opts.vmlinux1,
						      opts.suppression_paths,
						      opts.kabi_whitelist_paths,
						      opts.read_time_supprs,
						      opts.verbose, env,
						      abi_reader_options,
						      requested_fe_kind);
	  print_kernel_dist_binary_paths_under(opts.kernel_dist_root1, opts);
	}
      else if (ftype == FILE_TYPE_XML_CORPUS_GROUP)
	group1 =
	  abixml::read_corpus_group_from_abixml_file(opts.kernel_dist_root1,
						     env, abi_reader_options);

    }

  if (!opts.kernel_dist_root2.empty())
    {
      file_type ftype = guess_file_type(opts.kernel_dist_root2);
      if (ftype == FILE_TYPE_DIR)
	{
	  debug_info_root_dir = opts.di_root_path2.get()
	    ? opts.di_root_path2.get()
	    : "";
	  group2 =
	    build_corpus_group_from_kernel_dist_under(opts.kernel_dist_root2,
						      debug_info_root_dir,
						      opts.vmlinux2,
						      opts.suppression_paths,
						      opts.kabi_whitelist_paths,
						      opts.read_time_supprs,
						      opts.verbose, env,
						      abi_reader_options,
						      requested_fe_kind);
	  print_kernel_dist_binary_paths_under(opts.kernel_dist_root2, opts);
	}
      else if (ftype == FILE_TYPE_XML_CORPUS_GROUP)
	group2 =
	  abixml::read_corpus_group_from_abixml_file(opts.kernel_dist_root2,
						     env, abi_reader_options);
    }

  abidiff_status status = abigail::tools_utils::ABIDIFF_OK;
  if (group1 && group2)
    {
      diff_context_sptr diff_ctxt(new diff_context);
      set_diff_context(diff_ctxt, opts);

      corpus_diff_sptr diff= compute_diff(group1, group2, diff_ctxt);

      if (diff->has_net_changes())
	status = abigail::tools_utils::ABIDIFF_ABI_CHANGE;

      if (diff->has_incompatible_changes())
	status |= abigail::tools_utils::ABIDIFF_ABI_INCOMPATIBLE_CHANGE;

      if (diff->has_changes())
	diff->report(cout);
    }
  else
    status = abigail::tools_utils::ABIDIFF_ERROR;

  return status;
}
