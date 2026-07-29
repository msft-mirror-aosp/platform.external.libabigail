// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// -*- Mode: C++ -*-
//
// Copyright (C) 2013-2026 Red Hat, Inc.
//
// Author: Dodji Seketeli

/// @file
///
/// This program reads an elf file, try to load its debug info (in
/// DWARF format) and emit it back in a set of "text sections" in native
/// libabigail XML format.

#include "config.h"
#include <argp.h>
#include <unistd.h>
#include <cassert>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <fstream>
#include <iostream>
#include <memory>
#include <string>
#include <vector>
#include <set>
#include "abg-config.h"
#include "abg-tools-utils.h"
#include "abg-corpus.h"
#include "abg-dwarf-reader.h"
#ifdef WITH_CTF
#include "abg-ctf-reader.h"
#endif
#ifdef WITH_BTF
#include "abg-btf-reader.h"
#endif
#include "abg-writer.h"
#include "abg-reader.h"
#include "abg-comparison.h"
#include "abg-suppression.h"

using std::string;
using std::cerr;
using std::cout;
using std::ostream;
using std::ofstream;
using std::vector;
using std::set;
using std::shared_ptr;
using std::static_pointer_cast;
using abg_compat::optional;
using abigail::tools_utils::emit_prefix;
using abigail::tools_utils::temp_file;
using abigail::tools_utils::temp_file_sptr;
using abigail::tools_utils::check_file;
using abigail::tools_utils::build_corpus_group_from_kernel_dist_under;
using abigail::tools_utils::timer;
using abigail::tools_utils::create_best_elf_based_reader;
using abigail::tools_utils::stick_corpus_and_dependencies_into_corpus_group;
using abigail::tools_utils::stick_corpus_and_binaries_into_corpus_group;
using abigail::tools_utils::add_dependencies_into_corpus_group;
using abigail::ir::environment_sptr;
using abigail::ir::environment;
using abigail::corpus;
using abigail::corpus_sptr;
using abigail::translation_units;
using abigail::suppr::suppression_sptr;
using abigail::suppr::suppressions_type;
using abigail::suppr::read_suppressions;
using abigail::comparison::corpus_diff;
using abigail::comparison::corpus_diff_sptr;
using abigail::comparison::compute_diff;
using abigail::comparison::diff_context_sptr;
using abigail::comparison::diff_context;
using abigail::xml_writer::SEQUENCE_TYPE_ID_STYLE;
using abigail::xml_writer::HASH_TYPE_ID_STYLE;
using abigail::xml_writer::create_write_context;
using abigail::xml_writer::type_id_style_kind;
using abigail::xml_writer::write_context_sptr;
using abigail::xml_writer::write_corpus;
using abigail::xml_writer::write_corpus_group;
using abigail::abixml::read_corpus_from_abixml_file;

using namespace abigail;

struct options
{
  string		in_file_path;
  string		out_file_path;
  vector<string>	di_root_paths;
  vector<string>	headers_dirs;
  vector<string>	header_files;
  vector<string>	added_bins_dirs;
  vector<string>	added_bins;
  string		vmlinux;
  vector<string>	suppression_paths;
  vector<string>	kabi_whitelist_paths;
  suppressions_type	kabi_whitelist_supprs;
  bool			display_abixml_version;
  bool			fail_no_debug_info;
  bool			check_alt_debug_info_path;
  bool			show_base_name_alt_debug_info_path;
  bool			write_architecture;
  bool			write_corpus_path;
  bool			write_comp_dir;
  bool			write_elf_needed;
  bool			write_parameter_names;
  bool			short_locs;
  bool			default_sizes;
  bool			load_all_types;
  bool			load_undefined_interfaces;
  bool			linux_kernel_mode;
  bool			corpus_group_for_linux;
  bool			show_stats;
  bool			noout;
  bool			follow_dependencies;
  bool			list_dependencies;
#ifdef WITH_CTF
  bool			use_ctf;
#endif
#ifdef WITH_BTF
  bool			use_btf;
#endif
  bool			show_locs;
  bool			abidiff;
#ifdef WITH_DEBUG_SELF_COMPARISON
  bool			debug_abidiff;
#endif
#ifdef WITH_DEBUG_TYPE_CANONICALIZATION
  bool			debug_type_canonicalization;
  bool			debug_die_canonicalization;
#endif
  bool			annotate;
  bool			do_log;
  bool			drop_private_types;
  bool			force_early_suppression;
  bool			drop_undefined_syms;
  bool			assume_odr_for_cplusplus;
  bool			leverage_dwarf_factorization;
  optional<bool>	exported_interfaces_only;
  type_id_style_kind	type_id_style;
#ifdef WITH_DEBUG_SELF_COMPARISON
  string		type_id_file_path;
#endif

  options()
    : display_abixml_version(),
      fail_no_debug_info(),
      check_alt_debug_info_path(),
      show_base_name_alt_debug_info_path(),
      write_architecture(true),
      write_corpus_path(true),
      write_comp_dir(true),
      write_elf_needed(true),
      write_parameter_names(true),
      short_locs(false),
      default_sizes(true),
      load_all_types(),
      load_undefined_interfaces(true),
      linux_kernel_mode(true),
      corpus_group_for_linux(false),
      show_stats(),
      noout(),
      follow_dependencies(),
      list_dependencies(),
#ifdef WITH_CTF
      use_ctf(false),
#endif
#ifdef WITH_BTF
      use_btf(false),
#endif
      show_locs(true),
      abidiff(),
#ifdef WITH_DEBUG_SELF_COMPARISON
      debug_abidiff(),
#endif
#ifdef WITH_DEBUG_TYPE_CANONICALIZATION
      debug_type_canonicalization(),
      debug_die_canonicalization(),
#endif
      annotate(),
      do_log(),
      drop_private_types(false),
      force_early_suppression(false),
      drop_undefined_syms(false),
      assume_odr_for_cplusplus(true),
      leverage_dwarf_factorization(true),
      type_id_style(SEQUENCE_TYPE_ID_STYLE)
  {}

  ~options()
  {
  }
};

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
    if (!check_file(*i, cerr, "abidw"))
      return false;

  for (vector<string>::const_iterator i =
	 opts.kabi_whitelist_paths.begin();
       i != opts.kabi_whitelist_paths.end();
       ++i)
    if (!check_file(*i, cerr, "abidw"))
      return false;

  return true;
}

/// Check that the header files supplied are present.
/// If not, emit an error on stderr.
///
/// @param opts the options instance to use.
///
/// @return true if all header files are present, false otherwise.
static bool
maybe_check_header_files(const options& opts)
{
  for (vector<string>::const_iterator file = opts.header_files.begin();
       file != opts.header_files.end();
       ++file)
    if (!check_file(*file, cerr, "abidw"))
      return false;

  return true;
}

/// Set suppression specifications to the @p read_context used to load
/// the ABI corpus from the ELF/DWARF file.
///
/// These suppression specifications are going to be applied to drop
/// some ABI artifacts on the floor (while reading the ELF/DWARF file)
/// and thus minimize the size of the resulting ABI corpus.
///
/// @param read_ctxt the read context to apply the suppression
/// specifications to.
///
/// @param opts the options where to get the suppression
/// specifications from.
static void
set_suppressions(abigail::elf_based_reader& rdr, options& opts)
{
  suppressions_type supprs;
  for (vector<string>::const_iterator i = opts.suppression_paths.begin();
       i != opts.suppression_paths.end();
       ++i)
    read_suppressions(*i, supprs);

  if (opts.force_early_suppression)
    // User asked to unconditionally drop suppressed artifacts from
    // the IR.  Let's drop all nodes matched by suppression
    // specifications from the IR.
    for (auto& s : supprs)
      s->set_drops_artifact_from_ir(true);

  suppression_sptr suppr =
    abigail::tools_utils::gen_suppr_spec_from_headers(opts.headers_dirs,
						      opts.header_files);
  if (suppr)
    {
      if (opts.drop_private_types)
	suppr->set_drops_artifact_from_ir(true);
      supprs.push_back(suppr);
    }

  using abigail::tools_utils::gen_suppr_spec_from_kernel_abi_whitelists;
  const suppressions_type& wl_suppr =
      gen_suppr_spec_from_kernel_abi_whitelists(opts.kabi_whitelist_paths);

  opts.kabi_whitelist_supprs.insert(opts.kabi_whitelist_supprs.end(),
				    wl_suppr.begin(), wl_suppr.end());

  rdr.add_suppressions(supprs);
  rdr.add_suppressions(opts.kabi_whitelist_supprs);
}

/// Set a bunch of tunable buttons on the ELF-based reader from the
/// command-line options.
///
/// @param rdr the reader to tune.
///
/// @param opts the command line options.
static void
set_generic_options(abigail::fe_iface& rdr, options& opts)
{
  rdr.options().drop_undefined_syms = opts.drop_undefined_syms;
  rdr.options().show_stats = opts.show_stats;
  rdr.options().do_log = opts.do_log;
  rdr.options().leverage_dwarf_factorization =
    opts.leverage_dwarf_factorization;
  rdr.options().assume_odr_for_cplusplus =
    opts.assume_odr_for_cplusplus;
  rdr.options().load_undefined_interfaces = opts.load_undefined_interfaces;
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
///
/// @param t the timer to be used for the logs.
///
/// @param opts the options passed to the main program.
///
/// @param argv the vector of arguments of the main program.
///
/// @return 0 if the self comparison did yield the empty set, 1
/// otherwise.  If the comparison does (wronly) yield a result, that
/// result if emitted on std::cerr.
static int
perform_self_comparison(const write_context_sptr& write_ctxt,
			const corpus_sptr& corp,
			environment& env,
			timer& t,
			options& opts,
			char* argv[])
{
  // Save the abi in abixml format in a temporary file, read
  // it back, and compare the ABI of what we've read back
  // against the ABI of the input ELF file.
  temp_file_sptr tmp_file = temp_file::create();
  set_ostream(*write_ctxt, tmp_file->get_stream());
  corpus_group_sptr corp_group = is_corpus_group(corp);

  if (corp_group)
    write_corpus_group(*write_ctxt, corp_group, 0);
  else
    write_corpus(*write_ctxt, corp, 0);
  tmp_file->get_stream().flush();

#ifdef WITH_DEBUG_SELF_COMPARISON
  if (opts.debug_abidiff)
    {
      opts.type_id_file_path = tmp_file->get_path() + string(".typeid");
      write_canonical_type_ids(*write_ctxt, opts.type_id_file_path);
    }
#endif
  fe_iface_sptr rdr = abixml::create_reader(tmp_file->get_path(), env);
  set_generic_options(*rdr, opts);

#ifdef WITH_DEBUG_SELF_COMPARISON
  if (opts.debug_abidiff
      && !opts.type_id_file_path.empty())
    load_canonical_type_ids(*rdr, opts.type_id_file_path);
#endif

  t.start();
  fe_iface::status sts;
  corpus_sptr corp2;
  corpus_group_sptr corp_group2;

  if (corp_group)
    corp_group2 = abixml::read_corpus_group_from_input(*rdr);
  else
    corp2 = rdr->read_corpus(sts);

  t.stop();
  if (opts.do_log)
    emit_prefix(argv[0], cerr)
      << "Read corpus in: " << t << "\n";

#ifdef WITH_DEBUG_SELF_COMPARISON
  if (opts.debug_abidiff
      && !opts.type_id_file_path.empty())
    remove(opts.type_id_file_path.c_str());
#endif

  if (!corp2 && !corp_group2)
    {
      emit_prefix(argv[0], cerr)
	<< "Could not read temporary XML representation of "
	"elf file back\n";
      return 1;
    }

  diff_context_sptr ctxt(new diff_context);
  set_diff_context(ctxt);
  ctxt->show_locs(opts.show_locs);
  t.start();
  corpus_diff_sptr diff =
    corp_group2
    ? compute_diff(corp_group, corp_group2, ctxt)
    : compute_diff(corp, corp2, ctxt);

  t.stop();
  if (opts.do_log)
    emit_prefix(argv[0], cerr)
      << "computed diff in: " << t << "\n";

  bool has_error = diff->has_changes();
  if (has_error)
    {
      t.start();
      diff->report(cerr);
      t.stop();
      if (opts.do_log)
	emit_prefix(argv[0], cerr)
	  << "emitted report in: " << t << "\n";
      return 1;
    }
  return 0;
}

/// Load an ABI @ref corpus (the internal representation of the ABI of
/// a binary) and write it out as an abixml.
///
/// @param argv the arguments the program was called with.
///
/// @param env the environment the ABI artifacts are being created in.
///
/// @param opts the options of the program.
///
/// @return the exit code: 0 if everything went fine, non-zero
/// otherwise.
static int
load_corpus_and_write_abixml(char* argv[],
			     environment& env,
			     options& opts)
{
  int exit_code = 0;
  timer t;

#ifdef WITH_DEBUG_SELF_COMPARISON
  if (opts.debug_abidiff)
    env.self_comparison_debug_is_on(true);
#endif

#ifdef WITH_DEBUG_TYPE_CANONICALIZATION
  if (opts.debug_type_canonicalization)
    env.debug_type_canonicalization_is_on(true);
  if (opts.debug_die_canonicalization)
    env.debug_die_canonicalization_is_on(true);
#endif

  corpus_sptr corp;
  corpus_group_sptr corp_group;
  fe_iface::status s = fe_iface::STATUS_UNKNOWN;
  corpus::origin requested_fe_kind = corpus::DWARF_ORIGIN;
#ifdef WITH_CTF
  if (opts.use_ctf)
    requested_fe_kind = corpus::CTF_ORIGIN;
#endif
#ifdef WITH_BTF
  if (opts.use_btf)
    requested_fe_kind = corpus::BTF_ORIGIN;
#endif

  // First of all, create a reader to read the ABI from the file
  // specfied in opts ...
  abigail::elf_based_reader_sptr reader =
    create_best_elf_based_reader(opts.in_file_path,
				 opts.di_root_paths,
				 env, requested_fe_kind,
				 opts.load_all_types,
				 opts.linux_kernel_mode);
  ABG_ASSERT(reader);

  // ... then tune a bunch of "buttons" on the newly created reader
  // ...
  set_generic_options(*reader, opts);
  set_suppressions(*reader, opts);

  // If the user asked us to check if we found the "alternate debug
  // info file" associated to the input binary, then proceed to do so
  // ...
  if (opts.check_alt_debug_info_path)
    {
      string alt_di_path = reader->alternate_dwarf_debug_info_path();
      if (!alt_di_path.empty())
	{
	  cout << "found the alternate debug info file";
	  if (opts.show_base_name_alt_debug_info_path)
	    {
	      tools_utils::base_name(alt_di_path, alt_di_path);
	      cout << " '" << alt_di_path << "'";
	    }
	  cout << "\n";
	  return 0;
	}
      else
	{
	  emit_prefix(argv[0], cerr)
	    << "could not find alternate debug info file\n";
	  return 1;
	}
    }

  // ... ff we are asked to only analyze exported interfaces (to stay
  // concise), then take that into account ...
  if (opts.exported_interfaces_only.has_value())
    env.analyze_exported_interfaces_only(*opts.exported_interfaces_only);

  // And now, really read/analyze the ABI of the input file.
  t.start();
  corp = reader->read_corpus(s);
  t.stop();
  if (opts.do_log)
    emit_prefix(argv[0], cerr)
      << "read corpus from elf file in: " << t << "\n";

  if (opts.do_log)
    emit_prefix(argv[0], cerr)
      << "reset reader ELF in: " << t << "\n";

  // If we couldn't create a corpus, emit some (hopefully) useful
  // diagnostics and return an error.
  if (!corp)
    {
      if (s == fe_iface::STATUS_DEBUG_INFO_NOT_FOUND)
	{
	  if (opts.di_root_paths.empty())
	    {
	      emit_prefix(argv[0], cerr)
		<< "Could not read debug info from "
		<< opts.in_file_path << "\n";

	      emit_prefix(argv[0], cerr)
		<< "You might want to supply the root directory where "
		"to search debug info from, using the "
		"--debug-info-dir option "
		"(e.g --debug-info-dir /usr/lib/debug)\n";
	    }
	  else
	    {
	      emit_prefix(argv[0], cerr)
		<< "Could not read debug info for '" << opts.in_file_path
		<< "' from debug info root directory '";
	      for (vector<string>::const_iterator i =
		     opts.di_root_paths.begin();
		   i != opts.di_root_paths.end();
		   ++i)
		{
		  if (i != opts.di_root_paths.begin())
		    cerr << ", ";
		  cerr << *i;
		}
	    }
	}
      else if (s == fe_iface::STATUS_NO_SYMBOLS_FOUND)
	emit_prefix(argv[0], cerr)
	  << "Could not read ELF symbol information from "
	  << opts.in_file_path << "\n";
      else if (s & fe_iface::STATUS_ALT_DEBUG_INFO_NOT_FOUND)
	{
	  emit_prefix(argv[0], cerr)
	    << "Could not read alternate debug info file";
	  if (!reader->alternate_dwarf_debug_info_path().empty())
	    cerr << " '" << reader->alternate_dwarf_debug_info_path() << "'";
	  cerr << " for '"
	    << opts.in_file_path << "'.\n";
	  emit_prefix(argv[0], cerr)
	    << "You might have forgotten to install some "
	    "additional needed debug info\n";
	}

      return 1;
    }

  if (opts.fail_no_debug_info
      && s & fe_iface::STATUS_DEBUG_INFO_NOT_FOUND)
    {
      emit_prefix(argv[0], cerr)
	<< "Could not read debug info from "
	<< opts.in_file_path << "\n";

      emit_prefix(argv[0], cerr)
	<< "You might want to either recompile the binary with "
	"debug info support or supply the root directory where "
	"to search debug info from, using the "
	"--debug-info-dir option "
	"(e.g --debug-info-dir /usr/lib/debug)\n";
      return 1;
    }

  if (opts.list_dependencies)
    {
      // Show the dependencies of the corpus and display them.
      set<string> dependencies;
      if (tools_utils::get_dependencies(*corp, opts.added_bins_dirs,
					dependencies))
	{
	  cout << "Dependencies of '" << corp->get_path()
	       << "':\n\t";
	  int n = 0;
	  for (const auto& dep : dependencies)
	    {
	      if (n)
		cout << ", ";
	      cout << dep;
	      ++n;
	    }
	  cout << "\n";
	}
    }

  if (!opts.added_bins.empty())
    corp_group =
      stick_corpus_and_binaries_into_corpus_group(reader, corp,
						  opts.added_bins,
						  opts.added_bins_dirs);

  if (opts.follow_dependencies)
    {
      // load the dependencies of the corpus and put them all into a
      // corpus group.

      // If a corpus_group already exists, use that one ...
      if (corp_group && !corp_group->is_empty())
	add_dependencies_into_corpus_group(reader, *corp,
					   opts.added_bins_dirs,
					   *corp_group);
      else
	// .. otherwise, create a new corpus group.
	corp_group =
	  stick_corpus_and_dependencies_into_corpus_group(reader, corp,
							  opts.added_bins_dirs);
    }

  // Clear some resources to gain back some space.
  t.start();
  reader.reset();
  t.stop();

  // Now create a write context and write out an ABI XML description
  // of the read corpus.
  t.start();
  const write_context_sptr& write_ctxt = create_write_context(env, cout);
  set_common_options(*write_ctxt, opts);
  t.stop();

  if (opts.do_log)
    emit_prefix(argv[0], cerr)
      << "created & initialized write context in: "
      << t << "\n";

  if (opts.abidiff)
    return perform_self_comparison(write_ctxt,
				   corp_group ? corp_group : corp,
				   env, t, opts, argv);

  if (opts.noout)
    return 0;

  if (!opts.out_file_path.empty())
    {
      ofstream of(opts.out_file_path.c_str(), std::ios_base::trunc);
      if (!of.is_open())
        {
          emit_prefix(argv[0], cerr)
            << "could not open output file '"
            << opts.out_file_path << "'\n";
          return 1;
        }
      set_ostream(*write_ctxt, of);
      t.start();
      if (corp_group)
	write_corpus_group(*write_ctxt, corp_group, 0);
      else
	write_corpus(*write_ctxt, corp, 0);
      t.stop();
      if (opts.do_log)
        emit_prefix(argv[0], cerr)
          << "emitted abixml output in: " << t << "\n";
      of.close();
      return 0;
    }
  else
    {
      t.start();
      exit_code =
	corp_group
	? !write_corpus_group(*write_ctxt, corp_group, 0)
	: !write_corpus(*write_ctxt, corp, 0);
      t.stop();
      if (opts.do_log)
        emit_prefix(argv[0], cerr)
          << "emitted abixml out in: " << t << "\n";
    }

  return exit_code;
}

/// Load a corpus group representing the union of a Linux Kernel
/// vmlinux binary and its modules, and emit an abixml representation
/// for it.
///
/// @param argv the arguments this program was called with.
///
/// @param env the environment the ABI artifacts are created in.
///
/// @param opts the options this program was created with.
///
/// @return the exit code.  Zero if everything went well, non-zero
/// otherwise.
static int
load_kernel_corpus_group_and_write_abixml(char* argv[],
					  environment& env,
					  options& opts)
{
  if (!(tools_utils::is_dir(opts.in_file_path) && opts.corpus_group_for_linux))
    return 1;

  int exit_code = 0;

  if (!opts.vmlinux.empty())
    if (!abigail::tools_utils::check_file(opts.vmlinux, cerr, argv[0]))
      return 1;

#ifdef WITH_DEBUG_SELF_COMPARISON
  if (opts.debug_abidiff)
    env.self_comparison_debug_is_on(true);
#endif

  timer t, global_timer;
  suppressions_type supprs;

  if (opts.exported_interfaces_only.has_value())
    env.analyze_exported_interfaces_only(*opts.exported_interfaces_only);

  if (opts.do_log)
    emit_prefix(argv[0], cerr)
      << "going to build ABI representation of the Linux Kernel ...\n";

  global_timer.start();
  t.start();
  corpus::origin requested_fe_kind =
#ifdef WITH_CTF
    opts.use_ctf ? corpus::CTF_ORIGIN :
#endif
#ifdef WITH_BTF
    opts.use_btf ? corpus::BTF_ORIGIN :
#endif
    corpus::DWARF_ORIGIN;
  corpus_group_sptr group =
    build_corpus_group_from_kernel_dist_under(opts.in_file_path,
					      /*debug_info_root=*/"",
					      opts.vmlinux,
					      opts.suppression_paths,
					      opts.kabi_whitelist_paths,
					      supprs, opts.do_log, env,
					      requested_fe_kind);
  t.stop();

  if (opts.do_log)
    {
      emit_prefix(argv[0], cerr)
	<< "built ABI representation of the Linux Kernel in: "
	<< t << "\n";
    }

  if (!group)
    return 1;

  if (!opts.noout)
    {
      const xml_writer::write_context_sptr& ctxt
	  = xml_writer::create_write_context(env, cout);
      set_common_options(*ctxt, opts);

      if (opts.abidiff)
	return perform_self_comparison(ctxt, group, env, t, opts, argv);

      if (!opts.out_file_path.empty())
	{
	  ofstream of(opts.out_file_path.c_str(), std::ios_base::trunc);
	  if (!of.is_open())
	    {
	      emit_prefix(argv[0], cerr)
		<< "could not open output file '"
		<< opts.out_file_path << "'\n";
	      return 1;
	    }

	  if (opts.do_log)
	    emit_prefix(argv[0], cerr)
	      << "emitting the abixml output ...\n";
	  set_ostream(*ctxt, of);
	  t.start();
	  exit_code = !write_corpus_group(*ctxt, group, 0);
	  t.stop();
	  if (opts.do_log)
	    emit_prefix(argv[0], cerr)
	      << "emitted abixml output in: " << t << "\n";
	}
      else
	{
	  if (opts.do_log)
	    emit_prefix(argv[0], cerr)
	      << "emitting the abixml output ...\n";
	  t.start();
	  exit_code = !write_corpus_group(*ctxt, group, 0);
	  t.stop();
	  if (opts.do_log)
	    emit_prefix(argv[0], cerr)
	      << "emitted abixml output in: " << t << "\n";
	}
    }

  global_timer.stop();
  if (opts.do_log)
    emit_prefix(argv[0], cerr)
      << "total processing done in " << global_timer << "\n";
  return exit_code;
}

enum option_key
{
  OPT_ABIDIFF = 256,
  OPT_ABIXML_VERSION,
  OPT_ADD_BINARIES,
  OPT_ADDED_BINS_DIR,
  OPT_ALLOW_NON_EXPORTED_INTERFACES,
  OPT_ANNOTATE,
  OPT_BTF,
  OPT_CHECK_ALT_DEBUG_INFO,
  OPT_CHECK_ALT_DEBUG_INFO_BASE_NAME,
  OPT_CTF,
  OPT_DEBUG_ABIDIFF,
  OPT_DEBUG_DC,
  OPT_DEBUG_TC,
  OPT_DEBUG_INFO_DIR,
  OPT_DROP_PRIVATE_TYPES,
  OPT_DROP_UNDEFINED_SYMS,
  OPT_EXPORTED_INTERFACES_ONLY,
  OPT_FAIL_NO_DEBUG_INFO,
  OPT_FDEPS,
  OPT_FORCE_EARLY_SUPPRESSION,
  OPT_HD,
  OPT_HF,
  OPT_KMI_WHITELIST,
  OPT_LDEPS,
  OPT_LINUX_TREE,
  OPT_LOAD_ALL_TYPES,
  OPT_NO_ARCH,
  OPT_NO_ASSUME_ODR_FOR_CPLUSPLUS,
  OPT_NO_COMP_DIR_PATH,
  OPT_NO_CORPUS_PATH,
  OPT_NO_ELF_NEEDED,
  OPT_NO_LEVERAGE_DWARF_FACTORIZATION,
  OPT_NO_LINUX_KERNEL_MODE,
  OPT_NO_LOAD_UNDEFINED_INTERFACES,
  OPT_NO_PARAMETER_NAMES,
  OPT_NO_SHOW_LOCS,
  OPT_NO_WRITE_DEFAULT_SIZES,
  OPT_NOOUT,
  OPT_OUT_FILE,
  OPT_SHORT_LOCS,
  OPT_SHOW_STATS,
  OPT_SUPPR,
  OPT_TYPE_ID_STYLE,
  OPT_VERBOSE,
  OPT_VMLINUX,
};

static const struct argp_option argp_options[] =
{
  { "abidiff", OPT_ABIDIFF, 0, 0,
    "compare the loaded ABI against itself", 0 },
  { "abixml-version", OPT_ABIXML_VERSION, 0, 0,
    "display the version of the ABIXML ABI format", 0 },
  { "add-binaries", OPT_ADD_BINARIES, "BIN1,BIN2,..", 0,
    "build a corpus group with the added binaries", 0 },
  { "added-binaries-dir", OPT_ADDED_BINS_DIR, "PATH", 0,
    "where to look for dependencies or added binaries", 0 },
  { "abd", OPT_ADDED_BINS_DIR, "PATH", OPTION_ALIAS, 0, 0 },
  { "allow-non-exported-interfaces", OPT_ALLOW_NON_EXPORTED_INTERFACES, 0, 0,
    "analyze interfaces that might not be exported", 0 },
  { "annotate", OPT_ANNOTATE, 0, 0,
    "annotate the ABI artifacts emitted in the output", 0 },
#ifdef WITH_BTF
  { "btf", OPT_BTF, 0, 0,
    "use BTF instead of DWARF in ELF files", 0 },
#endif
  { "check-alternate-debug-info", OPT_CHECK_ALT_DEBUG_INFO, "ELF-PATH", 0,
    "check alternate debug info of <elf-path>", 0 },
  { "check-alternate-debug-info-base-name",
    OPT_CHECK_ALT_DEBUG_INFO_BASE_NAME, "ELF-PATH", 0,
    "check alternate debug info of <elf-path>, and show its base name", 0 },
#ifdef WITH_CTF
  { "ctf", OPT_CTF, 0, 0,
    "use CTF instead of DWARF in ELF files", 0 },
#endif
#ifdef WITH_DEBUG_SELF_COMPARISON
  { "debug-abidiff", OPT_DEBUG_ABIDIFF, 0, 0,
    "debug the process of comparing the loaded ABI against itself", 0 },
#endif
#ifdef WITH_DEBUG_TYPE_CANONICALIZATION
  { "debug-dc", OPT_DEBUG_DC, 0, 0,
    "debug the DIE canonicalization process", 0 },
  { "debug-tc", OPT_DEBUG_TC, 0, 0,
    "debug the type canonicalization process", 0 },
#endif
  { "debug-info-dir", OPT_DEBUG_INFO_DIR, "PATH", 0,
    "look for debug info under 'dir-path'", 0 },
  { "d", OPT_DEBUG_INFO_DIR, "PATH", OPTION_ALIAS, 0, 0 },
  { "drop-private-types", OPT_DROP_PRIVATE_TYPES, 0, 0,
    "drop private types from internal representation", 0 },
  { "drop-undefined-syms", OPT_DROP_UNDEFINED_SYMS, 0, 0,
    "drop undefined symbols from representation", 0 },
  { "exported-interfaces-only", OPT_EXPORTED_INTERFACES_ONLY, 0, 0,
    "analyze exported interfaces only", 0 },
  { "fail-no-debug-info", OPT_FAIL_NO_DEBUG_INFO, 0, 0,
    "bail out if no debug info was found", 0 },
  { "follow-dependencies", OPT_FDEPS, 0, 0,
    "build a corpus group with the dependencies", 0 },
  { "force-early-suppression", OPT_FORCE_EARLY_SUPPRESSION, 0, 0,
    "drop IR nodes that match suppression specifications", 0 },
  { "headers-dir", OPT_HD, "PATH", 0,
    "the path to headers of the elf file", 0 },
  { "hd", OPT_HD, "PATH", OPTION_ALIAS, 0, 0 },
  { "header-file", OPT_HF, "PATH", 0,
    "the path to one header of the elf file", 0 },
  { "hf", OPT_HF, "PATH", OPTION_ALIAS, 0, 0 },
  { "kmi-whitelist", OPT_KMI_WHITELIST, "PATH", 0,
    "path to a linux kernel abi whitelist", 0 },
  { "kmi-stablelist", OPT_KMI_WHITELIST, "PATH", OPTION_ALIAS, 0, 0 },
  { "w", OPT_KMI_WHITELIST, "PATH", OPTION_ALIAS, 0, 0 },
  { "list-dependencies", OPT_LDEPS, 0, 0,
    "list the dependencies of a given binary", 0 },
  { "linux-tree", OPT_LINUX_TREE, 0, 0,
    "emit the ABI for the union of a vmlinux and its modules", 0 },
  { "lt", OPT_LINUX_TREE, 0, OPTION_ALIAS, 0, 0 },
  { "load-all-types", OPT_LOAD_ALL_TYPES, 0, 0,
    "read all types including those not reachable from exported declarations",
    0 },
  { "no-architecture", OPT_NO_ARCH, 0, 0,
    "do not emit architecture info in the output", 0 },
  { "no-assume-odr-for-cplusplus", OPT_NO_ASSUME_ODR_FOR_CPLUSPLUS, 0, 0,
    "do not assume the ODR to speed-up the analysis of the binary", 0 },
  { "no-comp-dir-path", OPT_NO_COMP_DIR_PATH, 0, 0,
    "do not show compilation path information", 0 },
  { "no-corpus-path", OPT_NO_CORPUS_PATH, 0, 0,
    "do not take the path to the corpora into account", 0 },
  { "no-elf-needed", OPT_NO_ELF_NEEDED, 0, 0,
    "do not show the DT_NEEDED information", 0 },
  { "no-leverage-dwarf-factorization", OPT_NO_LEVERAGE_DWARF_FACTORIZATION,
    0, 0,
    "do not use DWZ optimisations to speed-up the analysis of the binary", 0 },
  { "no-linux-kernel-mode", OPT_NO_LINUX_KERNEL_MODE, 0, 0,
    "don't consider the input binary as a Linux Kernel binary", 0 },
  { "no-load-undefined-interfaces", OPT_NO_LOAD_UNDEFINED_INTERFACES, 0, 0,
    "do not consider undefined interfaces from the binary", 0 },
  { "no-parameter-names", OPT_NO_PARAMETER_NAMES, 0, 0,
    "do not show names of function parameters", 0 },
  { "no-show-locs", OPT_NO_SHOW_LOCS, 0, 0,
    "do not show location information", 0 },
  { "no-write-default-sizes", OPT_NO_WRITE_DEFAULT_SIZES, 0, 0,
    "do not emit pointer size when it equals the default address size of "
    "the translation unit", 0 },
  { "noout", OPT_NOOUT, 0, 0,
    "do not emit anything after reading the binary", 0 },
  { "out-file", OPT_OUT_FILE, "PATH", 0,
    "write the output to 'file-path'", 0 },
  { "o", OPT_OUT_FILE, "PATH", OPTION_ALIAS, 0, 0 },
  { "short-locs", OPT_SHORT_LOCS, 0, 0,
    "only print filenames rather than paths", 0 },
  { "stats", OPT_SHOW_STATS, 0, 0,
    "show statistics about various internal stuff", 0 },
  { "suppressions", OPT_SUPPR, "PATH", 0,
    "specify a suppression file", 0 },
  { "suppr", OPT_SUPPR, "PATH", OPTION_ALIAS, 0, 0 },
  { "type-id-style", OPT_TYPE_ID_STYLE, "STYLE", 0,
    "type id style (sequence(default): \"type-id-\" + number; hash: "
    "hex-digits)", 0 },
  { "verbose", OPT_VERBOSE, 0, 0,
    "show verbose messages about internal stuff", 0 },
  { "vmlinux", OPT_VMLINUX, "PATH", 0,
    "the path to the vmlinux binary to consider to emit the ABI of the union "
    "of vmlinux and its modules", 0 },
  { 0, 0, 0, 0, 0, 0 }
};

static error_t
parse_opt(int key, char* arg, struct argp_state* state)
{
  options& opts = *static_cast<options*>(state->input);
  const string argument = arg ? string(arg) : string();

  switch (key)
    {
    case OPT_ABIDIFF:
      opts.abidiff = true;
      break;

    case OPT_ABIXML_VERSION:
      opts.display_abixml_version = true;
      break;

    case OPT_ADD_BINARIES:
      if (argument.find(',') != string::npos)
	tools_utils::split_string(argument, ",", opts.added_bins);
      else
	opts.added_bins.push_back(argument);
      break;

    case OPT_ADDED_BINS_DIR:
      opts.added_bins_dirs.push_back(argument);
      break;

    case OPT_ALLOW_NON_EXPORTED_INTERFACES:
      opts.exported_interfaces_only = false;
      break;

    case OPT_ANNOTATE:
      opts.annotate = true;
      break;

#ifdef WITH_BTF
    case OPT_BTF:
      opts.use_btf = true;
      break;
#endif

    case OPT_CHECK_ALT_DEBUG_INFO:
      if (opts.in_file_path.empty())
	opts.in_file_path = argument;
      else
	argp_usage(state);
      opts.check_alt_debug_info_path = true;
      break;

    case OPT_CHECK_ALT_DEBUG_INFO_BASE_NAME:
      if (opts.in_file_path.empty())
	opts.in_file_path = argument;
      else
	argp_usage(state);
      opts.check_alt_debug_info_path = true;
      opts.show_base_name_alt_debug_info_path = true;
      break;

#ifdef WITH_CTF
    case OPT_CTF:
      opts.use_ctf = true;
      break;
#endif

#ifdef WITH_DEBUG_SELF_COMPARISON
    case OPT_DEBUG_ABIDIFF:
      opts.abidiff = true;
      opts.debug_abidiff = true;
      break;
#endif

#ifdef WITH_DEBUG_TYPE_CANONICALIZATION
    case OPT_DEBUG_DC:
      opts.debug_die_canonicalization = true;
      break;

    case OPT_DEBUG_TC:
      opts.debug_type_canonicalization = true;
      break;
#endif

    case OPT_DEBUG_INFO_DIR:
      opts.di_root_paths.push_back
	(abigail::tools_utils::make_path_absolute(argument));
      break;

    case OPT_DROP_PRIVATE_TYPES:
      opts.drop_private_types = true;
      break;

    case OPT_DROP_UNDEFINED_SYMS:
      opts.drop_undefined_syms = true;
      break;

    case OPT_EXPORTED_INTERFACES_ONLY:
      opts.exported_interfaces_only = true;
      break;

    case OPT_FAIL_NO_DEBUG_INFO:
      opts.fail_no_debug_info = true;
      break;

    case OPT_FDEPS:
      opts.follow_dependencies = true;
      break;

    case OPT_FORCE_EARLY_SUPPRESSION:
      opts.force_early_suppression = true;
      break;

    case OPT_HD:
      opts.headers_dirs.push_back(argument);
      opts.drop_private_types = true;
      break;

    case OPT_HF:
      opts.header_files.push_back(argument);
      opts.drop_private_types = true;
      break;

    case OPT_KMI_WHITELIST:
      opts.kabi_whitelist_paths.push_back(argument);
      break;

    case OPT_LDEPS:
      opts.list_dependencies = true;
      break;

    case OPT_LINUX_TREE:
      opts.corpus_group_for_linux = true;
      break;

    case OPT_LOAD_ALL_TYPES:
      opts.load_all_types = true;
      break;

    case OPT_NO_ARCH:
      opts.write_architecture = false;
      break;

    case OPT_NO_ASSUME_ODR_FOR_CPLUSPLUS:
      opts.assume_odr_for_cplusplus = false;
      break;

    case OPT_NO_COMP_DIR_PATH:
      opts.write_comp_dir = false;
      break;

    case OPT_NO_CORPUS_PATH:
      opts.write_corpus_path = false;
      break;

    case OPT_NO_ELF_NEEDED:
      opts.write_elf_needed = false;
      break;

    case OPT_NO_LEVERAGE_DWARF_FACTORIZATION:
      opts.leverage_dwarf_factorization = false;
      break;

    case OPT_NO_LINUX_KERNEL_MODE:
      opts.linux_kernel_mode = false;
      break;

    case OPT_NO_LOAD_UNDEFINED_INTERFACES:
      opts.load_undefined_interfaces = false;
      break;

    case OPT_NO_PARAMETER_NAMES:
      opts.write_parameter_names = false;
      break;

    case OPT_NO_SHOW_LOCS:
      opts.show_locs = false;
      break;

    case OPT_NO_WRITE_DEFAULT_SIZES:
      opts.default_sizes = false;
      break;

    case OPT_NOOUT:
      opts.noout = true;
      break;

    case OPT_OUT_FILE:
      if (!opts.out_file_path.empty())
	argp_usage(state);
      opts.out_file_path = argument;
      break;

    case OPT_SHORT_LOCS:
      opts.short_locs = true;
      break;

    case OPT_SHOW_STATS:
      opts.show_stats = true;
      break;

    case OPT_SUPPR:
      opts.suppression_paths.push_back(argument);
      break;

    case OPT_TYPE_ID_STYLE:
      if (argument == "sequence")
	opts.type_id_style = SEQUENCE_TYPE_ID_STYLE;
      else if (argument == "hash")
	opts.type_id_style = HASH_TYPE_ID_STYLE;
      else
	argp_usage(state);
      break;

    case OPT_VERBOSE:
      opts.do_log = true;
      break;

    case OPT_VMLINUX:
      opts.vmlinux = argument;
      break;

    case ARGP_KEY_ARG:
      if (opts.in_file_path.empty())
	opts.in_file_path = argument;
      else
	argp_usage(state);
      break;

    default:
      return ARGP_ERR_UNKNOWN;
    }

  return 0;
}

static const char* argp_args_doc = "[<path-to-elf-file>]";
static const char* argp_doc =
  "Read an ELF file, load its debug info and emit it in the native "
  "libabigail XML format.";

static const struct argp abidw_argp =
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
print_abidw_version(FILE *stream, struct argp_state* /*state*/)
{
  fprintf(stream, "abidw %s\n",
	  abigail::tools_utils::get_library_version_string().c_str());
}

/// Version printing hook to be passed to ARGP.
///
/// @param stream the output stream.
///
/// @param state the ARGP state.
static bool
parse_command_line(int argc, char* argv[], options& opts)
{
  argp_program_version_hook = print_abidw_version;
  argp_program_bug_address = "<libabigail@sourceware.org>";

  if (argp_parse(&abidw_argp, argc, argv, 0, 0, &opts) != 0)
    return false;
  return true;
}

int
main(int argc, char* argv[])
{
  options opts;

  abigail::tools_utils::initialize();

  if (!parse_command_line(argc, argv, opts)
      || (opts.in_file_path.empty()
	  && !opts.display_abixml_version))
    {
      char* prog_name = (char*) "abidw";
      argp_help(&abidw_argp, stderr, ARGP_HELP_USAGE, prog_name);
      return 1;
    }

    if (opts.display_abixml_version)
      {
	emit_prefix(argv[0], cout)
	  << abigail::tools_utils::get_abixml_version_string()
	  << "\n";
	return 0;
      }

  ABG_ASSERT(!opts.in_file_path.empty());
  if (opts.corpus_group_for_linux)
    {
      if (!abigail::tools_utils::check_dir(opts.in_file_path, cerr, argv[0]))
	return 1;
    }
  else
    {
      if (!abigail::tools_utils::check_file(opts.in_file_path, cerr, argv[0]))
	return 1;
    }

  if (!maybe_check_suppression_files(opts))
    return 1;

  if (!maybe_check_header_files(opts))
    return 1;

  abigail::tools_utils::file_type type =
    abigail::tools_utils::guess_file_type(opts.in_file_path);
  if (type != abigail::tools_utils::FILE_TYPE_ELF
      && type != abigail::tools_utils::FILE_TYPE_AR
      && type != abigail::tools_utils::FILE_TYPE_DIR)
    {
      emit_prefix(argv[0], cerr)
	<< "files of the kind of "<< opts.in_file_path << " are not handled\n";
      return 1;
    }

  environment env;
  int exit_code = 0;

  if (tools_utils::is_regular_file(opts.in_file_path))
    exit_code = load_corpus_and_write_abixml(argv, env, opts);
  else
    exit_code = load_kernel_corpus_group_and_write_abixml(argv, env, opts);

  return exit_code;
}
