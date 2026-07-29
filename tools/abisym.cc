// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// -*- Mode: C++ -*-
//
// Copyright (C) 2013-2026 Red Hat, Inc.
//
// Author: Dodji Seketeli

/// @file
///
/// This program takes parameters to open an elf file, lookup a symbol
/// in its symbol tables and report what it sees.

#include <argp.h>
#include <libgen.h>
#include <elf.h>
#include <cstring>
#include <iostream>
#include <sstream>
#include "abg-config.h"
#include "abg-dwarf-reader.h"
#include "abg-ir.h"
#include "abg-tools-utils.h"

using std::cout;
using std::cerr;
using std::string;
using std::ostream;
using std::ostringstream;
using std::vector;

using abigail::ir::environment;
using abigail::dwarf::lookup_symbol_from_elf;
using abigail::elf_symbol;
using abigail::elf_symbol_sptr;

struct options
{
  string elf_path;
  string symbol_name;
  bool	demangle;
  bool absolute_path;

  options()
    : demangle(false),
      absolute_path(true)
  {}
};


enum option_key
{
  OPT_DEMANGLE = 256,
  OPT_NO_ABSOLUTE_PATH,
};

static const struct argp_option argp_options[] =
{
  { "demangle", OPT_DEMANGLE, 0, 0,
    "demangle the symbols from the symbol table", 0 },
  { "no-absolute-path", OPT_NO_ABSOLUTE_PATH, 0, 0,
    "do not show absolute paths in messages", 0 },
  { 0, 0, 0, 0, 0, 0 }
};

static error_t
parse_opt(int key, char* arg, struct argp_state* state)
{
  options& opts = *static_cast<options*>(state->input);
  const string argument = arg ? string(arg) : string();

  switch (key)
    {
    case OPT_DEMANGLE:
      opts.demangle = true;
      break;

    case OPT_NO_ABSOLUTE_PATH:
      opts.absolute_path = false;
      break;

    case ARGP_KEY_ARG:
      if (opts.elf_path.empty())
	opts.elf_path = argument;
      else if (opts.symbol_name.empty())
	opts.symbol_name = argument;
      else
	argp_usage(state);
      break;

    case ARGP_KEY_END:
      if (opts.elf_path.empty() || opts.symbol_name.empty())
	argp_usage(state);
      break;

    default:
      return ARGP_ERR_UNKNOWN;
    }

  return 0;
}

static const char* argp_args_doc = "<elf-file> <symbol-name>";
static const char* argp_doc =
  "Look up a symbol in the symbol table of an ELF file and report "
  "information about it.";

static const struct argp abisym_argp =
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
static void
print_abisym_version(FILE *stream, struct argp_state* /*state*/)
{
  fprintf(stream, "abisym %s\n",
	  abigail::tools_utils::get_library_version_string().c_str());
}

/// Parse the command line
///
/// @param argc number of args
///
/// @param argv the array of arguments.
///
/// @param opts the options set as result of command line parsing.
static void
parse_command_line(int argc, char* argv[], options& opts)
{
  argp_program_version_hook = print_abisym_version;
  argp_program_bug_address = "<libabigail@sourceware.org>";

  argp_parse(&abisym_argp, argc, argv, 0, 0, &opts);
}

int
main(int argc, char* argv[])
{
  options opts;
  parse_command_line(argc, argv, opts);

  string p = opts.elf_path, n = opts.symbol_name;
  environment env;
  vector<elf_symbol_sptr> syms;
  if (!lookup_symbol_from_elf(env, p, n, opts.demangle, syms))
    {
      cout << "could not find symbol '"
	   << opts.symbol_name
	   << "' in file '";
      if (opts.absolute_path)
	cout << opts.elf_path << "'\n";
      else
	{
	  string b;
	  abigail::tools_utils::base_name(opts.elf_path, b);
	  cout << b;
	}
      return 0;
    }

  elf_symbol_sptr sym = syms[0];
  cout << "found symbol '" << n << "'";
  if (n != sym->get_name())
    cout << " (" << sym->get_name() << ")";
  cout << ", an instance of "
       << (elf_symbol::type) sym->get_type()
       << " of " << sym->get_binding();
  if (syms.size() > 1 || !sym->get_version().is_empty())
    {
      cout << ", of version";
      if (syms.size () > 1)
	cout << "s";
      cout << " ";
      for (vector<elf_symbol_sptr>::const_iterator i = syms.begin();
	   i != syms.end();
	   ++i)
	{
	  if (i != syms.begin())
	    cout << ", ";
	  cout << "'" << (*i)->get_version().str() << "'";
	}
    }
  cout << '\n';

  return 0;
}
