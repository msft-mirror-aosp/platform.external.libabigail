// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// -*- Mode: C++ -*-
//
// Copyright (C) 2013-2026 Red Hat, Inc.
//
// Author: Dodji Seketeli

/// @file
///
/// This is a tool that reads an ini file and, if it could read it OK,
/// prints it on its standard output.  It's mainly meant to test the
/// abigail::ini::* functions, but one could also use it to make sure
/// that an ini file can be handled by the abigail::ini::* facilities

#include <argp.h>
#include <cstring>
#include <iostream>
#include "abg-ini.h"
#include "abg-tools-utils.h"

using std::cout;
using std::cerr;
using std::cin;
using std::string;
using std::ostream;
using abigail::ini::config;
using abigail::ini::config_sptr;
using abigail::ini::read_config;
using abigail::ini::write_config;

struct options
{
  bool read_from_stdin;
  bool no_out;
  string path;

  options ()
    : read_from_stdin(false),
      no_out(false)
  {}
};

enum option_key
{
  OPT_FROM_STDIN = 256,
  OPT_NOOUT,
};

static const struct argp_option argp_options[] =
{
  { "from-stdin", OPT_FROM_STDIN, 0, 0,
    "read the input ini file from stdin", 0 },
  { "noout", OPT_NOOUT, 0, 0,
    "do not output anything on stdout", 0 },
  { 0, 0, 0, 0, 0, 0 }
};

static error_t
parse_opt(int key, char* arg, struct argp_state* state)
{
  options& opts = *static_cast<options*>(state->input);
  const string argument = arg ? string(arg) : string();

  switch (key)
    {
    case OPT_FROM_STDIN:
      opts.read_from_stdin = true;
      break;

    case OPT_NOOUT:
      opts.no_out = true;
      break;

    case ARGP_KEY_ARG:
      if (opts.path.empty())
	opts.path = argument;
      else
	argp_usage(state);
      break;

    default:
      return ARGP_ERR_UNKNOWN;
    }

  return 0;
}

static const char* argp_args_doc = "[<ini-file>]";
static const char* argp_doc =
  "Read an ini file and print it back on standard output.";

static const struct argp binilint_argp =
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
print_binilint_version(FILE *stream, struct argp_state* /*state*/)
{
  fprintf(stream, "abinilint %s\n",
	  abigail::tools_utils::get_library_version_string().c_str());
}

static bool
parse_command_line(int argc, char* argv[], options& opts)
{
  argp_program_version_hook = print_binilint_version;
  argp_program_bug_address = "<libabigail@sourceware.org>";

  if (argp_parse(&binilint_argp, argc, argv, 0, 0, &opts) != 0)
    return false;
  return true;
}

int
main(int argc, char* argv[])
{
  options opts;
  if (!parse_command_line(argc, argv, opts))
    {
      char* prog_name = (char*) "abinilint";
      argp_help(&binilint_argp, stderr, ARGP_HELP_USAGE, prog_name);
      return 1;
    }

  // Do the real work we are supposed to do after all.  That real work
  // is driven by the options the user set; these options are recorded
  // in the opts variable.

  config_sptr conf;

  if (opts.read_from_stdin)
    conf = read_config(cin);
  else if (!opts.path.empty())
    conf = read_config(opts.path);

  if (conf && !opts.no_out)
    write_config(*conf, std::cout);

  return !conf;
}
