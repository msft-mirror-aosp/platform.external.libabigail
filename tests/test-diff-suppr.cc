// -*- Mode: C++ -*-
//
// Copyright (C) 2013-2014 Red Hat, Inc.
//
// This file is part of the GNU Application Binary Interface Generic
// Analysis and Instrumentation Library (libabigail).  This library is
// free software; you can redistribute it and/or modify it under the
// terms of the GNU Lesser General Public License as published by the
// Free Software Foundation; either version 3, or (at your option) any
// later version.

// This library is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// General Lesser Public License for more details.

// You should have received a copy of the GNU Lesser General Public
// License along with this program; see the file COPYING-LGPLV3.  If
// not, see <http://www.gnu.org/licenses/>.

// Author: Dodji Seketeli

/// @file
///
/// This test harness program runs a diff between input ELF files
/// containing DWARF debugging information, exercising the
/// suppressions features of the "bidiff" command line program.
///
/// So it runs the diff diff between the two input files, using a
/// suppression file and compares the resulting diff with a reference
/// one.

#include <cstring>
#include <string>
#include <fstream>
#include <iostream>
#include <cstdlib>
#include "abg-tools-utils.h"
#include "test-utils.h"

using std::string;
using std::cerr;

/// This is an aggregate that specifies where a test shall get its
/// input from and where it shall write its ouput to.
struct InOutSpec
{
  const char* in_elfv0_path;
  const char* in_elfv1_path;
  const char* in_suppr_path;
  const char* bidiff_options;
  const char* in_report_path;
  const char* out_report_path;
}; // end struct InOutSpec;

InOutSpec in_out_specs[] =
{
  {
    "data/test-diff-suppr/test0-type-suppr-v0.o",
    "data/test-diff-suppr/test0-type-suppr-v1.o",
    NULL,
    "",
    "data/test-diff-suppr/test0-type-suppr-report-0.txt",
    "output/test-diff-suppr/test0-type-suppr-report-0.txt",
  },
  {
    "data/test-diff-suppr/test0-type-suppr-v0.o",
    "data/test-diff-suppr/test0-type-suppr-v1.o",
    "data/test-diff-suppr/test0-type-suppr-0.suppr",
    "",
    "data/test-diff-suppr/test0-type-suppr-report-1.txt",
    "output/test-diff-suppr/test0-type-suppr-report-1.txt",
  },
  {
    "data/test-diff-suppr/test0-type-suppr-v0.o",
    "data/test-diff-suppr/test0-type-suppr-v1.o",
    "data/test-diff-suppr/test0-type-suppr-1.suppr",
    "",
    "data/test-diff-suppr/test0-type-suppr-report-2.txt",
    "output/test-diff-suppr/test0-type-suppr-report-2.txt",
  },
  {
    "data/test-diff-suppr/test0-type-suppr-v0.o",
    "data/test-diff-suppr/test0-type-suppr-v1.o",
    "data/test-diff-suppr/test0-type-suppr-2.suppr",
    "",
    "data/test-diff-suppr/test0-type-suppr-report-3.txt",
    "output/test-diff-suppr/test0-type-suppr-report-3.txt",
  },
  {
    "data/test-diff-suppr/test1-typedef-suppr-v0.o",
    "data/test-diff-suppr/test1-typedef-suppr-v1.o",
    "",
    "",
    "data/test-diff-suppr/test1-typedef-suppr-report-0.txt",
    "output/test-diff-suppr/test1-typedef-suppr-report-0.txt",
  },
  {
    "data/test-diff-suppr/test1-typedef-suppr-v0.o",
    "data/test-diff-suppr/test1-typedef-suppr-v1.o",
    "data/test-diff-suppr/test1-typedef-suppr-0.suppr",
    "",
    "data/test-diff-suppr/test1-typedef-suppr-report-1.txt",
    "output/test-diff-suppr/test1-typedef-suppr-report-1.txt",
  },
  {
    "data/test-diff-suppr/test1-typedef-suppr-v0.o",
    "data/test-diff-suppr/test1-typedef-suppr-v1.o",
    "data/test-diff-suppr/test1-typedef-suppr-1.suppr",
    "",
    "data/test-diff-suppr/test1-typedef-suppr-report-2.txt",
    "output/test-diff-suppr/test1-typedef-suppr-report-2.txt",
  },
  {
    "data/test-diff-suppr/test2-struct-suppr-v0.o",
    "data/test-diff-suppr/test2-struct-suppr-v1.o",
    "data/test-diff-suppr/test2-struct-suppr-0.suppr",
    "",
    "data/test-diff-suppr/test2-struct-suppr-report-0.txt",
    "output/test-diff-suppr/test2-struct-suppr-report-0.txt",
  },
  {
    "data/test-diff-suppr/test2-struct-suppr-v0.o",
    "data/test-diff-suppr/test2-struct-suppr-v1.o",
    "data/test-diff-suppr/test2-struct-suppr-1.suppr",
    "",
    "data/test-diff-suppr/test2-struct-suppr-report-1.txt",
    "output/test-diff-suppr/test2-struct-suppr-report-1.txt",
  },
  {
    "data/test-diff-suppr/test3-struct-suppr-v0.o",
    "data/test-diff-suppr/test3-struct-suppr-v1.o",
    NULL,
    "",
    "data/test-diff-suppr/test3-struct-suppr-report-0.txt",
    "output/test-diff-suppr/test3-struct-suppr-report-0.txt",
  },
  {
    "data/test-diff-suppr/test3-struct-suppr-v0.o",
    "data/test-diff-suppr/test3-struct-suppr-v1.o",
    "data/test-diff-suppr/test3-struct-suppr-0.suppr",
    "",
    "data/test-diff-suppr/test3-struct-suppr-report-1.txt",
    "output/test-diff-suppr/test3-struct-suppr-report-1.txt",
  },
  {
    "data/test-diff-suppr/test3-struct-suppr-v0.o",
    "data/test-diff-suppr/test3-struct-suppr-v1.o",
    "data/test-diff-suppr/test3-struct-suppr-1.suppr",
    "",
    "data/test-diff-suppr/test3-struct-suppr-report-2.txt",
    "output/test-diff-suppr/test3-struct-suppr-report-2.txt",
  },
  {
    "data/test-diff-suppr/libtest4-local-suppr-v0.so",
    "data/test-diff-suppr/libtest4-local-suppr-v1.so",
    "data/test-diff-suppr/test4-local-suppr-0.suppr",
    "",
    "data/test-diff-suppr/test4-local-suppr-report-1.txt",
    "output/test-diff-suppr/test4-local-suppr-report-1.txt",
  },
  {
    "data/test-diff-suppr/libtest4-local-suppr-v0.so",
    "data/test-diff-suppr/libtest4-local-suppr-v1.so",
    "",
    "",
    "data/test-diff-suppr/test4-local-suppr-report-0.txt",
    "output/test-diff-suppr/test4-local-suppr-report-0.txt",
  },
  // This should be the last entry
  {NULL, NULL, NULL, NULL, NULL, NULL}
};

int
main()
{
  using abigail::tests::get_src_dir;
  using abigail::tests::get_build_dir;
  using abigail::tools::ensure_parent_dir_created;

  bool is_ok = true;
  string in_elfv0_path, in_elfv1_path,
    in_suppression_path, bidiff_options, bidiff, cmd,
    ref_diff_report_path, out_diff_report_path;

    for (InOutSpec* s = in_out_specs; s->in_elfv0_path; ++s)
      {
	in_elfv0_path = get_src_dir() + "/tests/" + s->in_elfv0_path;
	in_elfv1_path = get_src_dir() + "/tests/" + s->in_elfv1_path;
	if (s->in_suppr_path && strcmp(s->in_suppr_path, ""))
	  in_suppression_path = get_src_dir() + "/tests/" + s->in_suppr_path;
	else
	  in_suppression_path.clear();

	bidiff_options = s->bidiff_options;
	ref_diff_report_path = get_src_dir() + "/tests/" + s->in_report_path;
	out_diff_report_path = get_build_dir() + "/tests/" + s->out_report_path;

	if (!ensure_parent_dir_created(out_diff_report_path))
	  {
	    cerr << "could not create parent directory for "
		 << out_diff_report_path;
	    is_ok = false;
	    continue;
	  }

	bidiff = get_build_dir() + "/tools/abidiff";
	bidiff += " " + bidiff_options;

	if (!in_suppression_path.empty())
	  bidiff += " --suppressions " + in_suppression_path;

	cmd = bidiff + " " + in_elfv0_path + " " + in_elfv1_path;
	cmd += " > " + out_diff_report_path;

	bool bidiff_ok = true;
	if (system(cmd.c_str()))
	  bidiff_ok = false;

	if (bidiff_ok)
	  {
	    cmd = "diff -u " + ref_diff_report_path
	      + " " + out_diff_report_path;
	    if (system(cmd.c_str()))
	      is_ok = false;
	  }
	else
	  is_ok = false;
      }

    return !is_ok;
}
