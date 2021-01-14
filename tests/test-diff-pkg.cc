// -*- Mode: C++ -*-
//
// Copyright (C) 2013-2015 Red Hat, Inc.
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

// Author: Sinny Kumari

/// @file
///
/// This test harness program fetch ABI diff between ELF binaries present inside
/// input packages with optional debuginfo packages.
/// Resulting ABI diff report is compared with reference one.
///
/// The set of input files and reference reports to consider should be
/// present in the source distribution.

// For package configuration macros.
#include "config.h"
#include <cstring>
#include <string>
#include <cstdlib>
#include <iostream>
#include "test-utils.h"
#include "abg-tools-utils.h"

using std::string;
using std::cerr;
using abigail::tests::get_src_dir;

struct InOutSpec
{
  const char* first_in_package_path;
  const char* second_in_package_path;
  const char* prog_options;
  const char* suppression_path;
  const char* first_in_debug_package_path;
  const char* second_in_debug_package_path;
  const char* ref_report_path;
  const char* out_report_path;
};// end struct InOutSpec

static InOutSpec in_out_specs[] =
{
  // dir1 contains a suppr spec - it should be ignored.
  {
    "data/test-diff-pkg/dirpkg-0-dir1",
    "data/test-diff-pkg/dirpkg-0-dir2",
    "",
    "",
    "",
    "",
    "data/test-diff-pkg/dirpkg-0-report-0.txt",
    "output/test-diff-pkg/dirpkg-0-report-0.txt"
  },
  // dir2 contains a suppr spec - it should be recognized.
  {
    "data/test-diff-pkg/dirpkg-1-dir1",
    "data/test-diff-pkg/dirpkg-1-dir2",
    "",
    "",
    "",
    "",
    "data/test-diff-pkg/dirpkg-1-report-0.txt",
    "output/test-diff-pkg/dirpkg-1-report-0.txt"
  },
  // dir2 contains a suppr spec but --no-abignore is specified,
  // the file should be ignored.
  {
    "data/test-diff-pkg/dirpkg-1-dir1",
    "data/test-diff-pkg/dirpkg-1-dir2",
    "--no-abignore",
    "",
    "",
    "",
    "data/test-diff-pkg/dirpkg-1-report-1.txt",
    "output/test-diff-pkg/dirpkg-1-report-1.txt"
  },
  // dir2 contains several suppr spec files, ".abignore" and
  // "dir.abignore", so the specs should be merged.
  {
    "data/test-diff-pkg/dirpkg-2-dir1",
    "data/test-diff-pkg/dirpkg-2-dir2",
    "",
    "",
    "",
    "",
    "data/test-diff-pkg/dirpkg-2-report-0.txt",
    "output/test-diff-pkg/dirpkg-2-report-0.txt"
  },
  // dir2 contains a suppr spec file, ".abignore" and
  // an additional suppr file is specified on the command line,
  // so the specs should be merged.
  {
    "data/test-diff-pkg/dirpkg-3-dir1",
    "data/test-diff-pkg/dirpkg-3-dir2",
    "",
    "data/test-diff-pkg/dirpkg-3.suppr",
    "",
    "",
    "data/test-diff-pkg/dirpkg-3-report-0.txt",
    "output/test-diff-pkg/dirpkg-3-report-0.txt"
  },
  // dir2 contains a suppr spec file, ".abignore", which should
  // be ignored because of the program options  and
  // an additional suppr file is specified on the command line,
  // which should be recognized.
  {
    "data/test-diff-pkg/dirpkg-3-dir1",
    "data/test-diff-pkg/dirpkg-3-dir2",
    "--no-abignore",
    "data/test-diff-pkg/dirpkg-3.suppr",
    "",
    "",
    "data/test-diff-pkg/dirpkg-3-report-1.txt",
    "output/test-diff-pkg/dirpkg-3-report-1.txt"
  },

#if WITH_TAR
  {
    "data/test-diff-pkg/tarpkg-0-dir1.tar",
    "data/test-diff-pkg/tarpkg-0-dir2.tar",
    "",
    "",
    "",
    "",
    "data/test-diff-pkg/tarpkg-0-report-0.txt",
    "output/test-diff-pkg/tarpkg-0-report-0.txt"
  },
  {
    "data/test-diff-pkg/tarpkg-0-dir1.ta",
    "data/test-diff-pkg/tarpkg-0-dir2.ta",
    "",
    "",
    "",
    "",
    "data/test-diff-pkg/tarpkg-0-report-0.txt",
    "output/test-diff-pkg/tarpkg-0-report-0.txt"
  },
  {
    "data/test-diff-pkg/tarpkg-0-dir1.tar.gz",
    "data/test-diff-pkg/tarpkg-0-dir2.tar.gz",
    "",
    "",
    "",
    "",
    "data/test-diff-pkg/tarpkg-0-report-0.txt",
    "output/test-diff-pkg/tarpkg-0-report-0.txt"
  },
  {
    "data/test-diff-pkg/tarpkg-0-dir1.tar.bz2",
    "data/test-diff-pkg/tarpkg-0-dir2.tar.bz2",
    "",
    "",
    "",
    "",
    "data/test-diff-pkg/tarpkg-0-report-0.txt",
    "output/test-diff-pkg/tarpkg-0-report-0.txt"
  },
#endif //WITH_TAR

#ifdef WITH_RPM
  // Two RPM packages with debuginfo available and have ABI changes
  {
    "data/test-diff-pkg/dbus-glib-0.80-3.fc12.x86_64.rpm",
    "data/test-diff-pkg/dbus-glib-0.104-3.fc23.x86_64.rpm",
    "",
    "",
    "data/test-diff-pkg/dbus-glib-debuginfo-0.80-3.fc12.x86_64.rpm",
    "data/test-diff-pkg/dbus-glib-debuginfo-0.104-3.fc23.x86_64.rpm",
    "data/test-diff-pkg/test-rpm-report-0.txt",
    "output/test-diff-pkg/test-rpm-report-0.txt"
  },
  // Two RPM packages with 2nd package debuginfo missing
  {
  "data/test-diff-pkg/dbus-glib-0.80-3.fc12.x86_64.rpm",
  "data/test-diff-pkg/dbus-glib-0.104-3.fc23.x86_64.rpm",
  "",
  "",
  "data/test-diff-pkg/dbus-glib-debuginfo-0.80-3.fc12.x86_64.rpm",
  "",
  "data/test-diff-pkg/test-rpm-report-1.txt",
  "output/test-diff-pkg/test-rpm-report-1.txt"
  },

  // Two RPM packages with first package debuginfo missing
  {
  "data/test-diff-pkg/dbus-glib-0.80-3.fc12.x86_64.rpm",
  "data/test-diff-pkg/dbus-glib-0.104-3.fc23.x86_64.rpm",
  "",
  "",
  "",
  "data/test-diff-pkg/dbus-glib-debuginfo-0.104-3.fc23.x86_64.rpm",
  "data/test-diff-pkg/test-rpm-report-2.txt",
  "output/test-diff-pkg/test-rpm-report-2.txt"
  },

  // Two RPM packages with missing debuginfo
  {
  "data/test-diff-pkg/dbus-glib-0.80-3.fc12.x86_64.rpm",
  "data/test-diff-pkg/dbus-glib-0.104-3.fc23.x86_64.rpm",
  "",
  "",
  "",
  "",
  "data/test-diff-pkg/test-rpm-report-3.txt",
  "output/test-diff-pkg/test-rpm-report-3.txt"
  },

  // Two RPM packages with no ABI change
  {
  "data/test-diff-pkg/dbus-glib-0.80-3.fc12.x86_64.rpm",
  "data/test-diff-pkg/dbus-glib-0.80-3.fc12.x86_64.rpm",
  "",
  "",
  "data/test-diff-pkg/dbus-glib-debuginfo-0.80-3.fc12.x86_64.rpm",
  "data/test-diff-pkg/dbus-glib-debuginfo-0.80-3.fc12.x86_64.rpm",
  "data/test-diff-pkg/test-rpm-report-4.txt",
  "output/test-diff-pkg/test-rpm-report-4.txt"
  },
  // Two RPM packages with debuginfo available and we don't want to
  // see added symbols.
  {
    "data/test-diff-pkg/dbus-glib-0.80-3.fc12.x86_64.rpm",
    "data/test-diff-pkg/dbus-glib-0.104-3.fc23.x86_64.rpm",
    "--no-added-syms",
    "",
    "data/test-diff-pkg/dbus-glib-debuginfo-0.80-3.fc12.x86_64.rpm",
    "data/test-diff-pkg/dbus-glib-debuginfo-0.104-3.fc23.x86_64.rpm",
    "data/test-diff-pkg/test-rpm-report-5.txt",
    "output/test-diff-pkg/test-rpm-report-5.txt"
  },
#endif //WITH_RPM

#ifdef WITH_DEB
  // Two debian packages.
  {
    "data/test-diff-pkg/libsigc++-2.0-0c2a_2.4.0-1_amd64.deb",
    "data/test-diff-pkg/libsigc++-2.0-0v5_2.4.1-1ubuntu2_amd64.deb",
    "--fail-no-dbg",
    "",
    "data/test-diff-pkg/libsigc++-2.0-0c2a-dbgsym_2.4.0-1_amd64.ddeb",
    "data/test-diff-pkg/libsigc++-2.0-0v5-dbgsym_2.4.1-1ubuntu2_amd64.ddeb",
    "data/test-diff-pkg/libsigc++-2.0-0c2a_2.4.0-1_amd64--libsigc++-2.0-0v5_2.4.1-1ubuntu2_amd64-report-0.txt",
    "output/test-diff-pkg/libsigc++-2.0-0c2a_2.4.0-1_amd64--libsigc++-2.0-0v5_2.4.1-1ubuntu2_amd64-report-0.txt"
  },
#endif // WITH_DEB
  // This should be the last entry.
  {0, 0, 0, 0, 0, 0, 0, 0}
};

int
main()
{
  using abigail::tests::get_build_dir;
  using abigail::tools_utils::ensure_parent_dir_created;

  bool is_ok = true;
  string first_in_package_path, second_in_package_path,
    prog_options,
    ref_abi_diff_report_path, out_abi_diff_report_path, cmd, abipkgdiff,
    first_in_debug_package_path, second_in_debug_package_path,
    suppression_path;
  for (InOutSpec *s = in_out_specs; s->first_in_package_path; ++s)
    {
      first_in_package_path =
        get_src_dir() + "/tests/" + s->first_in_package_path;
      second_in_package_path =
        get_src_dir() + "/tests/" + s->second_in_package_path;

      prog_options = s->prog_options;

      if (s->first_in_debug_package_path
          && strcmp(s->first_in_debug_package_path, ""))
        first_in_debug_package_path =
          get_src_dir() + "/tests/" + s->first_in_debug_package_path;
      else
        first_in_debug_package_path.clear();

      if (s->second_in_debug_package_path
          && strcmp(s->second_in_debug_package_path, ""))
        second_in_debug_package_path =
          get_src_dir() + "/tests/" + s->second_in_debug_package_path;
      else
        second_in_debug_package_path.clear();

      if (s->suppression_path
          && strcmp(s->suppression_path, ""))
        suppression_path =
          get_src_dir() + "/tests/" + s->suppression_path;
      else
        suppression_path.clear();

      ref_abi_diff_report_path = get_src_dir() + "/tests/" + s->ref_report_path;
      out_abi_diff_report_path =
        get_build_dir() + "/tests/" + s->out_report_path;

      if (!ensure_parent_dir_created(out_abi_diff_report_path))
        {
          cerr << "could not create parent directory for "
               << out_abi_diff_report_path;
          is_ok = false;
          continue;
        }

      abipkgdiff = get_build_dir() + "/tools/abipkgdiff";

      if (!prog_options.empty())
	abipkgdiff +=  " " + prog_options;

      if (!first_in_debug_package_path.empty())
        abipkgdiff += " --d1 " + first_in_debug_package_path;
      if (!second_in_debug_package_path.empty())
        abipkgdiff += " --d2 " + second_in_debug_package_path;

      if (!suppression_path.empty())
        abipkgdiff += " --suppressions " + suppression_path;

      cmd =
        abipkgdiff + " " + first_in_package_path + " " + second_in_package_path;
      cmd += " > " + out_abi_diff_report_path;

      bool abipkgdiff_ok = true;
      if (system(cmd.c_str()) & 255)
        abipkgdiff_ok = false;

      if (abipkgdiff_ok)
        {
          cmd = "diff -u " + ref_abi_diff_report_path + " "
            + out_abi_diff_report_path;
          if (system(cmd.c_str()))
            is_ok = false;
        }
      else
        is_ok = false;

    }
    return !is_ok;
}
