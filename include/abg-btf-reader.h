// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// -*- Mode: C++ -*-
//
// Copyright (C) 2022-2026 Red Hat, Inc.
//
// Author: Dodji Seketeli

/// @file
///
/// This file contains the declarations of the front-end to analyze the
/// BTF information contained in an ELF file.

#ifndef __ABG_BTF_READER_H__
#define __ABG_BTF_READER_H__

#include "abg-elf-based-reader.h"

namespace abigail
{

/// Namespace of the reader for the BTF debug information.
namespace btf
{
  elf_based_reader_sptr
  create_reader(const std::string& elf_path,
		const vector<string>& debug_info_root_paths,
		environment& env,
		const fe_iface::options_type& options);

  elf_based_reader_sptr
  create_reader(const std::string& elf_path,
		const vector<string>& debug_info_root_paths,
		environment& env);

}//end namespace btf
}//end namespace abigail

#endif //__ABG_BTF_READER_H__
