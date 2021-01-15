// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// -*- Mode: C++ -*-
//
// Copyright (C) 2020 Google, Inc.
//
// Author: Matthias Maennich

/// @file
///
/// This program tests libabigail's symtab reader.

#include "lib/catch.hpp"

#include "abg-symtab-reader.h"

namespace abigail
{

using symtab_reader::symtab_filter;
using symtab_reader::symtab_filter_builder;

TEST_CASE("default symtab_filter matches anything",
	  "[symtab_reader, symtab_filter]")
{
  const symtab_filter	  filter;
  const elf_symbol_sptr symbol; // not initialized!
  CHECK(filter.matches(symbol));
}

TEST_CASE("default symtab_filter built with filter_builder matches anything",
	  "[symtab_reader, symtab_filter, symtab_filter_builder]")
{
  const symtab_filter filter = symtab_filter_builder();
  const elf_symbol_sptr symbol; // not initialized!
  CHECK(filter.matches(symbol));
}

} // namespace abigail
