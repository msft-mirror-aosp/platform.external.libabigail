// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// -*- mode: C++ -*-
//
// Copyright (C) 2013-2026 Red Hat, Inc.

/// @file

#include "config.h"

#include <algorithm>
#include <cassert>
#include <cstdio>
#include <cstring>
#include <stdexcept>
#include <unordered_map>
#include <set>
#include <memory>
#include <fstream>
#include <sstream>
#include "abg-internal.h"

// <headers defining libabigail's API go under here>
ABG_BEGIN_EXPORT_DECLARATIONS

#include "abg-corpus.h"
#include "abg-ir.h"
#include "abg-reader.h"
#include "abg-sptr-utils.h"
#include "abg-symtab-reader.h"
#include "abg-tools-utils.h"
#include "abg-writer.h"

ABG_END_EXPORT_DECLARATIONS
// </headers defining libabigail's API>

#include "abg-corpus-priv.h"
#include "abg-ir-priv.h"

namespace abigail
{

namespace ir
{

using std::ostringstream;
using std::unordered_map;
using std::list;
using std::vector;

using regex::regex_t_sptr;

/// Constructor of @ref corpus::exported_decls_builder.
///
/// @param fns a reference to the vector of exported functions.
///
/// @param vars a reference to the vector of exported variables.
///
/// @param fns_suppress_regexps the regular expressions that designate
/// the functions to suppress from the exported functions set.
///
/// @param vars_suppress_regexps the regular expressions that designate
/// the variables to suppress from the exported variables set.
///
/// @param fns_keep_regexps the regular expressions that designate the
/// functions to keep in the exported functions set.
///
/// @param fns_keep_regexps the regular expressions that designate the
/// functions to keep in the exported functions set.
///
/// @param vars_keep_regexps the regular expressions that designate
/// the variables to keep in the exported variables set.
///
/// @param sym_id_of_fns_to_keep the IDs of the functions to keep in
/// the exported functions set.
///
/// @param sym_id_of_vars_to_keep the IDs of the variables to keep in
/// the exported variables set.
corpus::exported_decls_builder
::exported_decls_builder(functions&	fns,
			 variables&	vars,
			 strings_type&	fns_suppress_regexps,
			 strings_type&	vars_suppress_regexps,
			 strings_type&	fns_keep_regexps,
			 strings_type&	vars_keep_regexps,
			 strings_type&	sym_id_of_fns_to_keep,
			 strings_type&	sym_id_of_vars_to_keep)
  : priv_(new priv(fns, vars,
		   fns_suppress_regexps,
		   vars_suppress_regexps,
		   fns_keep_regexps,
		   vars_keep_regexps,
		   sym_id_of_fns_to_keep,
		   sym_id_of_vars_to_keep))
{
}

/// Getter for the reference to the vector of exported functions.
/// This vector is shared with with the @ref corpus.  It's where the
/// set of exported function is ultimately stored.
///
/// @return a reference to the vector of exported functions.
const corpus::functions&
corpus::exported_decls_builder::exported_functions() const
{return priv_->fns_;}

/// Test if a given function ID maps to several functions in the same corpus.
///
/// The magic of ELF symbol aliases makes it possible for an ELF
/// symbol alias to designate several different functions.  This
/// function tests if the ELF symbol of a given function has a aliases
/// that designates another function or not.
///
/// @param fn the function to consider.
///
/// @return the set of functions designated by the ELF symbol of @p
/// fn, or nullptr if the function ID maps to just @p fn.
std::unordered_set<const function_decl*>*
corpus::exported_decls_builder::fn_id_maps_to_several_fns(const function_decl* fn)
{
  std::unordered_set<const function_decl*> *fns_for_id =
    priv_->fn_id_is_in_id_fns_map(fn);
  if (fns_for_id && fns_for_id->size() > 1)
    return fns_for_id;

  return nullptr;
}

/// Getter for the reference to the vector of exported variables.
/// This vector is shared with with the @ref corpus.  It's where the
/// set of exported variable is ultimately stored.
///
/// @return a reference to the vector of exported variables.
const corpus::variables&
corpus::exported_decls_builder::exported_variables() const
{return priv_->vars_;}

/// Consider at all the tunables that control wether a function should
/// be added to the set of exported function and if it fits in, add
/// the function to that set.
///
/// @param fn the function to add the set of exported functions.
///
/// @param do_update if true, add the function to the set of exported
/// functions even if one already exists there.  Otherwise, if a
/// function's ID is already recorded as being exported, do not add @p
/// fn.
///
/// @return true iff the function was added to the set of exported
/// functions.
bool
corpus::exported_decls_builder::maybe_add_fn_to_exported_fns(function_decl* fn,
							     bool do_update)
{
  if (!fn->get_is_in_public_symbol_table())
    return false;

  const string& fn_id = priv_->get_id(*fn);
  ABG_ASSERT(!fn_id.empty());

  if (!do_update && priv_->fn_id_is_in_id_fns_map(fn))
    return false;

  {
    if (priv_->keep_wrt_id_of_fns_to_keep(fn)
	&& priv_->keep_wrt_regex_of_fns_to_suppress(fn)
	&& priv_->keep_wrt_regex_of_fns_to_keep(fn))
      {
	priv_->add_fn_to_exported(fn, do_update);
	return true;
      }
  }
  return false;
}

/// Consider at all the tunables that control wether a variable should
/// be added to the set of exported variable and if it fits in, add
/// the variable to that set.
///
/// @param fn the variable to add the set of exported variables.
///
/// @return true iff the variable was added to the set of exported
/// variables.
bool
corpus::exported_decls_builder::maybe_add_var_to_exported_vars(const var_decl_sptr& var)
{
  if (!var->get_is_in_public_symbol_table())
    return false;

  const interned_string& var_id = priv_->get_id(*var);
  ABG_ASSERT(!var_id.empty());

  if (priv_->var_is_in_id_vars_map(var))
    return false;

  {
    if (priv_->keep_wrt_id_of_vars_to_keep(var)
	&& priv_->keep_wrt_regex_of_vars_to_suppress(var)
	&& priv_->keep_wrt_regex_of_vars_to_keep(var))
      {
	priv_->add_var_to_exported(var);
	return true;
      }
  }
  return false;
}

// </corpus::exported_decls_builder>

/// Convenience typedef for a hash map of pointer to function_decl and
/// boolean.
typedef unordered_map<const function_decl*,
		      bool,
		      function_decl::hash,
		      function_decl::ptr_equal> fn_ptr_map_type;

/// Convenience typedef for a hash map of string and pointer to
/// function_decl.
typedef unordered_map<string, const function_decl*> str_fn_ptr_map_type;

/// Convenience typedef for a hash map of pointer to var_decl and boolean.
typedef unordered_map<const var_decl*,
		      bool,
		      var_decl::hash,
		      var_decl::ptr_equal> var_ptr_map_type;

/// This is a comparison functor for comparing pointers to @ref
/// function_decl.
struct func_comp
{
  /// The comparisong operator for pointers to @ref function_decl.  It
  /// performs a string comparison of the mangled names of the
  /// functions.  If the functions don't have mangled names, it
  /// compares their names instead.
  ///
  /// @param first the first function to consider in the comparison.
  ///
  /// @param second the second function to consider in the comparison.
  ///
  /// @return true if the (mangled) name of the first function is less
  /// than the (mangled)name of the second one, false otherwise.
  bool
  operator()(const function_decl* first,
	     const function_decl* second) const
  {
    ABG_ASSERT(first != 0 && second != 0);

    string first_name, second_name;
    first_name = first->get_linkage_name();
    if (first_name.empty())
      first_name = first->get_name();
    ABG_ASSERT(!first_name.empty());

    second_name = second->get_linkage_name();
    if (second_name.empty())
      second_name = second->get_name();
    ABG_ASSERT(!second_name.empty());

    if (first_name != second_name)
      return first_name < second_name;

    // If the functions are member functions, sort them using the hash
    // value and canonical index of their class.
    if (is_member_function(first) && is_member_function(second))
      {
	class_or_union_sptr first_scope =
	  is_class_or_union_type(first->get_scope());
	class_or_union_sptr second_scope =
	  is_class_or_union_type(second->get_scope());
	type_base* fc = first_scope
	  ? first_scope->get_naked_canonical_type()
	  : nullptr;
	type_base* sc = second_scope
	  ? second_scope->get_naked_canonical_type()
	  : nullptr;
	if (fc && sc)
	    {
	      hash_t fh = peek_hash_value(*fc);
	      hash_t sh = peek_hash_value(*sc);
	      if (fh && sh && *fh != *sh)
		return *fh < *sh;

	      size_t f_cti = get_canonical_type_index(fc);
	      size_t s_cti = get_canonical_type_index(sc);
	      if (f_cti != s_cti)
		return f_cti < s_cti;
	    }
      }

    return first_name < second_name;
  }
};

/// This is a comparison functor for comparing pointers to @ref
/// var_decl.
struct var_comp
{
  /// The comparison operator for pointers to @ref var_decl.
  ///
  /// It perform a string comparison on the names of the variables.
  ///
  /// @param first the first variable to consider for the comparison.
  ///
  /// @param second the second variable to consider for the comparison.
  ///
  /// @return true if first is less than second, false otherwise.
  bool
  operator()(const var_decl* first,
	     const var_decl* second) const
  {
    ABG_ASSERT(first != 0 && second != 0);

    string first_name, second_name;
    first_name = first->get_linkage_name();
    if (first_name.empty())
      {
	first_name = first->get_pretty_representation();
	second_name = second->get_pretty_representation();
	ABG_ASSERT(!second_name.empty());
      }
    ABG_ASSERT(!first_name.empty());

    if (second_name.empty())
      second_name = second->get_linkage_name();

    if (second_name.empty())
      {
	second_name = second->get_pretty_representation();
	first_name = first->get_pretty_representation();
	ABG_ASSERT(!first_name.empty());
      }
    ABG_ASSERT(!second_name.empty());

    return first_name < second_name;
  }

  bool
  operator()(const var_decl_sptr& first,
	     const var_decl_sptr& second) const
  {return operator()(first.get(), second.get());}
};

/// A comparison functor to compare elf_symbols for the purpose of
/// sorting.
struct comp_elf_symbols_functor
{
  bool
  operator()(const elf_symbol& l,
	     const elf_symbol& r) const
  {return l.get_id_string() < r.get_id_string();}

  bool
  operator()(const elf_symbol_sptr l,
	     const elf_symbol_sptr r) const
  {return operator()(*l, *r);}
}; // end struct comp_elf_symbols_functor


// <corpus stuff>

/// Get the maps that associate a name to a certain kind of type.
type_maps&
corpus::priv::get_types()
{return types_;}

/// Get the maps that associate a name to a certain kind of type.
const type_maps&
corpus::priv::get_types() const
{return types_;}

/// Return a sorted vector of function symbols for this corpus.
///
/// Note that the first time this function is called, the symbols are
/// sorted and cached.  Subsequent invocations of this function return
/// the cached vector that was built previously.
///
/// @return the sorted list of function symbols.
const elf_symbols&
corpus::priv::get_sorted_fun_symbols() const
{
  if (!sorted_fun_symbols)
    {
      if (symtab_)
	{
	  auto filter = symtab_->make_filter();
	  filter.set_functions();
	  sorted_fun_symbols = elf_symbols(symtab_->begin(filter),
					   symtab_->end());
	}
      else
	sorted_fun_symbols = elf_symbols();
    }
  return *sorted_fun_symbols;
}

/// Return a map from name to function symbol for this corpus.
///
/// Note that the first time this function is called, the map is built.
/// Subsequent invocations of this function return the cached map that was
/// built previously.
///
/// @return the name function symbol map
const string_elf_symbols_map_type&
corpus::priv::get_fun_symbol_map() const
{
  if (!fun_symbol_map)
    {
      fun_symbol_map = string_elf_symbols_map_type();
      for (const auto& symbol : get_sorted_fun_symbols())
	{
	  (*fun_symbol_map)[symbol->get_name()].push_back(symbol);
	  (*fun_symbol_map)[symbol->get_id_string()].push_back(symbol);
	}
    }
  return *fun_symbol_map;
}

/// Getter for a sorted vector of the function symbols undefined in
/// this corpus.
///
/// @return a vector of the function symbols undefined in this corpus,
/// sorted by name and then version.
const elf_symbols&
corpus::priv::get_sorted_undefined_fun_symbols() const
{
  if (!sorted_undefined_fun_symbols)
    {
      if (symtab_)
	{
	  auto filter = symtab_->make_filter();
	  filter.set_functions();
	  filter.set_undefined_symbols();
	  filter.set_public_symbols(false);

	  sorted_undefined_fun_symbols =
	    elf_symbols(symtab_->begin(filter), symtab_->end());
	}
      else
	sorted_undefined_fun_symbols = elf_symbols();
    }
  return *sorted_undefined_fun_symbols;
}

/// Return a map from name to undefined function symbol for this corpus.
///
/// Note that the first time this function is called, the map is built.
/// Subsequent invocations of this function return the cached map that was
/// built previously.
///
/// @return the name function symbol map for undefined symbols
const string_elf_symbols_map_type&
corpus::priv::get_undefined_fun_symbol_map() const
{
  if (!undefined_fun_symbol_map)
    {
      undefined_fun_symbol_map = string_elf_symbols_map_type();
      for (const auto& symbol : get_sorted_undefined_fun_symbols())
	{
	  (*undefined_fun_symbol_map)[symbol->get_name()].push_back(symbol);
	  (*undefined_fun_symbol_map)[symbol->get_id_string()].push_back(symbol);
	}
    }
  return *undefined_fun_symbol_map;
}

/// Return a list of symbols that are not referenced by any function of
/// corpus::get_functions().
///
/// Note that this function considers the list of function symbols to keep,
/// that is provided by corpus::get_sym_ids_of_fns_to_keep(). If a given
/// unreferenced function symbol is not in the list of functions to keep, then
/// that symbol is dropped and will not be part of the resulting table of
/// unreferenced symbol that is built.
///
/// @return list of symbols that are not referenced by any function
const elf_symbols&
corpus::priv::get_unreferenced_function_symbols() const
{
  if (!unrefed_fun_symbols)
    {
      unrefed_fun_symbols = elf_symbols();
      if (symtab_)
	{
	  unordered_map<string, bool> refed_funs;

	  for (const auto& function : fns)
	    if (elf_symbol_sptr sym = function->get_symbol())
	      {
		refed_funs[sym->get_id_string()] = true;
		for (elf_symbol_sptr a = sym->get_next_alias();
		     a && !a->is_main_symbol(); a = a->get_next_alias())
		  refed_funs[a->get_id_string()] = true;
	      }

	  auto filter = symtab_->make_filter();
	  filter.set_functions();
	  for (const auto& symbol :
	       symtab_reader::filtered_symtab(*symtab_, filter))
	    {
	      const std::string sym_id = symbol->get_id_string();
	      if (refed_funs.find(sym_id) == refed_funs.end())
		{
		  bool keep = sym_id_fns_to_keep.empty();
		  for (const auto& id : sym_id_fns_to_keep)
		    {
		      if (id == sym_id)
			{
			  keep = true;
			  break;
			}
		    }
		  if (keep)
		    unrefed_fun_symbols->push_back(symbol);
		}
	    }
	}
    }
  return *unrefed_fun_symbols;
}

/// Getter for the sorted vector of variable symbols for this corpus.
///
/// Note that the first time this function is called, it computes the
/// sorted vector, caches the result and returns it.  Subsequent
/// invocations of this function just return the cached vector.
///
/// @return the sorted vector of variable symbols for this corpus.
const elf_symbols&
corpus::priv::get_sorted_var_symbols() const
{
  if (!sorted_var_symbols)
    {
      if (symtab_)
	{
	  auto filter = symtab_->make_filter();
	  filter.set_variables();

	  sorted_var_symbols = elf_symbols(symtab_->begin(filter),
					   symtab_->end());
	}
      else
	sorted_var_symbols = elf_symbols();
    }
  return *sorted_var_symbols;
}

/// Return a map from name to variable symbol for this corpus.
///
/// Note that the first time this function is called, the map is built.
/// Subsequent invocations of this function return the cached map that was
/// built previously.
///
/// @return the name variable symbol map
const string_elf_symbols_map_type&
corpus::priv::get_var_symbol_map() const
{
  if (!var_symbol_map)
    {
      var_symbol_map = string_elf_symbols_map_type();
      for (const auto& symbol : get_sorted_var_symbols())
	(*var_symbol_map)[symbol->get_name()].push_back(symbol);
    }
  return *var_symbol_map;
}

/// Getter for a sorted vector of the variable symbols undefined in
/// this corpus.
///
/// @return a vector of the variable symbols undefined in this corpus,
/// sorted by name and then version.
const elf_symbols&
corpus::priv::get_sorted_undefined_var_symbols() const
{
  if (!sorted_undefined_var_symbols)
    {
      if (symtab_)
	{
	  auto filter = symtab_->make_filter();
	  filter.set_variables();
	  filter.set_undefined_symbols();
	  filter.set_public_symbols(false);

	  sorted_undefined_var_symbols =
	    elf_symbols(symtab_->begin(filter), symtab_->end());
	}
      else
	sorted_undefined_var_symbols = elf_symbols();
    }
  return *sorted_undefined_var_symbols;
}

/// Return a map from name to undefined variable symbol for this corpus.
///
/// Note that the first time this function is called, the map is built.
/// Subsequent invocations of this function return the cached map that was
/// built previously.
///
/// @return the name undefined variable symbol map
const string_elf_symbols_map_type&
corpus::priv::get_undefined_var_symbol_map() const
{
  if (!undefined_var_symbol_map)
    {
      undefined_var_symbol_map = string_elf_symbols_map_type();
      for (const auto& symbol : get_sorted_undefined_var_symbols())
	(*undefined_var_symbol_map)[symbol->get_name()].push_back(symbol);
    }
  return *undefined_var_symbol_map;
}

/// Return a list of symbols that are not referenced by any variable of
/// corpus::get_variables().
///
/// Note that this function considers the list of variable symbols to keep,
/// that is provided by corpus::get_sym_ids_of_vars_to_keep(). If a given
/// unreferenced variable symbol is not in the list of variable to keep, then
/// that symbol is dropped and will not be part of the resulting table of
/// unreferenced symbol that is built.
///
/// @return list of symbols that are not referenced by any variable
const elf_symbols&
corpus::priv::get_unreferenced_variable_symbols() const
{
  if (!unrefed_var_symbols)
    {
      unrefed_var_symbols = elf_symbols();
      if (symtab_)
	{
	  unordered_map<string, bool> refed_vars;
	  for (const auto& variable : vars)
	    if (elf_symbol_sptr sym = variable->get_symbol())
	      {
		refed_vars[sym->get_id_string()] = true;
		for (elf_symbol_sptr a = sym->get_next_alias();
		     a && !a->is_main_symbol(); a = a->get_next_alias())
		  refed_vars[a->get_id_string()] = true;
	      }

	  auto filter = symtab_->make_filter();
	  filter.set_variables();
	  for (const auto& symbol :
	       symtab_reader::filtered_symtab(*symtab_, filter))
	    {
	      const std::string sym_id = symbol->get_id_string();
	      if (refed_vars.find(sym_id) == refed_vars.end())
		{
		  bool keep = sym_id_vars_to_keep.empty();
		  for (const auto& id : sym_id_vars_to_keep)
		    {
		      if (id == sym_id)
			{
			  keep = true;
			  break;
			}
		    }
		  if (keep)
		    unrefed_var_symbols->push_back(symbol);
		}
	    }
	}
    }
  return *unrefed_var_symbols;
}


/// Lookup the function which has a given function ID.
///
/// Note that there can have been several functions with the same ID.
/// This is because debug info can declare the same function in
/// several different translation units.  Normally, all these function
/// should be equal.  But still, this function returns all these
/// functions.
///
/// @param id the ID of the function to lookup.  This ID must be
/// either the result of invoking function::get_id() of
/// elf_symbol::get_id_string().
///
/// @return the set of functions which ID is @p id, or nil if no
/// function with that ID was found.
const std::unordered_set<const function_decl*>*
corpus::priv::lookup_functions(const interned_string& id)
{
  exported_decls_builder_sptr &b = exported_decls_builder;
  if (b)
    {
      auto i = b->priv_->id_fns_map_.find(id);
      if (i == b->priv_->id_fns_map_.end())
	return 0;
      return &i->second;
    }
  return nullptr;
}

/// Get the member function variant that belongs to the canonical type
/// of the containing type.
///
/// If the function we are looking at has no containing type, then
/// return the same function.
///
/// @param fn the function to consider.
///
/// @return the canonical function for @p fn.
static const function_decl*
get_canonical_function(const function_decl *fn)
{
  if (!fn)
    return nullptr;

  const function_decl* canonical_function = fn;
  class_or_union_sptr scope = nullptr, canonical_scope = nullptr;
  scope = is_class_or_union_type(fn->get_scope());

  if (scope)
    {
      scope = look_through_decl_only_class(scope);
      canonical_scope = is_class_or_union_type(scope->get_canonical_type());

      if (// When the canonicalized type and the canonical type are
	  // from the same ABI corpus, no problem, do the thing for
	  // all functions.  The set of member functions of the
	  // canonical type represents the union of the member
	  // functions of all it canonicalized peer types.  So the
	  // member function that is on the canonical type is the
	  // "right one" that is going to be serialized into ABIXML in
	  // the end.
	  (canonical_scope && canonical_scope->get_corpus() == scope->get_corpus())
	  // But when the two types come from different ABI corpora,
	  // then, only consider virtual member functions because
	  // these are the only one that we know are equivalent
	  // because they are taken into account during the c14n
	  // process.  Non-virtual members are not taken into account
	  // by the c14n so they might be different.
	  || get_member_function_is_virtual(is_method_decl(fn)))
	{
	  interned_string ln = fn->get_linkage_name();
	  ABG_ASSERT(!ln.empty());
	  canonical_function =
	    canonical_scope->find_member_function(ln);
	  if (!canonical_function)
	    canonical_function = fn;
	}
    }

  return canonical_function;
}

/// Remove redundant functions from the "fns" vector.
void
corpus::priv::remove_redundant_functions()
{
  // This set cannot contain two identical function, by construction.
  functions_set_type fns_set;

  lock_guard<recursive_mutex> lock(get_mutex());
  // Perform the de-duplication by filing fns_set with the content of
  // fns.
  for (auto& f : fns)
    fns_set.insert(get_canonical_function(f));

  // Clear the original non-deduplicated functions vector.
  fns.clear();

  // Fill fns back with the de-duplicated set of functions.
  for (auto& f : fns_set)
    fns.push_back(f);

  // Do something similar for the set of functions associated to each
  // function ID.
  exported_decls_builder_sptr &b = exported_decls_builder;
  if (b)
    for (auto& entry : b->priv_->id_fns_map_)
      {
	functions_set_type f_set;
	if (entry.second.size() > 1)
	  {
	    for (auto& fn : entry.second)
	      {
		// Replace each one of the member functions associated
		// with the linkage_name entry.first with the matching
		// member function of the canonical class of its
		// containing class.  That should reduce the number of
		// functions in entry.second to just one, if there is
		// no ODR violation.
		auto canonical_function = get_canonical_function(fn);
		f_set.insert(canonical_function);
	      }

	    entry.second.clear();
	    for (auto& fn : f_set)
	      {
		auto it = fns_set.find(fn);
		if (it != fns_set.end())
		  entry.second.insert(*it);
	      }
	  }
      }
}

/// Set the "compute-non-reachable-types' property of the corpus.
/// When it's set to true, then the debug info reader loads all
/// non-reachable types from the binary.
///
/// @param f the new value of the property.
void
corpus::priv::set_compute_non_reachable_types(bool f)
{do_compute_non_reachable_types_ = f;}

/// Get the "compute-non-reachable-types' property of the corpus.
/// When it's set to true, then the debug info reader loads all
/// non-reachable types from the binary.
///
/// @return f the new value of the property.
bool
corpus::priv::get_compute_non_reachable_types() const
{return do_compute_non_reachable_types_.load();}

/// Getter of the recursive mutex of this corpus.
///
/// @return the recursive mutex associated to this corpus.
recursive_mutex&
corpus::priv::get_mutex()
{return mutex_;}

/// Destructor of the @ref corpus::priv type.
corpus::priv::~priv()
{
}

/// Constructor of the @ref corpus type.
///
/// @param env the environment of the corpus.
///
/// @param path the path to the file containing the ABI corpus.
corpus::corpus(const ir::environment& env, const string& path)
{
  priv_.reset(new priv(path, env));
  init_format_version();
}

corpus::~corpus() = default;

/// Getter of the enviroment of the corpus.
///
/// @return the environment of this corpus.
const environment&
corpus::get_environment() const
{return priv_->env;}

/// Test if logging was requested.
///
/// @return true iff logging was requested.
bool
corpus::do_log() const
{return priv_->do_log;}

/// Request logging, or not.
///
/// @param f true iff logging is requested.
void
corpus::do_log(bool f)
{priv_->do_log = f;}

/// Add a translation unit to the current ABI Corpus.
///
/// Note that two translation units with the same path (as returned by
/// translation_unit::get_path) cannot be added to the same @ref
/// corpus.  If that happens, the library aborts.
///
/// @param tu the new translation unit to add.
void
corpus::add(const translation_unit_sptr& tu)
{
  lock_guard<recursive_mutex> lock(priv_->get_mutex());

  ABG_ASSERT(priv_->members.insert(tu).second);

  // Update the path -> translation_unit map.
  string_tu_map_type::const_iterator i =
    priv_->path_tu_map.find(tu->get_absolute_path());
  ABG_ASSERT(i == priv_->path_tu_map.end());
  priv_->path_tu_map[tu->get_absolute_path()] = tu;
  i = priv_->path_tu_map.find(tu->get_path());
  if (i == priv_->path_tu_map.end())
    priv_->path_tu_map[tu->get_path()] = tu;

  tu->set_corpus(this);
}

/// Return the list of translation units of the current corpus.
///
/// @return the list of translation units of the current corpus.
const translation_units&
corpus::get_translation_units() const
{return priv_->members;}

/// Find the translation unit that has a given path.
///
/// @param path the path of the translation unit to look for.
///
/// @return the translation unit found, if any.  Otherwise, return
/// nil.
const translation_unit_sptr
corpus::find_translation_unit(const string &path) const
{
  lock_guard<recursive_mutex> lock(priv_->get_mutex());
  string_tu_map_type::const_iterator i =
    priv_->path_tu_map.find(path);

  if (i == priv_->path_tu_map.end())
    return translation_unit_sptr();
  return i->second;
}

/// Erase the translation units contained in this in-memory object.
///
/// Note that the on-disk archive file that contains the serialized
/// representation of this object is not modified.
void
corpus::drop_translation_units()
{
  lock_guard<recursive_mutex> lock(priv_->get_mutex());
  priv_->members.clear();
}

/// Get the maps that associate a name to a certain kind of type.
///
/// @return the maps that associate a name to a certain kind of type.
type_maps&
corpus::get_types()
{return priv_->types_;}

/// Get the maps that associate a name to a certain kind of type.
///
/// @return the maps that associate a name to a certain kind of
/// type.
const type_maps&
corpus::get_types() const
{return priv_->types_;}

/// Get the maps that associate a location string to a certain kind of
/// type.
///
/// The location string is the result of the invocation to the
/// function abigail::ir::location::expand().  It has the form
/// "file.c:4:1", with 'file.c' being the file name, '4' being the
/// line number and '1' being the column number.
///
/// @return the maps.
const type_maps&
corpus::get_type_per_loc_map() const
{return priv_->type_per_loc_map_;}

/// Test if the recording of reachable types (and thus, indirectly,
/// the recording of non-reachable types) is activated for the
/// current @ref corpus.
///
/// @return true iff the recording of reachable types is activated for
/// the current @ref corpus.
bool
corpus::recording_types_reachable_from_public_interface_supported()
{
  lock_guard<recursive_mutex> lock(priv_->get_mutex());
  return !priv_->reachable_types_from_pub_ifaces_.empty();
}

/// Record a type as being reachable from public interfaces (global
/// functions and variables).
///
/// @param t the type to record as reachable.
bool
corpus::record_type_as_reachable_from_public_interfaces(const type_base& t)
{
  type_base* e = get_exemplar_type(&t);
  if (type_is_reachable_from_public_interfaces(*e))
    return false;
  {
    lock_guard<recursive_mutex> lock(priv_->get_mutex());
    priv_->reachable_types_from_pub_ifaces_.insert(e);
  }
  return true;
}

void
corpus::remove_type_from_reachable_types(const type_base& t)
{
  lock_guard<recursive_mutex> lock(priv_->get_mutex());
  type_base* examplar = get_exemplar_type(&t);
  priv_->reachable_types_from_pub_ifaces_.erase(examplar);
}

/// Test if a type is reachable from public interfaces (global
/// functions and variables).
///
/// For a type to be considered reachable from public interfaces, it
/// must have been previously marked as such by calling
/// corpus::record_type_as_reachable_from_public_interfaces.
///
/// @param t the type to test for.
///
/// @return true iff @p t is reachable from public interfaces.
bool
corpus::type_is_reachable_from_public_interfaces(const type_base& t) const
{
  if (!priv_->get_compute_non_reachable_types())
    return true;

  lock_guard<recursive_mutex> lock(priv_->get_mutex());
  type_base* examplar = get_exemplar_type(&t);
  auto it = priv_->reachable_types_from_pub_ifaces_.find(examplar);
  if (it == priv_->reachable_types_from_pub_ifaces_.end())
    return false;
  return true;
}

/// Test if a given type is reachable from public interfaces of either
/// the ABI corpus it belongs to, or if necessary,from the public
/// interfaces of the main corpus of the corpus_group the type belongs
/// to.
///
/// @param t the type to consider.
///
/// @return true iff @p t is reachable from public interface of
bool
type_is_reachable_from_public_interfaces(const type_base& t)
{
  const corpus* abi = t.get_corpus();
  if (!abi)
    // If there is no ABI corpus associated, then all types are deemed
    // reachable.
    return true;

  if (!abi->priv_->get_compute_non_reachable_types())
    return true;

  bool result = abi->type_is_reachable_from_public_interfaces(t);

  // If we couldn't determine reachability from the ABI of the type,
  // then, if the type belongs to an ABI corpus that is part of a
  // corpus_group, check reachability from the group.
  if (!result)
    if (const corpus_group* group = abi->get_group())
      if (group->type_is_reachable_from_public_interfaces(t))
	result = true;

  return result;
}

/// Getter of a sorted vector of the types that are *NOT* reachable
/// from public interfaces.
///
/// Note that for this to be non-empty, the libabigail reader that
/// analyzed the input (be it a binary or an abixml file) must have be
/// configured to load types that are not reachable from public
/// interfaces.
///
/// @return a reference to a vector of sorted types NON reachable from
/// public interfaces.
const canonical_type_ptr_set_type&
corpus::get_types_not_reachable_from_public_interfaces() const
{
  if (!priv_->get_compute_non_reachable_types())
    return priv_->non_reachable_types_from_pub_ifaces_;

  lock_guard<recursive_mutex> lock(priv_->get_mutex());
  if (priv_->non_reachable_types_from_pub_ifaces_.empty())
    {
      const type_maps& types = get_types();
      for (auto& type_wptr : types.get_types_sorted())
	{
	  type_base_sptr t(type_wptr);
	  if (auto clazz = is_class_or_union_type(t))
	    t = look_through_decl_only_class(clazz);
	  type_base_sptr examplar_type = get_exemplar_type(t);
	  if (!type_is_reachable_from_public_interfaces(*examplar_type))
	    priv_->non_reachable_types_from_pub_ifaces_.insert(examplar_type.get());
	}
    }

  return priv_->non_reachable_types_from_pub_ifaces_;
}

const type_base_ptrs_type&
corpus::get_sorted_types_not_reachable_from_public_interfaces() const
{
  lock_guard<recursive_mutex> lock(priv_->get_mutex());
  if (priv_->sorted_non_reachable_types_from_pub_ifaces_.empty())
    {
      for (auto& type : get_types_not_reachable_from_public_interfaces())
	priv_->sorted_non_reachable_types_from_pub_ifaces_.push_back(type);

      type_topo_comp comp;
      sort(priv_->sorted_non_reachable_types_from_pub_ifaces_.begin(),
	   priv_->sorted_non_reachable_types_from_pub_ifaces_.end(),
	   comp);
    }
  return priv_->sorted_non_reachable_types_from_pub_ifaces_;
}

/// A pass that markes the types that are reachable from the ABI
/// interfaces.
class reachable_types_marker : public abigail::ir::ir_node_visitor
{
  corpus& corpus_;

public:

  reachable_types_marker() = delete;

  reachable_types_marker(corpus& abi)
    : corpus_(abi)
  {
    allow_visiting_already_visited_type_node(false);
  }

  /// This is a sub-routine of
  /// corpus::record_type_as_reachable_from_public_interfaces.
  ///
  /// This function walks the sub-types of a given type which has been
  /// marked as being reachable from a public interface and marks the
  /// sub-types as reachable as well.
  ///
  /// @param type the which to consider sub-types for.
  ///
  /// @param rec_types the types that have been explored so far.  This
  /// is to detect cycles and avoid endless loops.
  void
  maybe_record_subtypes_as_reachable(type_base* type,
				     std::unordered_set<type_base*>& rec_types)
  {
    if (!type)
      return;

    auto examplar_type = get_exemplar_type(type);
    ABG_ASSERT(examplar_type);

    if (rec_types.find(examplar_type) != rec_types.end())
      return;

    rec_types.insert(examplar_type);

    auto abi = examplar_type->get_corpus();
    ABG_ASSERT(abi);

    if (auto d = is_decl(examplar_type))
      {
	if (auto n = d->get_naming_typedef())
	  abi->record_type_as_reachable_from_public_interfaces(*n);
      }

    if (auto d = is_enum_type(examplar_type))
      {
	if (auto u = d->get_underlying_type())
	  abi->record_type_as_reachable_from_public_interfaces(*u);
      }
    else if (auto typdef = is_typedef(examplar_type))
      {
	if (auto u = typdef->get_underlying_type())
	  if (abi->record_type_as_reachable_from_public_interfaces(*u))
	    maybe_record_subtypes_as_reachable(u.get(), rec_types);
      }
    else if (auto ptr = is_pointer_type(examplar_type))
      {
	if (auto p = ptr->get_pointed_to_type())
	  if (abi->record_type_as_reachable_from_public_interfaces(*p))
	    maybe_record_subtypes_as_reachable(p.get(), rec_types);
      }
    else if (auto ref = is_reference_type(examplar_type))
      {
	if (auto p = ref->get_pointed_to_type())
	  if (abi->record_type_as_reachable_from_public_interfaces(*p))
	    maybe_record_subtypes_as_reachable(p.get(), rec_types);
      }
    else if (auto q = is_qualified_type(examplar_type))
      {
	if (auto u = q->get_underlying_type())
	  if (abi->record_type_as_reachable_from_public_interfaces(*u))
	    maybe_record_subtypes_as_reachable(u.get(), rec_types);
      }
    else if (auto a = is_array_type(examplar_type))
      {
	if (auto e = a->get_element_type())
	  if (abi->record_type_as_reachable_from_public_interfaces(*e))
	    maybe_record_subtypes_as_reachable(e.get(), rec_types);

	for (auto s : a->get_subranges())
	  if (abi->record_type_as_reachable_from_public_interfaces(*s))
	    maybe_record_subtypes_as_reachable(s.get(), rec_types);
      }
    else if (auto fn_type = is_function_type(examplar_type))
      {
	if (auto t = fn_type->get_return_type())
	  if (abi->record_type_as_reachable_from_public_interfaces(*t))
	    maybe_record_subtypes_as_reachable(t.get(), rec_types);

	for (auto parm : fn_type->get_parameters())
	  if (auto t = parm->get_type())
	    if (abi->record_type_as_reachable_from_public_interfaces(*t))
	      maybe_record_subtypes_as_reachable(t.get(), rec_types);
      }
    else if (auto cou = is_class_or_union_type(examplar_type))
      {
	for (auto dm : cou->get_data_members())
	  if (auto t = dm->get_type())
	    if (abi->record_type_as_reachable_from_public_interfaces(*t))
	      maybe_record_subtypes_as_reachable(t.get(), rec_types);

	for (auto mf : cou->get_member_functions())
	  if (auto t = mf->get_type())
	    if (abi->record_type_as_reachable_from_public_interfaces(*t))
	      maybe_record_subtypes_as_reachable(t.get(), rec_types);

	for (auto t : cou->get_sorted_member_types())
	  if (abi->record_type_as_reachable_from_public_interfaces(*t))
	    maybe_record_subtypes_as_reachable(t.get(), rec_types);

	if (auto klass = is_class_type(cou))
	  {
	    for (auto b : klass->get_base_specifiers())
	      if (auto bc = b->get_base_class())
		if (abi->record_type_as_reachable_from_public_interfaces(*bc))
		  maybe_record_subtypes_as_reachable(bc.get(), rec_types);

	    for (auto f : klass->get_virtual_mem_fns())
	      if (auto t = f->get_type())
		if (abi->record_type_as_reachable_from_public_interfaces(*t))
		  maybe_record_subtypes_as_reachable(t.get(), rec_types);
	  }
      }

    rec_types.erase(examplar_type);
  }

  /// The visitor code of the pass.
  ///
  /// @param type the type to mark as reachable.
  ///
  /// @return true, always.
  virtual bool
  visit_begin(type_base* type)
  {
    if (corpus_.record_type_as_reachable_from_public_interfaces(*type))
      {
	std::unordered_set<type_base*> rec_types;
	maybe_record_subtypes_as_reachable(type, rec_types);
      }
    return true;
  }
};// end class non_reachable_type_is_marker

void
corpus::mark_non_reachable_types()
{
  priv_->set_compute_non_reachable_types(true);

  reachable_types_marker walker(*this);

  for (auto function : get_functions())
    if (function)
      {
	const_cast<function_decl*>(function)->traverse(walker);
	if (auto type = is_type(function->get_scope()))
	  {
	    type = type->get_canonical_type();
	    type->traverse(walker);
	  }
      }

  for (auto variable : get_variables())
    if (variable)
      {
	variable->traverse(walker);
	if (auto type = is_type(variable->get_scope()))
	  {
	    type = type->get_canonical_type();
	    type->traverse(walker);
	  }
      }

  if (!get_environment().analyze_exported_interfaces_only())
    {
      for (auto function : get_undefined_functions())
	if (function)
	  {
	    const_cast<function_decl*>(function)->traverse(walker);
	    if (auto type = is_type(function->get_scope()))
	      {
		type = type->get_canonical_type();
		type->traverse(walker);
	      }
	  }

      for (auto variable : get_undefined_variables())
	if (variable)
	  {
	    variable->traverse(walker);
	    if (auto type = is_type(variable->get_scope()))
	      {
		type = type->get_canonical_type();
		type->traverse(walker);
	      }
	  }
    }
}

/// Get the maps that associate a location string to a certain kind of
/// type.
///
/// The location string is the result of the invocation to the
/// function abigail::ir::location::expand().  It has the form
/// "file.c:4:1", with 'file.c' being the file name, '4' being the
/// line number and '1' being the column number.
///
/// @return the maps.
type_maps&
corpus::get_type_per_loc_map()
{return priv_->type_per_loc_map_;}

/// Getter of the group this corpus is a member of.
///
/// @return the group this corpus is a member of, or nil if it's not
/// part of any @ref corpus_group.
const corpus_group*
corpus::get_group() const
{return priv_->group;}

/// Getter of the group this corpus belongs to.
///
/// @return the group this corpus belong to, or nil if it's not part
/// of any @ref corpus_group.
corpus_group*
corpus::get_group()
{return priv_->group;}

/// Setter of the group this corpus belongs to.
///
/// @param g the new group.
void
corpus::set_group(corpus_group* g)
{
  lock_guard<recursive_mutex> lock(priv_->get_mutex());
  priv_->group = g;
}

/// Initialize the abixml serialization format version number of the
/// corpus.
///
/// This function sets the format version number ot the default one
/// supported by the current version of Libabigail.
void
corpus::init_format_version()
{
  set_format_major_version_number
    (priv_->env.get_config().get_format_major_version_number());
  set_format_minor_version_number
    (priv_->env.get_config().get_format_minor_version_number());
}

/// Getter for the origin of the corpus.
///
/// @return the origin of the corpus.
corpus::origin
corpus::get_origin() const
{return priv_->origin_;}

/// Setter for the origin of the corpus.
///
/// @param o the new origin for the corpus.
void
corpus::set_origin(origin o)
{
  lock_guard<recursive_mutex> lock(priv_->get_mutex());
  priv_->origin_ = o;
}

/// Getter of the major version number of the abixml serialization
/// format.
///
/// @return the major version number of the abixml format.
string&
corpus::get_format_major_version_number() const
{return priv_->format_major_version_number_;}

/// Setter of the major version number of the abixml serialization
/// format.
///
/// @param maj the new major version numberof the abixml format.
void
corpus::set_format_major_version_number(const string& maj)
{
  lock_guard<recursive_mutex> lock(priv_->get_mutex());
  priv_->format_major_version_number_ = maj;
}

/// Getter of the minor version number of the abixml serialization
/// format.
///
/// @return the minor version number of the abixml serialization
/// format.
string&
corpus::get_format_minor_version_number() const
{return priv_->format_minor_version_number_;}

/// Setter of the minor version number of the abixml serialization
/// format.
///
/// @param min the new minor version number of the abixml
/// serialization format.
void
corpus::set_format_minor_version_number(const string& min)
{
  lock_guard<recursive_mutex> lock(priv_->get_mutex());
  priv_->format_minor_version_number_ = min;
}

/// Get the file path associated to the corpus file.
///
/// A subsequent call to corpus::read will deserialize the content of
/// the abi file expected at this path; likewise, a call to
/// corpus::write will serialize the translation units contained in
/// the corpus object into the on-disk file at this path.
///
/// @return the file path associated to the current corpus.
string&
corpus::get_path() const
{return priv_->path;}

/// Set the file path associated to the corpus file.
///
/// A subsequent call to corpus::read will deserialize the content of
/// the abi file expected at this path; likewise, a call to
/// corpus::write will serialize the translation units contained in
/// the corpus object into the on-disk file at this path.
///
/// @param path the new file path to assciate to the current corpus.
void
corpus::set_path(const string& path)
{
  lock_guard<recursive_mutex> lock(priv_->get_mutex());
  priv_->path = path;
}

/// Getter of the needed property of the corpus.
///
/// This property is meaningful for, e.g, corpora built from ELF
/// shared library files.  In that case, this is a vector of names of
/// dependencies of the ELF shared library file.
///
/// @return the vector of dependencies needed by this corpus.
const vector<string>&
corpus::get_needed() const
{return priv_->needed;}

/// Setter of the needed property of the corpus.
///
/// This property is meaningful for, e.g, corpora built from ELF
/// shared library files.  In that case, this is a vector of names of
/// dependencies of the ELF shared library file.
///
/// @param needed the new vector of dependencies needed by this
/// corpus.
void
corpus::set_needed(const vector<string>& needed)
{
  lock_guard<recursive_mutex> lock(priv_->get_mutex());
  priv_->needed = needed;
}

/// Getter for the soname property of the corpus.
///
/// This property is meaningful for, e.g, corpora built from ELF
/// shared library files.  In that case, this is the shared object
/// name exported by the shared library.
///
/// @return the soname property of the corpus.
const string&
corpus::get_soname()
{return priv_->soname;}

/// Setter for the soname property of the corpus.
///
/// This property is meaningful for, e.g, corpora built from ELF
/// shared library files.  In that case, this is the shared object
/// name exported by the shared library.
///
/// @param soname the new soname property of the corpus.
void
corpus::set_soname(const string& soname)
{
  lock_guard<recursive_mutex> lock(priv_->get_mutex());
  priv_->soname = soname;
}

/// Getter for the architecture name of the corpus.
///
/// This property is meaningful for e.g, corpora built from ELF shared
/// library files.  In that case, this is a string representation of
/// the Elf{32,64}_Ehdr::e_machine field.
///
/// @return the architecture name string.
const string&
corpus::get_architecture_name() const
{return priv_->architecture_name;}

/// Setter for the architecture name of the corpus.
///
/// This property is meaningful for e.g, corpora built from ELF shared
/// library files.  In that case, this is a string representation of
/// the Elf{32,64}_Ehdr::e_machine field.
///
/// @param arch the architecture name string.
void
corpus::set_architecture_name(const string& arch)
{
  lock_guard<recursive_mutex> lock(priv_->get_mutex());
  priv_->architecture_name = arch;
}

/// Tests if the corpus is empty from an ABI surface perspective. I.e. if all
/// of these criteria are true:
///  - all translation units (members) are empty
///  - the maps function and variable symbols are not having entries
///  - for shared libraries:
///    - the soname is empty
///    - there are no DT_NEEDED entries
///
/// @return true if the corpus contains no translation unit.
bool
corpus::is_empty() const
{
  lock_guard<recursive_mutex> lock(priv_->get_mutex());
  bool members_empty = true;
  for (translation_units::const_iterator i = priv_->members.begin(),
					 e = priv_->members.end();
       i != e; ++i)
    {
      if (!(*i)->is_empty())
	{
	  members_empty = false;
	  break;
	}
    }
  return (members_empty
	  && (!get_symtab() || !get_symtab()->has_symbols())
	  && priv_->soname.empty()
	  && priv_->needed.empty()
	  && priv_->architecture_name.empty()
	  && !priv_->group);
}

/// Compare the current @ref corpus against another one.
///
/// @param other the other corpus to compare against.
///
/// @return true if the two corpus are equal, false otherwise.
bool
corpus::operator==(const corpus& other) const
{
  translation_units::const_iterator i, j;
  for (i = get_translation_units().begin(),
	 j = other.get_translation_units().begin();
       (i != get_translation_units().end()
	&& j != other.get_translation_units().end());
       ++i, ++j)
    if ((**i) != (**j))
      return false;

  return (i == get_translation_units().end()
	  && j == other.get_translation_units().end());
}

/// Setter for the symtab object.
///
/// @param symtab a shared pointer to the new symtab object
void
corpus::set_symtab(symtab_reader::symtab_sptr symtab)
{
  lock_guard<recursive_mutex> lock(priv_->get_mutex());
  priv_->symtab_ = symtab;
}

/// Getter for the symtab object.
///
/// @return a shared pointer to the symtab object
const symtab_reader::symtab_sptr&
corpus::get_symtab() const
{return priv_->symtab_;}

/// Getter for the function symbols map.
///
/// @return a reference to the function symbols map.
const string_elf_symbols_map_type&
corpus::get_fun_symbol_map() const
{return priv_->get_fun_symbol_map();}

/// Getter for the map of function symbols that are undefined in this
/// corpus.
///
/// @return the map of function symbols not defined in this corpus.
/// The key of the map is the name of the function symbol.  The value
/// is a vector of all the function symbols that have the same name.
const string_elf_symbols_map_type&
corpus::get_undefined_fun_symbol_map() const
{return priv_->get_undefined_fun_symbol_map();}

/// Return a sorted vector of function symbols for this corpus.
///
/// Note that the first time this function is called, the symbols are
/// sorted and cached.  Subsequent invocations of this function return
/// the cached vector that was built previously.
///
/// @return the sorted list of function symbols.
const elf_symbols&
corpus::get_sorted_fun_symbols() const
{return priv_->get_sorted_fun_symbols();}

/// Getter for a sorted vector of the function symbols undefined in
/// this corpus.
///
/// @return a vector of the function symbols undefined in this corpus,
/// sorted by name and then version.
const elf_symbols&
corpus::get_sorted_undefined_fun_symbols() const
{return priv_->get_sorted_undefined_fun_symbols();}

/// Getter for the sorted vector of variable symbols for this corpus.
///
/// Note that the first time this function is called, it computes the
/// sorted vector, caches the result and returns it.  Subsequent
/// invocations of this function just return the cached vector.
///
/// @return the sorted vector of variable symbols for this corpus.
const elf_symbols&
corpus::get_sorted_var_symbols() const
{return priv_->get_sorted_var_symbols();}

/// Getter for a sorted vector of the variable symbols undefined in
/// this corpus.
///
/// @return a vector of the variable symbols undefined in this corpus,
/// sorted by name and then version.
const elf_symbols&
corpus::get_sorted_undefined_var_symbols() const
{return priv_->get_sorted_undefined_var_symbols();}

/// Getter for the variable symbols map.
///
/// @return a reference to the variabl symbols map.
const string_elf_symbols_map_type&
corpus::get_var_symbol_map() const
{return priv_->get_var_symbol_map();}

/// Getter for the map of variable symbols that are undefined in this
/// corpus.
///
/// @return the map of variable symbols not defined in this corpus.
/// The key of the map is the name of the variable symbol.  The value
/// is a vector of all the variable symbols that have the same name.
const string_elf_symbols_map_type&
corpus::get_undefined_var_symbol_map() const
{return priv_->get_undefined_var_symbol_map();}

/// Look in the function symbols map for a symbol with a given name.
///
/// @param n the name of the symbol to look for.
///
/// return the first symbol with the name @p n.
const elf_symbol_sptr
corpus::lookup_function_symbol(const string& n) const
{
  if (get_fun_symbol_map().empty() && get_undefined_fun_symbol_map().empty())
    return elf_symbol_sptr();

  string_elf_symbols_map_type::const_iterator it = get_fun_symbol_map().find(n);
  if ( it == get_fun_symbol_map().end())
    {
      it = get_undefined_fun_symbol_map().find(n);
      if (it == get_undefined_fun_symbol_map().end())
	return elf_symbol_sptr();
    }
  return it->second[0];
}

/// Look into a set of symbols and look for a symbol that has a given
/// version.
///
/// This is a sub-routine for corpus::lookup_function_symbol() and
/// corpus::lookup_variable_symbol().
///
/// @param version the version of the symbol to look for.
///
/// @param symbols the set of symbols to consider.
///
/// @return the symbol found, or nil if none was found.
static const elf_symbol_sptr
find_symbol_by_version(const elf_symbol::version& version,
		       const vector<elf_symbol_sptr>& symbols)
{
  if (version.is_empty())
    {
      // We are looing for a symbol with no version.

      // So first look for possible aliases with no version
      for (elf_symbols::const_iterator s = symbols.begin();
	   s != symbols.end();
	   ++s)
	if ((*s)->get_version().is_empty())
	  return *s;

      // Or, look for a version that is a default one!
      for (elf_symbols::const_iterator s = symbols.begin();
	   s != symbols.end();
	   ++s)
	if ((*s)->get_version().is_default())
	  return *s;
    }
  else
    // We are looking for a symbol with a particular defined version.
    for (elf_symbols::const_iterator s = symbols.begin();
	 s != symbols.end();
	 ++s)
      if ((*s)->get_version().str() == version.str())
	return *s;

  return elf_symbol_sptr();
}

/// Look in the function symbols map for a symbol with a given name.
///
/// @param symbol_name the name of the symbol to look for.
///
/// @param version the version of the symbol to look for.
///
/// return the symbol with name @p symbol_name and with version @p
/// version, or nil if no symbol has been found with that name and
/// version.
const elf_symbol_sptr
corpus::lookup_function_symbol(const string& symbol_name,
			       const elf_symbol::version& version) const
{
  if (get_fun_symbol_map().empty() && get_undefined_fun_symbol_map().empty())
    return elf_symbol_sptr();

  string_elf_symbols_map_type::const_iterator it =
    get_fun_symbol_map().find(symbol_name);
  if ( it == get_fun_symbol_map().end())
    {
      it = get_undefined_fun_symbol_map().find(symbol_name);
      if (it == get_undefined_fun_symbol_map().end())
	return elf_symbol_sptr();
    }

  return find_symbol_by_version(version, it->second);
}

/// Look in the function symbols map for a symbol with the same name
/// and version as a given symbol.
///
/// @param symbol the symbol to look for.
///
/// return the symbol with the same name and version as @p symbol.
const elf_symbol_sptr
corpus::lookup_function_symbol(const elf_symbol& symbol) const
{return lookup_function_symbol(symbol.get_name(), symbol.get_version());}

/// Look in the function symbols map for a symbol with the same name
/// and version as a given symbol.
///
/// @param symbol the symbol to look for.
///
/// return the symbol with the same name and version as @p symbol.
const elf_symbol_sptr
corpus::lookup_function_symbol(const elf_symbol_sptr& symbol) const
{
  if (!symbol)
    return nullptr;
  return lookup_function_symbol(*symbol);
}


/// Look in the variable symbols map for a symbol with a given name.
///
/// @param n the name of the symbol to look for.
///
/// return the first symbol with the name @p n.
const elf_symbol_sptr
corpus::lookup_variable_symbol(const string& n) const
{
  if (get_var_symbol_map().empty() && get_undefined_var_symbol_map().empty())
    return elf_symbol_sptr();

  string_elf_symbols_map_type::const_iterator it = get_var_symbol_map().find(n);
  if ( it == get_var_symbol_map().end())
    {
      it = get_undefined_var_symbol_map().find(n);
      if (it == get_undefined_var_symbol_map().end())
	return elf_symbol_sptr();
    }
  return it->second[0];
}

/// Look in the variable symbols map for a symbol with a given name.
///
/// @param symbol_name the name of the symbol to look for.
///
/// @param symbol_version the version of the symbol to look for.
///
/// return the first symbol with the name @p symbol_name and with
/// version @p version.
const elf_symbol_sptr
corpus::lookup_variable_symbol(const string& symbol_name,
			       const elf_symbol::version& version) const
{
  if (get_var_symbol_map().empty() && get_undefined_var_symbol_map().empty())
    return elf_symbol_sptr();

  string_elf_symbols_map_type::const_iterator it =
    get_var_symbol_map().find(symbol_name);
  if ( it == get_var_symbol_map().end())
    {
      it = get_undefined_var_symbol_map().find(symbol_name);
      if (it == get_undefined_var_symbol_map().end())
	return elf_symbol_sptr();
    }

  return find_symbol_by_version(version, it->second);
}

/// Look in the variable symbols map for a symbol with the same name
/// and version as a given symbol.
///
/// @param symbol the symbol to look for.
///
/// return the symbol with the same name and version as @p symbol.
const elf_symbol_sptr
corpus::lookup_variable_symbol(const elf_symbol& symbol) const
{return lookup_variable_symbol(symbol.get_name(), symbol.get_version());}

/// Return the functions public decl table of the current corpus.
///
/// The function public decl tables is a vector of all the functions
/// and member functions found in the current corpus.
///
/// Note that the caller can suppress some functions from the vector
/// supplying regular expressions describing the set of functions she
/// want to see removed from the public decl table by populating the
/// vector of regular expressions returned by
/// corpus::get_regex_patterns_of_fns_to_suppress().
///
/// @return the vector of functions of the public decl table.  The
/// functions are sorted using their mangled name or name if they
/// don't have mangle names.
const corpus::functions&
corpus::get_functions() const
{return priv_->fns;}

/// Lookup the function which has a given function ID.
///
/// Note that there can have been several functions with the same ID.
/// This is because debug info can declare the same function in
/// several different translation units.  Normally, all these function
/// should be equal.  But still, this function returns all these
/// functions.
///
/// @param id the ID of the function to lookup.  This ID must be
/// either the result of invoking function::get_id() of
/// elf_symbol::get_id_string().
///
/// @return the set of functions which ID is @p id, or nil if no
/// function with that ID was found.
const std::unordered_set<const function_decl*>*
corpus::lookup_functions(const interned_string& id) const
{return priv_->lookup_functions(id);}

/// Lookup the function which has a given function ID.
///
/// Note that there can have been several functions with the same ID.
/// This is because debug info can declare the same function in
/// several different translation units.  Normally, all these function
/// should be equal.  But still, this function returns all these
/// functions.
///
/// @param id the ID of the function to lookup.  This ID must be
/// either the result of invoking function::get_id() of
/// elf_symbol::get_id_string().
///
/// @return the set of functions which ID is @p id, or nil if no
/// function with that ID was found.
const std::unordered_set<const function_decl*>*
corpus::lookup_functions(const char* id) const
{
  if (!id)
    return nullptr;

  interned_string string_id = priv_->env.intern(id);
  return lookup_functions(string_id);
}

/// Lookup the function which has a given function ID.
///
/// Note that there can have been several functions with the same ID.
/// This is because debug info can declare the same function in
/// several different translation units.  Normally, all these function
/// should be equal.  But still, this function returns all these
/// functions.
///
/// @param id the ID of the function to lookup.  This ID must be
/// either the result of invoking function::get_id() of
/// elf_symbol::get_id_string().
///
/// @return the set of functions which ID is @p id, or nil if no
/// function with that ID was found.
const std::unordered_set<const function_decl*>*
corpus::lookup_functions(const string& id) const
{
  interned_string string_id = priv_->env.intern(id);
  return lookup_functions(string_id);
}

/// Lookup the exported variables which all have a given variable ID.
///
/// @param id the ID of the variable to look up.
///
/// @return a pointer to the set of variables with ID @p id, or
/// nullptr if no variable was found with that ID.
const std::unordered_set<var_decl_sptr>*
corpus::lookup_variables(const interned_string& id) const
{
  exported_decls_builder_sptr b = get_exported_decls_builder();
  auto i = b->priv_->id_vars_map_.find(id);
  if (i == b->priv_->id_vars_map_.end())
    return nullptr;
  return &i->second;
}

/// Lookup the exported variables which all have a given variable ID.
///
/// @param id the ID of the variable to look up.
///
/// @return a pointer to the set of variables with ID @p id, or
/// nullptr if no variable was found with that ID.
const std::unordered_set<var_decl_sptr>*
corpus::lookup_variables(const char* id) const
{
  if (!id)
    return nullptr;

  interned_string string_id = priv_->env.intern(id);
  return lookup_variables(string_id);
}

/// Sort the set of functions exported by this corpus.
///
/// This function removes redundant functions from the vector of
/// functions that make up the ABI entry points and then sort the
/// resulting de-duplicated vector of functions.
///
/// Normally, unless you are writting a front-end that creates an ABI
/// @ref corpus, you shouldn't be calling this because the code that
/// creates the corpus for should do it for you.  In any case, this
/// function should be called after the types carried by the
/// environment of the corpus are canonicalized.
void
corpus::sort_functions()
{
  priv_->remove_redundant_functions();

  func_comp fc;

  lock_guard<recursive_mutex> lock(priv_->get_mutex());
  {
    std::sort(priv_->fns.begin(), priv_->fns.end(), fc);

    priv_->sorted_undefined_fns.clear();

    for (auto& f : priv_->undefined_fns)
      priv_->sorted_undefined_fns.push_back(f);

    std::sort(priv_->sorted_undefined_fns.begin(),
	      priv_->sorted_undefined_fns.end(), fc);
  }
}

/// Return the public decl table of the global variables of the
/// current corpus.
///
/// The variable public decls table is a vector of all the public
/// global variables and static member variables found in the current
/// corpus.
///
/// Note that the caller can suppress some variables from the vector
/// supplying regular expressions describing the set of variables she
/// wants to see removed from the public decl table by populating the
/// vector of regular expressions returned by
/// corpus::get_regex_patterns_of_fns_to_suppress().
///
/// @return the vector of variables of the public decl table.  The
/// variables are sorted using their name.
const corpus::variables&
corpus::get_variables() const
{return priv_->vars;}

/// Sort the set of variables exported by this corpus.
///
/// Normally, you shouldn't be calling this as the code that creates
/// the corpus for you should do it for you too.
void
corpus::sort_variables()
{
  lock_guard<recursive_mutex> lock(priv_->get_mutex());

  var_comp vc;
  std::sort(priv_->vars.begin(), priv_->vars.end(), vc);

  priv_->sorted_undefined_vars.clear();
  for (auto& f : priv_->undefined_vars)
    priv_->sorted_undefined_vars.push_back(f);

  std::sort(priv_->sorted_undefined_vars.begin(),
	    priv_->sorted_undefined_vars.end(), vc);
}

/// Getter of the undefined functions of the corpus.
///
/// Undefined functions are functions which symbols are not defined.
///
/// @return a set of @ref function_decl* representing the functions
/// that are undefined in the corpus.
const corpus::functions_set&
corpus::get_undefined_functions() const
{return priv_->undefined_fns;}

/// Getter of the undefined functions of the corpus.
///
/// @return a set of @ref function_decl* representing the functions
/// that are undefined in the corpus.
corpus::functions_set&
corpus::get_undefined_functions()
{return priv_->undefined_fns;}

/// Getter of the sorted vector of undefined functions of the corpus.
///
/// @return a vector of @ref function_decl* representing the functions
/// that are undefined in the corpus.
const corpus::functions&
corpus::get_sorted_undefined_functions() const
{
  if (priv_->sorted_undefined_fns.empty()
      && !priv_->undefined_fns.empty())
    // We have undefined functions but we haven't sorted them yet.
    // Let's do the sorting now then.
    const_cast<corpus*>(this)->sort_functions();

  return priv_->sorted_undefined_fns;
}

void
corpus::add_undefined_function(const function_decl* fn)
{
  if (fn)
    {
      lock_guard<mutex> lock(priv_->undefined_fns_mutex_);
      get_undefined_functions().insert(fn);
    }
}

/// Getter of the undefined variables of the corpus.
///
/// @return a set of @ref var_decl* representing the variables that
/// are undefined in the corpus.
const corpus::variables_set&
corpus::get_undefined_variables() const
{return priv_->undefined_vars;}

/// Getter of the undefined variables of the corpus.
///
/// @return a set of @ref var_decl* representing the variables that
/// are undefined in the corpus.
corpus::variables_set&
corpus::get_undefined_variables()
{return priv_->undefined_vars;}

/// Getter of the sorted vector of undefined variables of the corpus.
///
/// @return a sorted vector of @ref var_decl* representing the
/// variables that are undefined in the corpus.
const corpus::variables&
corpus::get_sorted_undefined_variables() const
{
  if (priv_->sorted_undefined_vars.empty()
      && !priv_->undefined_vars.empty())
    // We have undefined variables but we haven't sorted them yet.
    // Let's do the sorting now then.
    const_cast<corpus*>(this)->sort_variables();

  return priv_->sorted_undefined_vars;
}

void
corpus::add_undefined_variable(const var_decl_sptr& var)
{
  if (var)
    {
      lock_guard<mutex> lock(priv_->undefined_vars_mutex_);
      get_undefined_variables().insert(var);
    }
}
/// Getter of the set of function symbols that are not referenced by
/// any function exported by the current corpus.
///
/// When the corpus has been created from an ELF library or program,
/// this function returns the set of function symbols not referenced
/// by any debug information.
///
/// @return the vector of function symbols not referenced by any
/// function exported by the current corpus.
const elf_symbols&
corpus::get_unreferenced_function_symbols() const
{return priv_->get_unreferenced_function_symbols();}

/// Getter of the set of variable symbols that are not referenced by
/// any variable exported by the current corpus.
///
/// When the corpus has been created from an ELF library or program,
/// this function returns the set of variable symbols not referenced
/// by any debug information.
///
/// @return the vector of variable symbols not referenced by any
/// variable exported by the current corpus.
const elf_symbols&
corpus::get_unreferenced_variable_symbols() const
{return priv_->get_unreferenced_variable_symbols();}

/// Accessor for the regex patterns describing the functions to drop
/// from the public decl table.
///
/// @return the regex patterns describing the functions to drop from
/// the public decl table.
vector<string>&
corpus::get_regex_patterns_of_fns_to_suppress()
{return priv_->regex_patterns_fns_to_suppress;}

/// Accessor for the regex patterns describing the functions to drop
/// from the public decl table.
///
/// @return the regex patterns describing the functions to drop from
/// the public decl table.
const vector<string>&
corpus::get_regex_patterns_of_fns_to_suppress() const
{return priv_->regex_patterns_fns_to_suppress;}

/// Accessor for the regex patterns describing the variables to drop
/// from the public decl table.
///
/// @return the regex patterns describing the variables to drop from
/// the public decl table.
vector<string>&
corpus::get_regex_patterns_of_vars_to_suppress()
{return priv_->regex_patterns_vars_to_suppress;}

/// Accessor for the regex patterns describing the variables to drop
/// from the public decl table.
///
/// @return the regex patterns describing the variables to drop from
/// the public decl table.
const vector<string>&
corpus::get_regex_patterns_of_vars_to_suppress() const
{return priv_->regex_patterns_vars_to_suppress;}

/// Accessor for the regex patterns describing the functions to keep
/// into the public decl table.  The other functions not matches by these
/// regexes are dropped from the public decl table.
///
/// @return the regex patterns describing the functions to keep into
/// the public decl table.
vector<string>&
corpus::get_regex_patterns_of_fns_to_keep()
{return priv_->regex_patterns_fns_to_keep;}

/// Accessor for the regex patterns describing the functions to keep
/// into the public decl table.  The other functions not matches by these
/// regexes are dropped from the public decl table.
///
/// @return the regex patterns describing the functions to keep into
/// the public decl table.
const vector<string>&
corpus::get_regex_patterns_of_fns_to_keep() const
{return priv_->regex_patterns_fns_to_keep;}

/// Getter for the vector of function symbol IDs to keep.
///
/// A symbol ID is a string made of the name of the symbol and its
/// version, separated by one or two '@'.
///
/// @return a vector of IDs of function symbols to keep.
vector<string>&
corpus::get_sym_ids_of_fns_to_keep()
{return priv_->sym_id_fns_to_keep;}

/// Getter for the vector of function symbol IDs to keep.
///
/// A symbol ID is a string made of the name of the symbol and its
/// version, separated by one or two '@'.
///
/// @return a vector of IDs of function symbols to keep.
const vector<string>&
corpus::get_sym_ids_of_fns_to_keep() const
{return priv_->sym_id_fns_to_keep;}

/// Accessor for the regex patterns describing the variables to keep
/// into the public decl table.  The other variables not matches by these
/// regexes are dropped from the public decl table.
///
/// @return the regex patterns describing the variables to keep into
/// the public decl table.
vector<string>&
corpus::get_regex_patterns_of_vars_to_keep()
{return priv_->regex_patterns_vars_to_keep;}

/// Accessor for the regex patterns describing the variables to keep
/// into the public decl table.  The other variables not matches by these
/// regexes are dropped from the public decl table.
///
/// @return the regex patterns describing the variables to keep into
/// the public decl table.
const vector<string>&
corpus::get_regex_patterns_of_vars_to_keep() const
{return priv_->regex_patterns_vars_to_keep;}

/// Getter for the vector of variable symbol IDs to keep.
///
/// A symbol ID is a string made of the name of the symbol and its
/// version, separated by one or two '@'.
///
/// @return a vector of IDs of variable symbols to keep.
vector<string>&
corpus::get_sym_ids_of_vars_to_keep()
{return priv_->sym_id_vars_to_keep;}

/// Getter for the vector of variable symbol IDs to keep.
///
/// A symbol ID is a string made of the name of the symbol and its
/// version, separated by one or two '@'.
///
/// @return a vector of IDs of variable symbols to keep.
const vector<string>&
corpus::get_sym_ids_of_vars_to_keep() const
{return priv_->sym_id_vars_to_keep;}

/// After the set of exported functions and variables have been built,
/// consider all the tunables that control that set and see if some
/// functions need to be removed from that set; if so, remove them.
void
corpus::maybe_drop_some_exported_decls()
{
  lock_guard<recursive_mutex> lock(priv_->get_mutex());
  string sym_name, sym_version;

  functions fns_to_keep;
  exported_decls_builder* b = get_exported_decls_builder().get();
  for (auto f = priv_->fns.begin(); f != priv_->fns.end(); ++f)
    {
      if (b->priv_->keep_wrt_id_of_fns_to_keep(*f)
	  && b->priv_->keep_wrt_regex_of_fns_to_suppress(*f)
	  && b->priv_->keep_wrt_regex_of_fns_to_keep(*f))
	fns_to_keep.push_back(*f);
    }
  priv_->fns = fns_to_keep;

  variables vars_to_keep;
  for (auto v = priv_->vars.begin(); v != priv_->vars.end(); ++v)
    {
      if (b->priv_->keep_wrt_id_of_vars_to_keep(*v)
	  && b->priv_->keep_wrt_regex_of_vars_to_suppress(*v)
	  && b->priv_->keep_wrt_regex_of_vars_to_keep(*v))
	vars_to_keep.push_back(*v);
    }
  priv_->vars = vars_to_keep;
}

///  Getter for the object that is responsible for determining what
///  decls ought to be in the set of exported decls.
///
///  The object does have methods to add the decls to the set of
///  exported decls, right at the place where the corpus expects it,
///  so that there is no unnecessary copying involved.
///
///  @return a (smart) pointer to the instance of @ref
///  corpus::exported_decls_builder that is responsible for determine
///  what decls ought to be in the set of exported decls.
corpus::exported_decls_builder_sptr
corpus::get_exported_decls_builder() const
{
  {
    lock_guard<mutex> lock(priv_->exported_decls_builder_mutex);
    if (!priv_->exported_decls_builder)
      {
	priv_->exported_decls_builder.reset
	  (new exported_decls_builder(priv_->fns,
				      priv_->vars,
				      priv_->regex_patterns_fns_to_suppress,
				      priv_->regex_patterns_vars_to_suppress,
				      priv_->regex_patterns_fns_to_keep,
				      priv_->regex_patterns_vars_to_keep,
				      priv_->sym_id_fns_to_keep,
				      priv_->sym_id_vars_to_keep));
      }
  }
  return priv_->exported_decls_builder;
}

/// Bitwise | operator for the corpus::origin type.
///
/// @param l the left-hand side operand of the | operation.
///
/// @param r the right-hand side operand of the | operation.
///
/// @return the result of the operation.
corpus::origin
operator|(corpus::origin l, corpus::origin r)
{
  return static_cast<corpus::origin>
    (static_cast<uint32_t>(l) |  static_cast<uint32_t>(r));
}

/// Bitwise |= operator for the corpus::origin type.
///
/// @param l the left-hand side operand for the |= operation.
///
/// @param r the right-hand side operand for the |= operation.
///
/// @return the result of the operation.
corpus::origin
operator|=(corpus::origin &l, corpus::origin r)
{
  l = l | r;
  return l;
}

/// Bitwise & operator for the corpus::origin type.
///
/// @param l the left-hand side operand of the & operation.
///
/// @param r the right-hand side operand of the & operation.
///
/// @return the result of the operation.
corpus::origin
operator&(corpus::origin l, corpus::origin r)
{
    return static_cast<corpus::origin>
    (static_cast<uint32_t>(l) & static_cast<uint32_t>(r));
}

/// Bitwise &= operator for the corpus::origin type.
///
/// @param l the left-hand side operand of the &= operation.
///
/// @param r the right-hand side operand of the &= operation.
///
/// @return the result of the operation.
corpus::origin
operator&=(corpus::origin &l, corpus::origin r)
{
  l = l & r;
  return l;
}

// </corpus stuff>

// <corpus_group stuff>

/// Type of the private data of @ref corpus_group
struct corpus_group::priv
{
  recursive_mutex		mutex;
  std::set<string>		corpora_paths;
  corpora_type			corpora;
  istring_function_decl_ptr_map_type fns_map;
  corpus::functions		fns;
  istring_var_decl_ptr_map_type vars_map;
  corpus::variables		vars;
  string_elf_symbols_map_type	var_symbol_map;
  string_elf_symbols_map_type	fun_symbol_map;
  elf_symbols			sorted_var_symbols;
  elf_symbols			sorted_fun_symbols;
  unordered_map<string, elf_symbol_sptr> unrefed_fun_symbol_map;
  elf_symbols			unrefed_fun_symbols;
  bool				unrefed_fun_symbols_built;
  unordered_map<string, elf_symbol_sptr> unrefed_var_symbol_map;
  elf_symbols			unrefed_var_symbols;
  bool				unrefed_var_symbols_built;
  unordered_set<interned_string, hash_interned_string> pub_type_pretty_reprs_;

  priv()
    : unrefed_fun_symbols_built(),
      unrefed_var_symbols_built()
  {}

  /// Add symbols to the set of corpus group function symbols that are
  /// *NOT* referenced by debug info.
  ///
  /// @param syms the set the symbols to add.
  void
  add_unref_fun_symbols(const elf_symbols& syms)
  {
    lock_guard<recursive_mutex> lock(get_mutex());
    for (elf_symbols::const_iterator e = syms.begin();
	 e != syms.end();
	 ++e)
      {
	string sym_id = (*e)->get_id_string();
	unordered_map<string, elf_symbol_sptr>::const_iterator j =
	  unrefed_fun_symbol_map.find(sym_id);
	if (j != unrefed_fun_symbol_map.end())
	  continue;

	unrefed_fun_symbol_map[sym_id] = *e;
	unrefed_fun_symbols.push_back(*e);
      }
    unrefed_fun_symbols_built = true;
  }

  /// Add symbols to the set of corpus group variable symbols that are
  /// *NOT* referenced by debug info.
  ///
  /// @param syms the set the symbols to add.
  void
  add_unref_var_symbols(const elf_symbols& syms)
  {
    lock_guard<recursive_mutex> lock(get_mutex());
    for (elf_symbols::const_iterator e = syms.begin();
	 e != syms.end();
	 ++e)
      {
	string sym_id = (*e)->get_id_string();
	unordered_map<string, elf_symbol_sptr>::const_iterator j =
	  unrefed_var_symbol_map.find(sym_id);
	if (j != unrefed_var_symbol_map.end())
	  continue;

	unrefed_var_symbol_map[sym_id] = *e;
	unrefed_var_symbols.push_back(*e);
      }
    unrefed_var_symbols_built = true;
  }

  recursive_mutex&
  get_mutex()
  {return mutex;}
}; // end corpus_group::priv

/// Constructor of the @ref corpus_group type.
///
/// @param env the environment of the @ref corpus_group.
///
/// @param path the path to the file represented by the corpus group.
corpus_group::corpus_group(const environment& env, const string& path = "")
  : corpus(env, path), priv_(new priv)
{}

/// Desctructor of the @ref corpus_group type.
corpus_group::~corpus_group()
{}

/// Add a new corpus to the current instance of @ref corpus_group.
///
/// @param corp the new corpus to add.
void
corpus_group::add_corpus(const corpus_sptr& corp)
{
  if (!corp)
    return;

  if (!corp->get_path().empty()
      && has_corpus(corp->get_path()))
    return;

  lock_guard<recursive_mutex> lock(priv_->get_mutex());
  {
    // Ensure the new architecture name matches the current one.
    string cur_arch = get_architecture_name(),
      corp_arch = corp->get_architecture_name();
    if (cur_arch.empty())
      set_architecture_name(corp_arch);
    else if (cur_arch != corp_arch)
      {
	std::cerr << "corpus '" << corp->get_path() << "'"
		  << " has architecture '" << corp_arch << "'"
		  << " but expected '" << cur_arch << "'\n";
	ABG_ASSERT_NOT_REACHED;
      }

    priv_->corpora.push_back(corp);
    corp->set_group(this);
    priv_->corpora_paths.insert(corp->get_path());
  }

  /// Add the unreferenced function and variable symbols of this
  /// corpus to the unreferenced symbols of the current corpus group.
  priv_->add_unref_fun_symbols(get_unreferenced_function_symbols());
  priv_->add_unref_var_symbols(get_unreferenced_variable_symbols());

  // Copy the reachable types from the added corpus to the group.
  for (auto& t : corp->priv_->reachable_types_from_pub_ifaces_)
    record_type_as_reachable_from_public_interfaces(*t);
}

/// Test if a corpus of a given path has been added to the group.
///
/// @param path the path to the corpus to consider.
///
/// @return true iff a corpus with path @p path is already present in
/// the group⋅
bool
corpus_group::has_corpus(const string& path)
{
  if (priv_->corpora_paths.find(path) != priv_->corpora_paths.end())
    return true;
  return false;
}

/// Getter of the vector of corpora held by the current @ref
/// corpus_group.
///
/// @return the vector corpora.
const corpus_group::corpora_type&
corpus_group::get_corpora() const
{return priv_->corpora;}

/// Getter of the first corpus added to this Group.
///
/// @return the first corpus added to this Group.
const corpus_sptr
corpus_group::get_main_corpus() const
{return const_cast<corpus_group*>(this)->get_main_corpus();}

/// Getter of the first corpus added to this Group.
///
/// @return the first corpus added to this Group.
corpus_sptr
corpus_group::get_main_corpus()
{
  if (!get_corpora().empty())
    return get_corpora().front();
  return corpus_sptr();
}

/// Test if the current corpus group is empty.
///
/// @return true iff the current corpus group is empty.
bool
corpus_group::is_empty() const
{return get_corpora().empty();}

/// Get the functions exported by the corpora of the current corpus
/// group.
///
/// Upon its first invocation, this function walks the corpora
/// contained in the corpus group and caches the functions they exported.
///
/// Subsequent invocations just return the cached functions.
///
/// @return the exported functions.
const corpus::functions&
corpus_group::get_functions() const
{
  if (priv_->fns.empty())
    {
      lock_guard<recursive_mutex> lock(priv_->get_mutex());
      for (corpora_type::const_iterator i = get_corpora().begin();
	   i != get_corpora().end();
	   ++i)
	{
	  corpus_sptr c = *i;
	  for (corpus::functions::const_iterator f = c->get_functions().begin();
	       f != c->get_functions().end();
	       ++f)
	    {
	      interned_string fid = (*f)->get_id();
	      istring_function_decl_ptr_map_type::const_iterator j =
		priv_->fns_map.find(fid);

	      if (j != priv_->fns_map.end())
		// Don't cache the same function twice ...
		continue;

	      priv_->fns_map[fid] = *f;
	      // really cache the function now.
	      priv_->fns.push_back(*f);
	    }
	}
    }

  return priv_->fns;
}

/// Get the global variables exported by the corpora of the current
/// corpus group.
///
/// Upon its first invocation, this function walks the corpora
/// contained in the corpus group and caches the variables they
/// export.
///
/// @return the exported variables.
const corpus::variables&
corpus_group::get_variables() const
{
  if (priv_->vars.empty())
    {
      lock_guard<recursive_mutex> lock(priv_->get_mutex());
      for (corpora_type::const_iterator i = get_corpora().begin();
	   i != get_corpora().end();
	   ++i)
	{
	  corpus_sptr c = *i;
	  for (corpus::variables::const_iterator v = c->get_variables().begin();
	       v != c->get_variables().end();
	       ++v)
	    {
	      interned_string vid = (*v)->get_id();
	      istring_var_decl_ptr_map_type::const_iterator j =
		priv_->vars_map.find(vid);

	      if (j != priv_->vars_map.end())
		// Don't cache the same variable twice ...
		continue;

	      priv_->vars_map[vid] = *v;
	      // Really cache the variable now.
	      priv_->vars.push_back(*v);
	    }
	}
    }

  return priv_->vars;
}

/// Get the symbols of the global variables exported by the corpora of
/// the current @ref corpus_group.
///
/// @return the symbols of the global variables exported by the corpora
const string_elf_symbols_map_type&
corpus_group::get_var_symbol_map() const
{
  if (priv_->var_symbol_map.empty())
    {
      lock_guard<recursive_mutex> lock(priv_->get_mutex());
      for (corpora_type::const_iterator i = get_corpora().begin();
	   i != get_corpora().end();
	   ++i)
	priv_->var_symbol_map.insert((*i)->get_var_symbol_map().begin(),
				     (*i)->get_var_symbol_map().end());
    }
  return priv_->var_symbol_map;
}

/// Get the symbols of the global functions exported by the corpora of
/// the current @ref corpus_group.
///
/// @return the symbols of the global functions exported by the corpora
const string_elf_symbols_map_type&
corpus_group::get_fun_symbol_map() const
{
  if (priv_->fun_symbol_map.empty())
    {
      lock_guard<recursive_mutex> lock(priv_->get_mutex());
      for (corpora_type::const_iterator i = get_corpora().begin();
	   i != get_corpora().end();
	   ++i)
	priv_->fun_symbol_map.insert((*i)->get_fun_symbol_map().begin(),
				     (*i)->get_fun_symbol_map().end());
    }

  return priv_->fun_symbol_map;
}

/// Get a sorted vector of the symbols of the functions exported by
/// the corpora of the current group.
///
/// @return the sorted vectors of the exported function symbols.
const elf_symbols&
corpus_group::get_sorted_fun_symbols() const
{
  if (priv_->sorted_fun_symbols.empty()
      && !get_fun_symbol_map().empty())
    {
      lock_guard<recursive_mutex> lock(priv_->get_mutex());
      for (corpora_type::const_iterator i = get_corpora().begin();
	   i != get_corpora().end();
	   ++i)
	{
	  corpus_sptr c = *i;
	  for (string_elf_symbols_map_type::const_iterator j =
		 c->get_fun_symbol_map().begin();
	       j != c->get_fun_symbol_map().begin();
	       ++j)
	    priv_->sorted_fun_symbols.insert(priv_->sorted_fun_symbols.end(),
					     j->second.begin(),
					     j->second.end());
	}
      comp_elf_symbols_functor comp;
      std::sort(priv_->sorted_fun_symbols.begin(),
		priv_->sorted_fun_symbols.end(),
		comp);
    }

  return priv_->sorted_fun_symbols;
}

/// Get a sorted vector of the symbols of the variables exported by
/// the corpora of the current group.
///
/// @return the sorted vectors of the exported variable symbols.
const elf_symbols&
corpus_group::get_sorted_var_symbols() const
{
  if (priv_->sorted_var_symbols.empty()
      && !get_var_symbol_map().empty())
    {
      lock_guard<recursive_mutex> lock(priv_->get_mutex());
      for (corpora_type::const_iterator i = get_corpora().begin();
	   i != get_corpora().end();
	   ++i)
	{
	  corpus_sptr c = *i;
	  for (string_elf_symbols_map_type::const_iterator j =
		 c->get_var_symbol_map().begin();
	       j != c->get_var_symbol_map().begin();
	       ++j)
	    priv_->sorted_var_symbols.insert(priv_->sorted_var_symbols.end(),
					     j->second.begin(),
					     j->second.end());
	}
      comp_elf_symbols_functor comp;
      std::sort(priv_->sorted_var_symbols.begin(),
		priv_->sorted_var_symbols.end(),
		comp);
    }

  return priv_->sorted_var_symbols;
}

/// Get the set of function symbols not referenced by any debug info,
/// from all the corpora of the current corpus group.
///
/// Upon its first invocation, this function possibly walks all the
/// copora of this corpus group and caches the unreferenced symbols
/// they export.  The function then returns the cache.
///
/// Upon subsequent invocations, this functions just returns the
/// cached symbols.
///
/// @return the unreferenced symbols.
const elf_symbols&
corpus_group::get_unreferenced_function_symbols() const
{
  if (!priv_->unrefed_fun_symbols_built)
    if (priv_->unrefed_fun_symbols.empty())
      {
	lock_guard<recursive_mutex> lock(priv_->get_mutex());
	for (corpora_type::const_iterator i = get_corpora().begin();
	     i != get_corpora().end();
	     ++i)
	  {
	    corpus_sptr c = *i;
	    for (elf_symbols::const_iterator e =
		   c->get_unreferenced_function_symbols().begin();
		 e != c->get_unreferenced_function_symbols().end();
		 ++e)
	      {
		string sym_id = (*e)->get_id_string();
		unordered_map<string, elf_symbol_sptr>::const_iterator j =
		  priv_->unrefed_fun_symbol_map.find(sym_id);
		if (j != priv_->unrefed_fun_symbol_map.end())
		  continue;

		priv_->unrefed_fun_symbol_map[sym_id] = *e;
		priv_->unrefed_fun_symbols.push_back(*e);
	      }
	  }
	priv_->unrefed_fun_symbols_built = true;
      }

  return priv_->unrefed_fun_symbols;
}

/// Get the set of variable symbols not referenced by any debug info,
/// from all the corpora of the current corpus group.
///
/// Upon its first invocation, this function possibly walks all the
/// copora of this corpus group and caches the unreferenced symbols
/// they export.  The function then returns the cache.
///
/// Upon subsequent invocations, this functions just returns the
/// cached symbols.
///
/// @return the unreferenced symbols.
const elf_symbols&
corpus_group::get_unreferenced_variable_symbols() const
{
  if (!priv_->unrefed_var_symbols_built)
    if (priv_->unrefed_var_symbols.empty())
      {
	lock_guard<recursive_mutex> lock(priv_->get_mutex());
	for (corpora_type::const_iterator i = get_corpora().begin();
	     i != get_corpora().end();
	     ++i)
	  {
	    corpus_sptr c = *i;
	    for (elf_symbols::const_iterator e =
		   c->get_unreferenced_variable_symbols().begin();
		 e != c->get_unreferenced_variable_symbols().end();
		 ++e)
	      {
		string sym_id = (*e)->get_id_string();
		unordered_map<string, elf_symbol_sptr>::const_iterator j =
		  priv_->unrefed_var_symbol_map.find(sym_id);
		if (j != priv_->unrefed_var_symbol_map.end())
		  continue;

		priv_->unrefed_var_symbol_map[sym_id] = *e;
		priv_->unrefed_var_symbols.push_back(*e);
	      }
	  }
	priv_->unrefed_var_symbols_built = true;
      }

  return priv_->unrefed_var_symbols;
}

/// Lookup the function which has a given function ID.
///
/// Note that there can have been several functions with the same ID.
/// This is because debug info can declare the same function in
/// several different translation units.  Normally, all these
/// functions should be equal.  But still, this function returns all
/// these functions.
///
/// Also, note that this function cycles over each corpora of the
/// corpus group, invokes corpus::lookup_functions on them and returns
/// the result of the first one that succeeds.
///
/// @param id the ID of the function to lookup.  This ID must be
/// either the result of invoking function::get_id() of
/// elf_symbol::get_id_string().
///
/// @return the set of functions which ID is @p id, or nil if no
/// function with that ID was found.
const std::unordered_set<const function_decl*>*
corpus_group::lookup_functions(const interned_string& id) const
{
  for (auto& corp :get_corpora())
    if (auto fns = corp->lookup_functions(id))
      return fns;

  return nullptr;
}

/// Lookup the function which has a given function ID.
///
/// Note that there can have been several functions with the same ID.
/// This is because debug info can declare the same function in
/// several different translation units.  Normally, all these
/// functions should be equal.  But still, this function returns all
/// these functions.
///
/// Also, note that this function cycles over each corpora of the
/// corpus group, invokes corpus::lookup_functions on them and returns
/// the result of the first one that succeeds.
///
/// @param id the ID of the function to lookup.  This ID must be
/// either the result of invoking function::get_id() of
/// elf_symbol::get_id_string().
///
/// @return the set of functions which ID is @p id, or nil if no
/// function with that ID was found.
const std::unordered_set<const function_decl*>*
corpus_group::lookup_functions(const char* id) const
{
  for (auto& corp :get_corpora())
    if (const auto& fns = corp->lookup_functions(id))
      return fns;

  return nullptr;
}

/// Lookup the exported variables which all have a given variable ID.
///
/// Note that this function cycles over each corpora of the corpus
/// group, invokes corpus::lookup_variabless on them and returns the
/// result of the first one that succeeds.
///
/// @param id the ID of the variable to look up.
///
/// @return a pointer to the set of variables with ID @p id, or
/// nullptr if no variable was found with that ID.
const std::unordered_set<var_decl_sptr>*
corpus_group::lookup_variables(const interned_string& id) const
{
  for (auto& corp :get_corpora())
    if (const auto& vars = corp->lookup_variables(id))
      return vars;

  return nullptr;
}

/// Lookup the exported variables which all have a given variable ID.
///
/// Note that this function cycles over each corpora of the corpus
/// group, invokes corpus::lookup_variabless on them and returns the
/// result of the first one that succeeds.
///
/// @param id the ID of the variable to look up.
///
/// @return a pointer to the set of variables with ID @p id, or
/// nullptr if no variable was found with that ID.
const std::unordered_set<var_decl_sptr>*
corpus_group::lookup_variables(const char* id) const
{
  for (auto& corp :get_corpora())
    if (const auto& vars = corp->lookup_variables(id))
      return vars;

  return nullptr;
}

/// Test if a @ref corpus is a @ref corpus_group.
///
/// @param corpus the corpus to consider.
///
/// @return the @ref corpus_group is @p corpus is a corpus group, or
/// nil.
corpus_group_sptr
is_corpus_group(const corpus_sptr& corpus)
{return std::dynamic_pointer_cast<corpus_group>(corpus);}

// </corpus_group stuff>

void
dumptypes(const vector<type_base_wptr>& types,
	  const char* output_file,
	  const corpus& abi,
	  bool emit_location)
{
  if (!output_file)
    return;

  std::ofstream of(output_file);
  if (of.fail() ||!of.is_open())
    return;

  string repr;
  for (auto& t : types)
    {
      type_base_sptr type(t.lock().get(), sptr_utils::noop_deleter());
      repr = type->get_pretty_representation();
      std::ostringstream os;
      os << "'" << repr << "'";
      if (emit_location)
	if (decl_base_sptr decl = get_type_declaration(type))
	  os << ":" << decl->get_location().expand();
      os << " // ";
      if (abi.type_is_reachable_from_public_interfaces(*type))
	os << " r";
      else
	os << " nr";
      of << os.str() << "\n";
    }
  of.close();
}

}// end namespace ir
}// end namespace abigail
