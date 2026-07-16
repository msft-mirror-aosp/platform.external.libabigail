// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// -*- mode: C++ -*-
//
// Copyright (C) 2013-2026 Red Hat, Inc.

/// @file
///
/// This file contains the definitions of the entry points to
/// de-serialize an instance of @ref abigail::translation_unit to an
/// ABI Instrumentation file in libabigail native XML format.  This
/// native XML format is named "abixml".

#include "config.h"
#include <assert.h>
#include <algorithm>
#include <fstream>
#include <iomanip>
#include <ios>
#include <iostream>
#include <memory>
#include <sstream>
#include <stack>
#include <unordered_map>
#include <vector>

#include "abg-tools-utils.h"
#include "abg-ir-priv.h"

#include "abg-ir-priv.h"
#include "abg-internal.h"
// <headers defining libabigail's API go under here>
ABG_BEGIN_EXPORT_DECLARATIONS

#include "abg-config.h"
#include "abg-corpus.h"
#include "abg-hash.h"
#include "abg-sptr-utils.h"

#include "abg-writer.h"
#include "abg-libxml-utils.h"
#include "abg-fwd.h"

ABG_END_EXPORT_DECLARATIONS
// </headers defining libabigail's API>

namespace abigail
{
using std::cerr;
using std::shared_ptr;
using std::dynamic_pointer_cast;
using std::static_pointer_cast;
using std::ofstream;
using std::ostream;
using std::ostringstream;
using std::list;
using std::vector;
using std::stack;
using std::unordered_map;
using abigail::sptr_utils::noop_deleter;

/// The namespace for the native XML file format writer.
///
/// It contains utilities to serialize ABI artifacts from the @ref ir
/// namespace into the native XML format.
namespace xml_writer
{

class id_manager
{
  const environment& m_env;
  mutable unsigned long long m_cur_id;

  unsigned long long
  get_new_id() const
  { return ++m_cur_id; }

public:
  id_manager(const environment& env)
    : m_env(env),
      m_cur_id(0) {}

  const environment&
  get_environment() const
  {return m_env;}

  /// Return a unique string representing a numerical id.
  interned_string
  get_id() const
  {
    ostringstream o;
    o << get_new_id();
    const environment& env = get_environment();
    return env.intern(o.str());
  }

  /// Return a unique string representing a numerical ID, prefixed by
  /// prefix.
  ///
  /// @param prefix the prefix of the returned unique id.
  interned_string
  get_id_with_prefix(const string& prefix) const
  {
    ostringstream o;
    o << prefix << get_new_id();
    const environment& env = get_environment();
    return env.intern(o.str());
  }
};

/// The hashing functor of for the set of non canonicalized types, aka
/// @ref nc_type_ptr_set_type
struct non_canonicalized_type_hash
{
  /// Hashing function
  ///
  /// This hashes a string representation of the non canonicalized
  /// types.  For now, two typedefs with different names but with the
  /// same underlying types will hash differently.
  ///
  /// TODO: try making typedefs with different names hash the same if
  /// their underlying types are equal and see what breaks.
  ///
  /// @param p the non canonicalized type to hash.
  ///
  /// @return the hash value.
  size_t
  operator() (const type_base* p) const
  {
    ABG_ASSERT(is_non_canonicalized_type(p));
    std::hash<string> h;
    return h(p->get_pretty_representation(/*internal=*/false,
					  // By choosing the
					  // non-internal format,
					  // classes and structs are
					  // named differently and
					  // typedefs and
					  // classes/structs are named
					  // differently.  This
					  // implies less uncertainty
					  // in the sorting.
					  true));
  }
}; // end struct non_canonicalized_type_hash

/// The equality functor of for the set of non canonicalized types, aka
/// @ref nc_type_ptr_set_type
struct non_canonicalized_type_equal
{
  /// The equality operator.
  ///
  /// @param l the left-hand operand of the operation.
  ///
  /// @param r the right-hand operand of the operation.
  ///
  /// For now, two typedefs with different names but with the same
  /// underlying types are considered different.
  ///
  /// TODO: try making typedefs with different names hash the same if
  /// their underlying types are equal and see what breaks.
  //
  // @return true iff @p l equals @p r.
  bool
  operator()(const type_base *l, const type_base *r) const
  {
    ABG_ASSERT(is_non_canonicalized_type(l));
    ABG_ASSERT(is_non_canonicalized_type(r));

   return *l == *r;
  }
}; // end struct non_canonicalized_type_equal

/// The equality functor of for the set of canonicalized types
struct canonicalized_type_equal
{
  /// Equality operator between two pointers to @ref type_base.
  ///
  /// @param l the first type to compare.
  ///
  /// @param r the second type to compare.
  ///
  /// @return true iff @p l equals @p r.
  bool
  operator()(const type_base *l, const type_base *r) const
  {
    ABG_ASSERT(l && r);

    l = get_exemplar_type(l);
    r = get_exemplar_type(r);

    if (l->get_naked_canonical_type() && r->get_canonical_type())
      return l == r;

    if (!l->get_naked_canonical_type() && !r->get_canonical_type())
      {
	if (const decl_base* ld = get_type_declaration(l))
	  if (const decl_base* rd = get_type_declaration(r))
	    {
	      // non-canonicalized anonymous types that are in
	      // different namespaces are different.
	      if (ld->get_is_anonymous() && rd->get_is_anonymous())
		if (ld->get_qualified_name(true) != rd->get_qualified_name(true))
		  return false;

	      return *ld == *rd;
	    }

	return *l == *r;
      }

    return false;
  }

  /// Equality operator between two types.
  ///
  /// @param l the first type to compare.
  ///
  /// @param r the second type to compare.
  ///
  /// @return true iff @p l equals @p r.
  bool
  operator()(type_base_sptr l, type_base_sptr r) const
  {return operator()(l.get(), r.get());}
};// end struct canonicalized_type_equal

/// The hashing functor of for the set of canonicalized types
struct canonicalized_type_hash
{
  size_t
  operator()(const type_base* t) const
  {
    t = get_exemplar_type(t);
    if (t->get_naked_canonical_type())
      return reinterpret_cast<size_t>(t);

    std::hash<string> h;
    string repr = t->get_pretty_representation(/*internal=*/false,
					       /*qualified_name=*/true);
    size_t result = h(repr);
    return result;
  }
}; //end struct canonicalized_type_hash

// A convenience typedef for a set of type_base*.
typedef std::unordered_set<const type_base*,
			   canonicalized_type_hash,
			   canonicalized_type_equal> type_ptr_set_type;

/// A convenience typedef for a map that associates a pointer to type
/// to a string.
typedef unordered_map<type_base*, interned_string,
		      canonicalized_type_hash,
		      canonicalized_type_equal> type_ptr_map;
/// A set meant to carry non canonicalized types.
///
/// Those types make the function is_non_canonicalized_type return
/// true.
typedef std::unordered_set<const type_base*,
			   non_canonicalized_type_hash,
			   non_canonicalized_type_equal>
nc_type_ptr_set_type;

/// A map meant to carry non canonicalized types as key.
///
/// Those types make the function is_non_canonicalized_type return
/// true.
typedef std::unordered_map<const type_base*, interned_string,
			   non_canonicalized_type_hash,
			   non_canonicalized_type_equal>
nc_type_ptr_istr_map_type;

/// A convenience typedef for a set of function type*.
typedef std::unordered_set<function_type*> fn_type_ptr_set_type;

struct function_tdecl_hash
{
  size_t operator()(const function_tdecl_sptr& f) const
  {return reinterpret_cast<size_t>(f.get());}
};

typedef unordered_map<function_tdecl_sptr,
		      string, function_tdecl_hash>
fn_tmpl_shared_ptr_map;

struct class_tdecl_hash
{
  size_t operator()(const class_tdecl_sptr& c) const
  {return reinterpret_cast<size_t>(c.get());}
};

typedef unordered_map<class_tdecl_sptr,
		      string,
		      class_tdecl_hash> class_tmpl_shared_ptr_map;

class write_context
{
  const environment&			m_env;
  id_manager				m_id_manager;
  ostream*				m_ostream;
  bool					m_annotate;
  bool					m_write_member_hashes;
  bool					m_show_locs;
  bool					m_write_architecture;
  bool					m_write_corpus_path;
  bool					m_write_comp_dir;
  bool					m_write_elf_needed;
  bool					m_write_undefined_symbols;
  bool					m_write_parameter_names;
  bool					m_short_locs;
  bool					m_write_default_sizes;
  bool					m_write_native_offsets;
  bool					m_write_non_reachable_types;
  type_id_style_kind			m_type_id_style;
  mutable type_ptr_map			m_type_id_map;
  // type id map for non-canonicalized types.
  mutable unordered_set<uint32_t>	m_used_type_id_hashes;
  mutable type_ptr_set_type		m_emitted_type_set;
  // A map of types that are referenced by emitted pointers,
  // references or typedefs
  type_ptr_set_type			m_referenced_types_set;
  fn_type_ptr_set_type			m_referenced_fn_types_set;
  fn_tmpl_shared_ptr_map		m_fn_tmpl_id_map;
  class_tmpl_shared_ptr_map		m_class_tmpl_id_map;
  string_elf_symbol_sptr_map_type	m_fun_symbol_map;
  string_elf_symbol_sptr_map_type	m_var_symbol_map;
  vars_set_type			m_emitted_var_decls_set;
  functions_set_type			m_emitted_function_decls_set;
  unordered_set<string>		m_emitted_corpora_set;

  write_context();

public:

  /// Constructor.
  ///
  /// @param env the enviroment we are operating from.
  ///
  /// @param os the output stream to write to.
  write_context(const environment& env, ostream& os)
    : m_env(env),
      m_id_manager(env),
      m_ostream(&os),
      m_annotate(false),
      m_write_member_hashes(false),
      m_show_locs(true),
      m_write_architecture(true),
      m_write_corpus_path(true),
      m_write_comp_dir(true),
      m_write_elf_needed(true),
      m_write_undefined_symbols(true),
      m_write_parameter_names(true),
      m_short_locs(false),
      m_write_default_sizes(true),
      m_write_native_offsets(false),
      m_write_non_reachable_types(false),
      m_type_id_style(SEQUENCE_TYPE_ID_STYLE)
  {}

  /// Getter of the environment we are operating from.
  ///
  /// @return the environment we are operating from.
  const environment&
  get_environment() const
  {return m_env;}

  const config&
  get_config() const
  {return get_environment().get_config();}

  /// Getter for the current ostream
  ///
  /// @return a reference to the current ostream
  ostream&
  get_ostream()
  {return *m_ostream;}

  /// Setter for the current ostream
  ///
  /// @param os the new ostream
  void
  set_ostream(ostream& os)
  {m_ostream = &os;}

  /// Getter of the annotation option.
  ///
  /// @return true iff ABIXML annotations are turned on
  bool
  get_annotate()
  {return m_annotate;}

  /// Setter of the annotation option.
  ///
  /// @param f the new value of the flag.
  void
  set_annotate(bool f)
  {m_annotate = f;}

  /// Getter of the "write-member-hashes" property.
  ///
  /// @return the parameter value.
  bool
  get_write_member_hashes()
  {return m_write_member_hashes;}

  /// Setter of the "write-member-hashes" property.
  ///
  /// @param f the new parameter value.
  void
  set_write_member_hashes(bool f)
  {m_write_member_hashes = f;}

  /// Getter of the write-architecture option.
  ///
  /// @return true iff architecture information shall be emitted
  bool
  get_write_architecture()
  {return m_write_architecture;}

  /// Setter of the write-architecture option
  ///
  /// @param f the new value of the flag.
  void
  set_write_architecture(bool f)
  {m_write_architecture = f;}

  /// Getter of the elf-needed option.
  ///
  /// @return true iff elf needed information shall be emitted
  bool
  get_write_elf_needed()
  {return m_write_elf_needed;}

  /// Setter of the elf-needed option.
  ///
  /// @param f the new value of the flag.
  void
  set_write_elf_needed(bool f)
  {m_write_elf_needed = f;}

  /// Getter of the "undefined-symbols" option.
  ///
  /// @return true iff undefined symbols shall be emitted.
  bool
  get_write_undefined_symbols() const
  {return m_write_undefined_symbols;}

  /// Setter of the "undefined-symbols" option.
  ///
  /// @param f true iff undefined symbols shall be emitted.
  void
  set_write_undefined_symbols(bool f)
  {m_write_undefined_symbols = f;}

  /// Getter of the default-sizes option.
  ///
  /// @return true iff default size-in-bits needs to be emitted
  bool
  get_write_default_sizes()
  {return m_write_default_sizes;}

  /// Setter of the default-sizes option.
  ///
  /// @param f the new value of the flag.
  void
  set_write_default_sizes(bool f)
  {m_write_default_sizes = f;}

  /// Getter of the write-corpus-path option.
  ///
  /// @return true iff corpus-path information shall be emitted
  bool
  get_write_corpus_path()
  {return m_write_corpus_path;}

  /// Setter of the write-corpus-path option
  ///
  /// @param f the new value of the flag.
  void
  set_write_corpus_path(bool f)
  {m_write_corpus_path = f;}

  /// Getter of the comp-dir-path option.
  ///
  /// @return true iff compilation dir information shall be emitted
  bool
  get_write_comp_dir()
  {return m_write_comp_dir;}

  /// Setter of the comp-dir-path option
  ///
  /// @param f the new value of the flag.
  void
  set_write_comp_dir(bool f)
  {m_write_comp_dir = f;}

  /// Getter of the short-locs option.
  ///
  /// @return true iff short locations shall be emitted
  bool
  get_short_locs()
  {return m_short_locs;}

  /// Setter of the short-locs option
  ///
  /// @param f the new value of the flag.
  void
  set_short_locs(bool f)
  {m_short_locs = f;}

  /// Getter of the parameter-names option.
  ///
  /// @return true iff parameter names shall be emitted
  bool
  get_write_parameter_names() const
  {return m_write_parameter_names;}

  /// Setter of the parameter-names option
  ///
  /// @param f the new value of the flag.
  void
  set_write_parameter_names(bool f)
  {m_write_parameter_names = f;}

  /// Setter of the "write-native-offset" flag.
  ///
  /// @param flag the new value of the flag.
  void
  set_write_native_offsets(bool flag)
  {m_write_native_offsets = flag;}

  /// Getter of the "write-native-offset" flag.
  ///
  /// @return the value of the flag.
  bool
  get_write_native_offsets()
  {return m_write_native_offsets;}

  /// Getter of the "write-non-reachable-types" flag.
  ///
  /// @return the value of the flag.
  bool
  get_write_non_reachable_types() const
  {return m_write_non_reachable_types;}

  /// Setter of the "write-non-reachable-types" flag.
  ///
  /// @param flag the new value of the flag.
  void
  set_write_non_reachable_types(bool flag)
  {m_write_non_reachable_types = flag;}

  /// Getter of the "show-locs" option.
  ///
  /// When this option is true then the XML writer emits location
  /// information for emitted ABI artifacts.
  ///
  /// @return the value of the "show-locs" option.
  bool
  get_show_locs() const
  {return m_show_locs;}

  /// Setter of the "show-locs" option.
  ///
  /// When this option is true then the XML writer emits location
  /// information for emitted ABI artifacts.
  ///
  /// @param f the new value of the "show-locs" option.
  void
  set_show_locs(bool f)
  {m_show_locs = f;}

  /// Getter of the "type-id-style" option.
  ///
  /// This option controls the kind of type ids used in XML output.
  ///
  /// @return the value of the "type-id-style" option.
  type_id_style_kind
  get_type_id_style() const
  {return m_type_id_style;}

  /// Setter of the "type-id-style" option.
  ///
  /// This option controls the kind of type ids used in XML output.
  ///
  /// @param style the new value of the "type-id-style" option.
  void
  set_type_id_style(type_id_style_kind style)
  {m_type_id_style = style;}

  /// Getter of the @ref id_manager.
  ///
  /// @return the @ref id_manager used by the current instance of @ref
  /// write_context.
  const id_manager&
  get_id_manager() const
  {return m_id_manager;}

  id_manager&
  get_id_manager()
  {return m_id_manager;}

  /// @return true iff type has already been assigned an ID.
  bool
  type_has_existing_id(type_base_sptr type) const
  {return type_has_existing_id(type.get());}

  /// @return true iff type has already been assigned an ID.
  bool
  type_has_existing_id(type_base* type) const
  {
    type = get_exemplar_type(type);
    return m_type_id_map.find(type) != m_type_id_map.end();
  }

  /// Associate a unique id to a given type.  For that, put the type
  /// in a hash table, hashing the type.  So if the type has no id
  /// associated to it, create a new one and return it.  Otherwise,
  /// return the existing id for that type.
  interned_string
  get_id_for_type(const type_base_sptr& t)
  {return get_id_for_type(t.get());}

  /// Associate a unique id to a given type.  For that, put the type
  /// in a hash table, hashing the type.  So if the type has no id
  /// associated to it, create a new one and return it.  Otherwise,
  /// return the existing id for that type.
  interned_string
  get_id_for_type(const type_base* type) const
  {
    type_base* c = get_exemplar_type(type);

    auto it = m_type_id_map.find(c);
    if (it != m_type_id_map.end())
      return it->second;

    switch (m_type_id_style)
      {
      case SEQUENCE_TYPE_ID_STYLE:
	{
	  interned_string id = get_id_manager().get_id_with_prefix("type-id-");
	  return m_type_id_map[c] = id;
	}
      case HASH_TYPE_ID_STYLE:
	{
	  interned_string pretty = c->get_cached_pretty_representation(true);
	  size_t hash = hashing::fnv_hash(pretty);
	  while (!m_used_type_id_hashes.insert(hash).second)
	    ++hash;
	  std::ostringstream os;
	  os << std::hex << std::setfill('0') << std::setw(8) << hash;
	  return m_type_id_map[c] = c->get_environment().intern(os.str());
	}
      }
    ABG_ASSERT_NOT_REACHED;
    return interned_string();
  }

  string
  get_id_for_fn_tmpl(const function_tdecl_sptr& f)
  {
    fn_tmpl_shared_ptr_map::const_iterator it = m_fn_tmpl_id_map.find(f);
    if (it == m_fn_tmpl_id_map.end())
      {
	string id = get_id_manager().get_id_with_prefix("fn-tmpl-id-");
	m_fn_tmpl_id_map[f] = id;
	return id;
      }
    return m_fn_tmpl_id_map[f];
  }

  string
  get_id_for_class_tmpl(const class_tdecl_sptr& c)
  {
    class_tmpl_shared_ptr_map::const_iterator it = m_class_tmpl_id_map.find(c);
    if (it == m_class_tmpl_id_map.end())
      {
	string id = get_id_manager().get_id_with_prefix("class-tmpl-id-");
	m_class_tmpl_id_map[c] = id;
	return id;
      }
    return m_class_tmpl_id_map[c];
  }

  void
  clear_type_id_map()
  {
    m_type_id_map.clear();
  }


  /// Getter of the set of types that were referenced by a pointer,
  /// reference or typedef.
  ///
  /// This set contains only types that do have canonical types and
  /// which are not function types.
  ///
  /// @return the set of types that were referenced.
  const type_ptr_set_type&
  get_referenced_types() const
  {return m_referenced_types_set;}

  /// Getter of the set of function types that were referenced by a
  /// pointer, reference or typedef.
  ///
  /// @return the set of function types that were referenced.
  const fn_type_ptr_set_type&
  get_referenced_function_types() const
  {return m_referenced_fn_types_set;}

  /// Test if there are non emitted referenced types.
  ///
  /// @return true iff there are non emitted referenced types.
  bool
  has_non_emitted_referenced_types() const
  {
    for (const auto t : get_referenced_types())
      if (!type_is_emitted(t))
	  return true;

    for (const auto t : get_referenced_function_types())
      if (!type_is_emitted(t))
	return true;

    return false;
  }

  /// Record a given type as being referenced by a pointer, a
  /// reference or a typedef type that is being emitted to the XML
  /// output.
  ///
  /// @param t a shared pointer to a type
  void
  record_type_as_referenced(const type_base_sptr& type)
  {
    type_base* t = get_exemplar_type(type.get());
    if (t && !m_write_non_reachable_types)
      ABG_ASSERT(type_is_reachable_from_public_interfaces(*t));

    // If the type is a function type, record it in a dedicated data
    // structure.
    if (function_type* f = is_function_type(t))
      m_referenced_fn_types_set.insert(f);
    else
      m_referenced_types_set.insert(t);
  }

  /// Test if a given type has been referenced by a pointer, a
  /// reference or a typedef type that was emitted to the XML output.
  ///
  /// @param f a shared pointer to a type
  ///
  /// @return true if the type has been referenced, false
  /// otherwise.
  bool
  type_is_referenced(const type_base_sptr& type)
  {
    type_base* t = get_exemplar_type(type.get());
    if (function_type* f = is_function_type(t))
      return (m_referenced_fn_types_set.find(f)
	      != m_referenced_fn_types_set.end());
    else
      return m_referenced_types_set.find(t) != m_referenced_types_set.end();
  }

  /// Sort the content of a set of type pointers into a vector.
  ///
  /// The pointers are sorted by using their string representation as
  /// the key to sort, lexicographically.
  ///
  /// @param types the map to sort.
  ///
  /// @param sorted the resulted sorted vector.  It's set by this
  /// function with the result of the sorting.
  void
  sort_types(type_ptr_set_type& types, vector<type_base*>& sorted)
  {
    string id;
    for (type_ptr_set_type::const_iterator i = types.begin();
	 i != types.end();
	 ++i)
      sorted.push_back(const_cast<type_base*>(*i));
    type_topo_comp comp;
    sort(sorted.begin(), sorted.end(), comp);
  }

  /// Sort the content of a map of type pointers into a vector.
  ///
  /// The pointers are sorted by using their string representation as
  /// the key to sort, lexicographically.
  ///
  /// @param types the map to sort.
  ///
  /// @param sorted the resulted sorted vector.  It's set by this
  /// function with the result of the sorting.
  void
  sort_types(const type_sptr_set_type& types, vector<type_base_sptr>& sorted)
  {
    string id;
    for (auto& type : types)
      sorted.push_back(type);
    type_topo_comp comp;
    sort(sorted.begin(), sorted.end(), comp);
  }

  /// Sort the content of a map of type pointers into a vector.
  ///
  /// The pointers are sorted by using their string representation as
  /// the key to sort, lexicographically.
  ///
  /// @param types the map to sort.
  ///
  /// @param sorted the resulted sorted vector.  It's set by this
  /// function with the result of the sorting.
  void
  sort_types(const istring_type_base_wptr_map_type& types,
	     vector<type_base_sptr> &sorted)
  {
    for (istring_type_base_wptr_map_type::const_iterator i = types.begin();
	 i != types.end();
	 ++i)
      sorted.push_back(type_base_sptr(i->second));
    type_topo_comp comp;
    sort(sorted.begin(), sorted.end(), comp);
  }

  /// Sort the content of a vector of function types into a vector of
  /// types.
  ///
  /// The pointers are sorted by using their string representation as
  /// the key to sort, lexicographically.
  ///
  /// @param types the vector of function types to store.
  ///
  /// @param sorted the resulted sorted vector.  It's set by this
  /// function with the result of the sorting.
  void
  sort_types(const vector<function_type_sptr>& types,
	     vector<type_base_sptr> &sorted)
  {
    for (vector<function_type_sptr>::const_iterator i = types.begin();
	 i != types.end();
	 ++i)
      sorted.push_back(*i);
    type_topo_comp comp;
    sort(sorted.begin(), sorted.end(), comp);
  }

  /// Flag a type as having been written out to the XML output.
  ///
  /// @param t the type to flag.
  void
  record_type_as_emitted(const type_base_sptr &t)
  {record_type_as_emitted(t.get());}

  /// Flag a type as having been written out to the XML output.
  ///
  /// @param t the type to flag.
  void
  record_type_as_emitted(const type_base* t)
  {
    type_base* c = get_exemplar_type(t);
    m_emitted_type_set.insert(c);
  }

  /// Test if a given type has been written out to the XML output.
  ///
  /// @param the type to test for.
  ///
  /// @return true if the type has already been emitted, false
  /// otherwise.
  bool
  type_is_emitted(const type_base* t) const
  {
    type_base* c = get_exemplar_type(t);
    return (m_emitted_type_set.find(c) != m_emitted_type_set.end());
  }

  /// Test if a given type has been written out to the XML output.
  ///
  /// @param the type to test for.
  ///
  /// @return true if the type has already been emitted, false
  /// otherwise.
  bool
  type_is_emitted(const type_base_sptr& t) const
  {return type_is_emitted(t.get());}

  /// Test if a given type is to be emitted to the ABIXML output.
  ///
  /// @param t the type to test for.
  ///
  /// @return true if @p t is to be emitted to the ABIXML output.
  bool
  type_to_be_emitted(const type_base* t) const
  {
    if (!t || type_is_emitted(t))
      return false;

    const corpus* abi = t->get_corpus();
    if (abi)
      if (!get_write_non_reachable_types()
	  && !type_is_reachable_from_public_interfaces(*t))
	return false;

    return true;
  }

  /// Test if a given type is to be emitted to the ABIXML output.
  ///
  /// @param t the type to test for.
  ///
  /// @return true if @p t is to be emitted to the ABIXML output.
  bool
  type_to_be_emitted(const type_base_sptr t) const
  {return type_to_be_emitted(t.get());}

  /// Test if a given decl has been written out to the XML output.
  ///
  /// @param the decl to consider.
  ///
  /// @return true if the decl has already been emitted, false
  /// otherwise.
  bool
  decl_is_emitted(const decl_base& decl) const
  {
    return decl_is_emitted(&decl);
  }

  /// Test if a given decl has been written out to the XML output.
  ///
  /// @param the decl to consider.
  ///
  /// @return true if the decl has already been emitted, false
  /// otherwise.
  bool
  decl_is_emitted(const decl_base* decl) const
  {
    bool result = false;
    if (const var_decl *var = is_var_decl(decl))
      result = (m_emitted_var_decls_set.find(var)
		!= m_emitted_var_decls_set.end());
    else if (function_decl *fn = is_function_decl(decl))
      result = (m_emitted_function_decls_set.find(fn)
		!= m_emitted_function_decls_set.end());
    return result;
  }

  /// Test if a given decl has been written out to the XML output.
  ///
  /// @param the decl to consider.
  ///
  /// @return true if the decl has already been emitted, false
  /// otherwise.
  bool
  decl_is_emitted(const decl_base_sptr& decl) const
  {
    return decl_is_emitted(decl.get());
  }

  /// Record a declaration as emitted in the abixml output.
  ///
  /// @param decl the decl to consider.
  void
  record_decl_as_emitted(const decl_base_sptr& decl)
  {
    if (var_decl_sptr var = is_var_decl(decl))
      m_emitted_var_decls_set.insert(var.get());
    else if (function_decl_sptr fn = is_function_decl(decl))
      m_emitted_function_decls_set.insert(fn.get());
    else
      ABG_ASSERT_NOT_REACHED;
  }

  /// Test if a corpus has already been emitted.
  ///
  /// A corpus is emitted if it's been recorded as having been emitted
  /// by the function record_corpus_as_emitted().
  ///
  /// @param corp the corpus to consider.
  ///
  /// @return true iff the corpus @p corp has been emitted.
  bool
  corpus_is_emitted(const corpus_sptr& corp)
  {
    if (!corp)
      return false;

    if (m_emitted_corpora_set.find(corp->get_path())
	== m_emitted_corpora_set.end())
      return false;

    return true;
  }

  /// Record the corpus has having been emitted.
  ///
  /// @param corp the corpus to consider.
  void
  record_corpus_as_emitted(const corpus_sptr& corp)
  {
    if (!corp)
      return;

    const string& path = corp->get_path();
    if (!path.empty())
      m_emitted_corpora_set.insert(path);
  }

  /// Get the set of types that have been emitted.
  ///
  /// @return the set of types that have been emitted.
  const type_ptr_set_type&
  get_emitted_types_set() const
  {return m_emitted_type_set;}

  /// Clear the map that contains the IDs of the types that has been
  /// recorded as having been written out to the XML output.
  void
  clear_referenced_types()
  {
    m_referenced_types_set.clear();
    m_referenced_fn_types_set.clear();
  }

  const string_elf_symbol_sptr_map_type&
  get_fun_symbol_map() const
  {return m_fun_symbol_map;}

  string_elf_symbol_sptr_map_type&
  get_fun_symbol_map()
  {return m_fun_symbol_map;}

};//end write_context

static void write_location(const location&, write_context&);
static void write_location(const decl_base_sptr&, write_context&);
static bool write_visibility(const decl_base_sptr&, ostream&);
static bool write_binding(const decl_base_sptr&, ostream&);
static bool write_is_artificial(const decl_base_sptr&, ostream&);
static bool write_is_non_reachable(const type_base_sptr&, ostream&);
static bool write_tracking_non_reachable_types(const corpus_sptr&, ostream&);
static void write_array_size_and_alignment(const array_type_def_sptr,
					   ostream&);
static void write_size_and_alignment(const type_base_sptr, ostream&,
				     size_t default_size = 0,
				     size_t default_alignment = 0);
static void write_access(access_specifier, ostream&);
static void write_layout_offset(var_decl_sptr, ostream&);
static void write_layout_offset(class_decl::base_spec_sptr, ostream&);
static void write_cdtor_const_static(bool, bool, bool, bool, ostream&);
static void write_voffset(function_decl_sptr, ostream&);
static void write_elf_symbol_type(elf_symbol::type, ostream&);
static void write_elf_symbol_binding(elf_symbol::binding, ostream&);
static bool write_elf_symbol_aliases(const elf_symbol&, ostream&);
static bool write_elf_symbol_reference(write_context&,
				       const elf_symbol&,
				       const corpus& abi,
				       ostream&);
static bool write_elf_symbol_reference(write_context&,
				       const elf_symbol_sptr,
				       const corpus& abi,
				       ostream&);
static void write_is_declaration_only(const decl_base_sptr&, ostream&);
static void write_is_struct(const class_decl_sptr&, ostream&);
static void write_is_anonymous(const decl_base_sptr&, ostream&);
static void write_type_hash_and_cti(const type_base_sptr&, ostream&);
static bool write_decl(const decl_base_sptr&, write_context&, unsigned);
static void write_artifact_in_scope(const type_or_decl_base_sptr&,
				    write_context&, unsigned);
static bool write_type_decl(const type_decl_sptr&, write_context&, unsigned);
static void write_decls_from_scope(write_context&,
				   const scope_decl&,
				   const unsigned);
static bool write_namespace_decl(const namespace_decl_sptr&,
				 write_context&, unsigned);
static bool write_qualified_type_def(const qualified_type_def_sptr&,
				     write_context&, unsigned);
static bool write_pointer_type_def(const pointer_type_def_sptr&,
				   write_context&, unsigned);
static bool write_reference_type_def(const reference_type_def_sptr&,
				     write_context&, unsigned);
static bool write_ptr_to_mbr_type(const ptr_to_mbr_type_sptr&,
				  write_context&, unsigned);
static bool write_array_type_def(const array_type_def_sptr&,
			         write_context&, unsigned);
static bool write_array_subrange_type(const array_type_def::subrange_sptr&,
				      write_context&,
				      unsigned);
static bool write_enum_type_decl(const enum_type_decl_sptr&,
				 write_context&, unsigned);
static bool write_typedef_decl(const typedef_decl_sptr&,
			       write_context&, unsigned);
static bool write_elf_symbol(const elf_symbol_sptr&,
			     write_context&, unsigned);
static bool write_elf_symbols_table(const elf_symbols&,
				    write_context&, unsigned);
static bool write_var_decl(const var_decl_sptr&,
			   write_context&, bool, unsigned);
static bool write_function_decl_opening_tag(const function_decl_sptr&,
					    write_context&,
					    unsigned);
static bool write_function_decl_closing_tag(const function_decl_sptr&,
					    write_context&,
					    unsigned indent);
static void write_fn_parm_and_return_types(const function_type_sptr fun_type,
					   bool skip_first_parm,
					   write_context& ctxt,
					   unsigned indent);
static void write_function_member_decls(const function_decl_sptr& decl,
					write_context& ctxt, unsigned indent);
static bool write_function_decl(const function_decl_sptr&, write_context&, unsigned);
static bool write_function_type(const function_type_sptr&,
				write_context&, unsigned);
static bool write_member_type_opening_tag(const type_base_sptr&,
					  write_context&, unsigned);
static bool write_member_function_opening_tag(const function_decl_sptr&,
					      write_context&, unsigned);
static bool write_member_function_closing_tag(const function_decl_sptr& fn_decl,
					      write_context& ctxt, unsigned indent);
static bool write_member_type(const type_base_sptr&,
			      write_context&, unsigned);
static bool write_member_function(const function_decl_sptr&,
				  write_context&, unsigned);
static string maybe_emit_type_hash(write_context&, type_base_sptr);
static bool write_class_decl_opening_tag(const class_decl_sptr&, const string&,
					 write_context&, unsigned, bool);
static bool write_class_decl(const class_decl_sptr&,
			     write_context&, unsigned);
static bool write_union_decl_opening_tag(const union_decl_sptr&, const string&,
					 write_context&, unsigned, bool);
static bool write_union_decl(const union_decl_sptr&, const string&,
			     write_context&, unsigned);
static bool write_union_decl(const union_decl_sptr&, write_context&, unsigned);
static void write_common_type_info(const type_base_sptr&, write_context&,
				   const string& id="");
static bool write_type(const type_base_sptr& t,
		       write_context& ctxt,
		       unsigned indent);
static bool write_type_tparameter
(const shared_ptr<type_tparameter>, write_context&, unsigned);
static bool write_non_type_tparameter
(const shared_ptr<non_type_tparameter>, write_context&, unsigned);
static bool write_template_tparameter
(const shared_ptr<template_tparameter>, write_context&, unsigned);
static bool write_type_composition
(const shared_ptr<type_composition>, write_context&, unsigned);
static bool write_template_parameter(const shared_ptr<template_parameter>,
				     write_context&, unsigned);
static void write_template_parameters(const shared_ptr<template_decl>,
				      write_context&, unsigned);
static bool write_function_tdecl
(const shared_ptr<function_tdecl>,
 write_context&, unsigned);
static bool write_class_tdecl
(const shared_ptr<class_tdecl>,
 write_context&, unsigned);
static void	do_indent(ostream&, unsigned);
static void	do_indent_to_level(write_context&, unsigned, unsigned);
static unsigned get_indent_to_level(write_context&, unsigned, unsigned);
static decl_base* get_variant_of_decl_from_abi(const write_context&,
					       const decl_base_sptr);
static bool scope_is_essentially_empty(const scope_decl *scope,
				       const write_context& ctxt);
static bool scope_is_essentially_empty(const scope_decl_sptr& decl,
				       const write_context& ctxt);
/// Emit nb_whitespaces white spaces into the output stream.
void
do_indent(ostream& o, unsigned nb_whitespaces)
{
  for (unsigned i = 0; i < nb_whitespaces; ++i)
    o << ' ';
}

/// Indent initial_indent + level number of xml element indentation.
///
/// @param ctxt the context of the parsing.
///
/// @param initial_indent the initial number of white space to indent to.
///
/// @param level the number of indentation level to indent to.
static void
do_indent_to_level(write_context& ctxt,
		   unsigned initial_indent,
		   unsigned level)
{
  do_indent(ctxt.get_ostream(),
	    get_indent_to_level(ctxt, initial_indent, level));
}

/// Return the number of white space of indentation that
/// #do_indent_to_level would have used.
///
/// @param ctxt the context of the parsing.
///
/// @param initial_indent the initial number of white space to indent to.
///
/// @param level the number of indentation level to indent to.
static unsigned
get_indent_to_level(write_context& ctxt, unsigned initial_indent,
		    unsigned level)
{
    int nb_ws = initial_indent +
      level * ctxt.get_config().get_xml_element_indent();
    return nb_ws;
}

/// Annotate a declaration in form of an ABIXML comment.
///
/// This function is further specialized for declarations and types
/// with special requirements.
///
/// @tparam T shall be of type decl_base_sptr or a shared pointer to a
/// type derived from it, for the instantiation to be syntactically
/// correct.
///
/// @param decl_sptr the shared pointer to the declaration of type T.
///
/// @param ctxt the context of the parsing.
///
/// @param indent the amount of white space to indent to.
///
/// @param addendum the addendum annotation to append to the main one.
///
/// @return true iff decl is valid.
template <typename T>
static bool
annotate(const T&	decl,
	 write_context& ctxt,
	 unsigned	indent,
	 const string&	addendum = "")
{
  if (!decl)
    return false;

  if (!ctxt.get_annotate())
    return true;

  ostream& o = ctxt.get_ostream();

  do_indent(o, indent);

  string decl_repr = decl->get_pretty_representation(/*internal=*/false);
  if (!addendum.empty())
    decl_repr += addendum;

  o << "<!-- "
    << xml::escape_xml_comment(decl_repr)
    << " -->\n";

  return true;
}

/// Annotate an elf symbol in form of an ABIXML comment, effectively
/// writing out its demangled form.
///
/// @param sym the symbol, whose name should be demangled.
///
/// @param ctxt the context of the parsing.
///
/// @param indent the amount of white space to indent to.
///
/// @param addendum the addendum annotation to append to the main one.
///
/// @return true iff decl is valid
template<>
bool
annotate(const elf_symbol_sptr& sym,
	 write_context&	ctxt,
	 unsigned		indent,
	 const string&		addendum)
{
  if (!sym)
    return false;

  if (!ctxt.get_annotate())
    return true;

  ostream& o = ctxt.get_ostream();

  string elf_symbol_repr =
    abigail::ir::demangle_cplus_mangled_name(sym->get_name());

  if (!addendum.empty())
    elf_symbol_repr += addendum;

  do_indent(o, indent);
  o << "<!-- "
    << xml::escape_xml_comment(elf_symbol_repr)
    << " -->\n";

  return true;
}

/// Annotate a typedef declaration in form of an ABIXML comment.
///
/// @param typedef_decl the typedef to annotate.
///
/// @param ctxt the context of the parsing.
///
/// @param indent the amount of white space to indent to.
///
/// @param addendum the addendum annotation to append to the main one.
///
/// @return true iff decl is valid
template<>
bool
annotate(const typedef_decl_sptr&	typedef_decl,
	 write_context&		ctxt,
	 unsigned			indent,
	 const string&			addendum)
{
  if (!typedef_decl)
    return false;

  if (!ctxt.get_annotate())
    return true;

  ostream& o = ctxt.get_ostream();

  do_indent(o, indent);

  o << "<!-- typedef "
    << get_type_name(typedef_decl->get_underlying_type())
    << " "
    << get_type_name(typedef_decl)
    << addendum
    << " -->\n";

  return true;
}

/// Annotate a function type in form of an ABIXML comment.
///
/// @param function_type the function type to annotate.
///
/// @param ctxt the context of the parsing.
///
/// @param indent the amount of white space to indent to.
///
/// @param skip_first_parm if true, do not serialize the first
/// parameter of the function decl.
///
/// @param addendum the addendum annotation to append to the main one.
///
/// @return true iff decl is valid
bool
annotate(const function_type_sptr&	function_type,
	 write_context&		ctxt,
	 unsigned			indent,
	 const string&			addendum = "")
{
  if (!function_type)
    return false;

  if (!ctxt.get_annotate())
    return true;

  ostream& o = ctxt.get_ostream();

  do_indent(o, indent);

  string repr = get_function_type_name(function_type) + addendum;

  o << "<!-- "
    << xml::escape_xml_comment(repr)
    << " -->\n";
  return true;
}

/// Annotate a function declaration in form of an ABIXML comment.
///
/// @param fn the function decl to annotate.
///
/// @param ctxt the context of the parsing.
///
/// @param indent the amount of white space to indent to.
///
/// @param addendum the addendum annotation to append to the main one.
///
/// @return true iff decl is valid
static bool
annotate(const function_decl_sptr&	fn,
	 write_context&		ctxt,
	 unsigned			indent,
	 const string&			addendum = "")
{
  if (!fn)
    return false;

  if (!ctxt.get_annotate())
    return true;

  ostream& o = ctxt.get_ostream();

  do_indent(o, indent);
  o << "<!-- ";

  if (is_member_function(fn)
      && (get_member_function_is_ctor(fn) || get_member_function_is_dtor(fn)))
    ; // we don't emit return types for ctor or dtors
  else
    o << xml::escape_xml_comment(get_type_name(fn->get_return_type()))
      << " ";

  o << xml::escape_xml_comment(fn->get_qualified_name()) << "(";

  vector<function_decl::parameter_sptr>::const_iterator pi =
    fn->get_first_non_implicit_parm();

  for (; pi != fn->get_parameters().end(); ++pi)
    {
      o << xml::escape_xml_comment((*pi)->get_type_name());
      // emit a comma after a param type, unless it's the last one
      if (distance(pi, fn->get_parameters().end()) > 1)
	o << ", ";
    }

  o << ")"
    << addendum
    << " -->\n";

  return true;
}

/// Annotate a function parameter in form of an ABIXML comment.
///
/// @param parm the function parameter to annotate.
///
/// @param ctxt the context of the parsing.
///
/// @param indent the amount of white space to indent to.
///
/// @param addendum the addendum annotation to append to the main one.
///
/// @return true iff decl is valid
template<>
bool
annotate(const function_decl::parameter_sptr&	parm,
	 write_context&			ctxt,
	 unsigned				indent,
	 const string&				addendum)
{
  if (!parm)
    return false;

  if (!ctxt.get_annotate())
    return true;

  ostream &o = ctxt.get_ostream();

  do_indent(o, indent);

  o << "<!-- ";

  if (parm->get_variadic_marker())
    o << "variadic parameter";
  else
    {
      if (parm->get_is_artificial())
	{
	  if (parm->get_index() == 0)
	    o << "implicit ";
	  else
	    o << "artificial ";
	}
      o << "parameter of type '"
	<< xml::escape_xml_comment(get_pretty_representation(parm->get_type()));
    }

  o << "'"
    << addendum
    << " -->\n";

  return true;
}

/// Write a location to the output stream.
///
/// If the location is empty, nothing is written.
///
/// @param loc the location to consider.
///
/// @param tu the translation unit the location belongs to.
///
/// @param ctxt the writer context to use.
static void
write_location(const location& loc, write_context& ctxt)
{
  if (!loc || loc.get_is_artificial())
    return;

  if (!ctxt.get_show_locs())
    return;

  string filepath;
  unsigned line = 0, column = 0;

  loc.expand(filepath, line, column);

  ostream &o = ctxt.get_ostream();

  if (ctxt.get_short_locs())
    tools_utils::base_name(filepath, filepath);

  o << " filepath='" << xml::escape_xml_string(filepath) << "'"
    << " line='"     << line     << "'"
    << " column='"   << column   << "'";
}

/// Write the location of a decl to the output stream.
///
/// If the location is empty, nothing is written.
///
/// @param decl the decl to consider.
///
/// @param ctxt the @ref writer_context to use.
static void
write_location(const decl_base_sptr&	decl,
	       write_context&		ctxt)
{
  if (!decl)
    return;

  location loc = decl->get_location();
  if (!loc)
    return;

  write_location(loc, ctxt);
}

/// Serialize the visibility property of the current decl as the
/// 'visibility' attribute for the current xml element.
///
/// @param decl the instance of decl_base to consider.
///
/// @param o the output stream to serialize the property to.
///
/// @return true upon successful completion, false otherwise.
static bool
write_visibility(const shared_ptr<decl_base>&	decl, ostream& o)
{
  if (!decl)
    return false;

  decl_base::visibility v = decl->get_visibility();
  string str;

  switch (v)
    {
    case decl_base::VISIBILITY_NONE:
      return true;
    case decl_base::VISIBILITY_DEFAULT:
      str = "default";
      break;
    case decl_base::VISIBILITY_PROTECTED:
      str = "protected";
      break;
    case decl_base::VISIBILITY_HIDDEN:
      str = "hidden";
      break;
    case decl_base::VISIBILITY_INTERNAL:
	str = "internal";
	break;
    }

  if (str.empty())
    return false;

  o << " visibility='" << str << "'";

  return true;
}

/// Serialize the 'binding' property of the current decl.
///
/// @param decl the decl to consider.
///
/// @param o the output stream to serialize the property to.
static bool
write_binding(const shared_ptr<decl_base>& decl, ostream& o)
{
  if (!decl)
    return false;

  decl_base::binding bind = decl_base::BINDING_NONE;

  shared_ptr<var_decl> var =
    dynamic_pointer_cast<var_decl>(decl);
  if (var)
    bind = var->get_binding();
  else
    {
      shared_ptr<function_decl> fun =
	dynamic_pointer_cast<function_decl>(decl);
      if (fun)
	bind = fun->get_binding();
    }

  string str;
  switch (bind)
    {
    case decl_base::BINDING_NONE:
      break;
    case decl_base::BINDING_LOCAL:
      str = "local";
      break;
    case decl_base::BINDING_GLOBAL:
	str = "global";
      break;
    case decl_base::BINDING_WEAK:
      str = "weak";
      break;
    }

  if (!str.empty())
    o << " binding='" << str << "'";

  return true;
}

/// Write the "is-artificial" attribute of the @ref decl.
///
/// @param decl the declaration to consider.
///
/// @param o the output stream to emit the "is-artificial" attribute
/// to.
///
/// @return true iff the "is-artificial" attribute was emitted.
static bool
write_is_artificial(const decl_base_sptr& decl, ostream& o)
{
  if (!decl)
    return false;

  if (decl->get_is_artificial())
    o << " is-artificial='yes'";

  return true;
}

/// Write the 'is-non-reachable' attribute if a given type we are
/// looking at is not reachable from global functions and variables
/// and if the user asked us to track that information.
///
/// @param t the type to consider.
///
/// @param o the output stream to write the 'is-non-reachable'
/// attribute to.
static bool
write_is_non_reachable(const type_base_sptr& t, ostream& o)
{
  if (!t)
    return false;

  corpus* c = t->get_corpus();
  if (!c || !c->recording_types_reachable_from_public_interface_supported())
    return false;

  if (!type_is_reachable_from_public_interfaces(*t))
    o << " is-non-reachable='yes'";

  return true;
}

/// Write the 'tracking-non-reachable-types' attribute if for a given
/// corpus, the user wants us to track non-reachable types.
///
/// @param corpus the ABI corpus to consider.
///
/// @param o the output parameter to write the
/// 'tracking-non-reachable-types' attribute to.
static bool
write_tracking_non_reachable_types(const corpus_sptr& corpus,
				   ostream& o)
{
  corpus_group* group = corpus->get_group();
  if (!group)
    if (corpus->recording_types_reachable_from_public_interface_supported())
      {
	o << " tracking-non-reachable-types='yes'";
	return true;
      }

  return false;
}

/// Serialize the size and alignment attributes of a given type.
///
/// @param decl the type to consider.
///
/// @param o the output stream to serialize to.
///
/// @param default_size size in bits that is the default for the type.
///                     No size-in-bits attribute is written if it
///                     would be the default value.
///
/// @param default_alignment alignment in bits that is the default for
///                     the type.  No alignment-in-bits attribute is
///                     written if it would be the default value.
static void
write_size_and_alignment(const shared_ptr<type_base> decl, ostream& o,
			 size_t default_size, size_t default_alignment)
{
  size_t size_in_bits = decl->get_size_in_bits();
  if (size_in_bits != default_size)
    o << " size-in-bits='" << size_in_bits << "'";

  size_t alignment_in_bits = decl->get_alignment_in_bits();
  if (alignment_in_bits != default_alignment)
    o << " alignment-in-bits='" << alignment_in_bits << "'";
}

/// Serialize the size and alignment attributes of a given type.
/// @param decl the type to consider.
///
/// @param o the output stream to serialize to.
static void
write_array_size_and_alignment(const shared_ptr<array_type_def> decl, ostream& o)
{
  if (decl->is_non_finite())
    o << " size-in-bits='" << "unknown" << "'";
  else {
    size_t size_in_bits = decl->get_size_in_bits();
    if (size_in_bits)
      o << " size-in-bits='" << size_in_bits << "'";
  }

  size_t alignment_in_bits = decl->get_alignment_in_bits();
  if (alignment_in_bits)
    o << " alignment-in-bits='" << alignment_in_bits << "'";
}
/// Serialize the access specifier.
///
/// @param a the access specifier to serialize.
///
/// @param o the output stream to serialize it to.
static void
write_access(access_specifier a, ostream& o)
{
  string access_str = "private";

  switch (a)
    {
    case private_access:
      access_str = "private";
      break;

    case protected_access:
      access_str = "protected";
      break;

    case public_access:
      access_str = "public";
      break;

    default:
      break;
    }

  o << " access='" << access_str << "'";
}

/// Serialize the layout offset of a data member.
static void
write_layout_offset(var_decl_sptr member, ostream& o)
{
  if (!is_data_member(member))
    return;

  if (get_data_member_is_laid_out(member))
    o << " layout-offset-in-bits='"
      << get_data_member_offset(member)
      << "'";
}

/// Serialize the layout offset of a base class
static void
write_layout_offset(shared_ptr<class_decl::base_spec> base, ostream& o)
{
  if (!base)
    return;

  if (base->get_offset_in_bits() >= 0)
    o << " layout-offset-in-bits='" << base->get_offset_in_bits() << "'";
}

/// Serialize the access specifier of a class member.
///
/// @param member a pointer to the class member to consider.
///
/// @param o the ostream to serialize the member to.
static void
write_access(decl_base_sptr member, ostream& o)
{write_access(get_member_access_specifier(member), o);}

/// Write the voffset of a member function if it's non-zero
///
/// @param fn the member function to consider
///
/// @param o the output stream to write to
static void
write_voffset(function_decl_sptr fn, ostream&o)
{
  if (!fn)
    return;

  if (get_member_function_is_virtual(fn))
    {
      ssize_t voffset = get_member_function_vtable_offset(fn);
      o << " vtable-offset='" << voffset << "'";
    }
}

/// Serialize an elf_symbol::type into an XML node attribute named
/// 'type'.
///
/// @param t the elf_symbol::type to serialize.
///
/// @param o the output stream to serialize it to.
static void
write_elf_symbol_type(elf_symbol::type t, ostream& o)
{
  string repr;

  switch (t)
    {
    case elf_symbol::NOTYPE_TYPE:
      repr = "no-type";
      break;
    case elf_symbol::OBJECT_TYPE:
      repr = "object-type";
      break;
    case elf_symbol::FUNC_TYPE:
      repr = "func-type";
      break;
    case elf_symbol::SECTION_TYPE:
      repr = "section-type";
      break;
    case elf_symbol::FILE_TYPE:
      repr = "file-type";
      break;
    case elf_symbol::COMMON_TYPE:
      repr = "common-type";
      break;
    case elf_symbol::TLS_TYPE:
      repr = "tls-type";
      break;
    case elf_symbol::GNU_IFUNC_TYPE:
      repr = "gnu-ifunc-type";
      break;
    default:
      repr = "no-type";
      break;
    }

  o << " type='" << repr << "'";
}

/// Serialize an elf_symbol::binding into an XML element attribute of
/// name 'binding'.
///
/// @param b the elf_symbol::binding to serialize.
///
/// @param o the output stream to serialize the binding to.
static void
write_elf_symbol_binding(elf_symbol::binding b, ostream& o)
{
  string repr;

  switch (b)
    {
    case elf_symbol::LOCAL_BINDING:
      repr = "local-binding";
      break;
    case elf_symbol::GLOBAL_BINDING:
      repr = "global-binding";
      break;
    case elf_symbol::WEAK_BINDING:
      repr = "weak-binding";
      break;
    case elf_symbol::GNU_UNIQUE_BINDING:
      repr = "gnu-unique-binding";
      break;
    default:
      repr = "no-binding";
      break;
    }

  o << " binding='" << repr << "'";
}

/// Serialize an elf_symbol::binding into an XML element attribute of
/// name 'binding'.
///
/// @param b the elf_symbol::binding to serialize.
///
/// @param o the output stream to serialize the binding to.
static void
write_elf_symbol_visibility(elf_symbol::visibility v, ostream& o)
{
  string repr;

  switch (v)
    {
    case elf_symbol::DEFAULT_VISIBILITY:
      repr = "default-visibility";
      break;
    case elf_symbol::PROTECTED_VISIBILITY:
      repr = "protected-visibility";
      break;
    case elf_symbol::HIDDEN_VISIBILITY:
      repr = "hidden-visibility";
      break;
    case elf_symbol::INTERNAL_VISIBILITY:
      repr = "internal-visibility";
      break;
    default:
      repr = "default-visibility";
      break;
    }

  o << " visibility='" << repr << "'";
}

/// Write alias attributes for the aliases of a given symbol.
///
/// @param sym the symbol to write the attributes for.
///
/// @param o the output stream to write the attributes to.
///
/// @return true upon successful completion.
static bool
write_elf_symbol_aliases(const elf_symbol& sym, ostream& out)
{
  if (!sym.is_main_symbol() || !sym.has_aliases())
    return false;


  std::vector<std::string> aliases;
  for (elf_symbol_sptr s = sym.get_next_alias(); s && !s->is_main_symbol();
       s = s->get_next_alias())
    {
      if (!s->is_public())
	continue;

      if (s->is_suppressed())
	continue;

      if (sym.is_in_ksymtab() != s->is_in_ksymtab())
	continue;

      aliases.push_back(s->get_id_string());
    }

  if (!aliases.empty())
    {
      out << " alias='";
      std::string separator;
      for (const auto& alias : aliases)
	{
	  out << separator << alias;
	  separator = ",";
	}

      out << "'";
      return true;
    }

  return false;
}

/// Write an XML attribute for the reference to a symbol for the
/// current decl.
///
///
/// @param ctxt the current write context to consider.
///
/// @param sym the symbol to consider.
///
/// @param abi the ABI corpus the symbol @p sym is supposed to belong
/// to.  If the symbol doesn't belong to that corpus, then the
/// reference is not be emitted.
///
/// @param o the output stream to write the attribute to.
///
/// @return true upon successful completion.
static bool
write_elf_symbol_reference(write_context& ctxt,
			   const elf_symbol& sym,
			   const corpus& abi,
			   ostream& o)
{
  elf_symbol_sptr s = abi.lookup_function_symbol(sym);
  if (!s)
    s = abi.lookup_variable_symbol(sym);

  if (// If that symbol wasn't found in the current corpus ...
      !s
      // ... or we were NOT asked to represent undefined symbols and
      // yet that symbol is undefined ...
      || (!ctxt.get_write_undefined_symbols() && !s->is_defined()))
    // Then do not emit this symbol reference.
    return false;

  const elf_symbol* main = sym.get_main_symbol().get();
  const elf_symbol* alias = &sym;
  bool found = !alias->is_suppressed();
  // If the symbol itself is suppressed, check the alias chain.
  if (!found)
    {
      alias = main;
      found = !alias->is_suppressed();
    }
  // If the main symbol is suppressed, search the remainder of the chain.
  while (!found)
    {
      alias = alias->get_next_alias().get();
      // Two separate termination conditions at present.
      if (!alias || alias == main)
        break;
      found = !alias->is_suppressed();
    }
  // If all aliases are suppressed, just stick with the main symbol.
  if (!found)
    alias = main;
  o << " elf-symbol-id='"
    << xml::escape_xml_string(alias->get_id_string())
    << "'";
  return true;
}

/// Write an XML attribute for the reference to a symbol for the
/// current decl.
///
/// @param ctxt the write context to consider.
/// 
/// @param sym the symbol to consider.
///
/// @param abi the ABI corpus the symbol @p sym is supposed to belong
/// to.  If the symbol doesn't belong to that corpus, then the
/// reference is not be emitted.
///
/// @param o the output stream to write the attribute to.
///
/// @return true upon successful completion.
static bool
write_elf_symbol_reference(write_context& ctxt,
			   const elf_symbol_sptr sym,
			   const corpus& abi,
			   ostream& o)
{
  if (!sym)
    return false;

  return write_elf_symbol_reference(ctxt, *sym, abi, o);
}

/// Serialize the attributes "constructor", "destructor" or "static"
/// if they have true value.
///
/// @param is_ctor if set to true, the "constructor='true'" string is
/// emitted.
///
/// @param is_dtor if set to true the "destructor='true' string is
/// emitted.
///
/// @param is_static if set to true the "static='true'" string is
/// emitted.
///
/// @param o the output stream to use for the serialization.
static void
write_cdtor_const_static(bool is_ctor,
			 bool is_dtor,
			 bool is_const,
			 bool is_static,
			 ostream& o)
{
  if (is_static)
    o << " static='yes'";
  if (is_ctor)
    o << " constructor='yes'";
  else if (is_dtor)
    o << " destructor='yes'";
  if (is_const)
    o << " const='yes'";
}

/// Serialize the attribute "is-declaration-only", if the
/// decl_base_sptr has its 'is_declaration_only property set.
///
/// @param t the pointer to instance of @ref decl_base to consider.
///
/// @param o the output stream to serialize to.
static void
write_is_declaration_only(const decl_base_sptr& d, ostream& o)
{
  if (d->get_is_declaration_only())
    o << " is-declaration-only='yes'";
}

/// Serialize the attribute "is-struct", if the current instance of
/// class_decl is a struct.
///
/// @param klass a pointer to the instance of class_decl to consider.
///
/// @param o the output stream to serialize to.
static void
write_is_struct(const class_decl_sptr& klass, ostream& o)
{
  if (klass->is_struct())
    o << " is-struct='yes'";
}

/// Serialize the attribute "is-anonymous", if the current instance of
/// decl is anonymous
///
/// @param dcl a pointer to the instance of @ref decl_base to consider.
///
/// @param o the output stream to serialize to.
static void
write_is_anonymous(const decl_base_sptr& decl, ostream& o)
{
  if (decl->get_is_anonymous())
    o << " is-anonymous='yes'";
}

/// Emit the hash value and the canonical type index of a given type.
///
/// @param t the type to consider.
///
/// @param o the output stream to emit the hash to.
static void
write_type_hash_and_cti(const type_base_sptr& t, ostream& o)
{
  hash_t hash = t->hash_value();

  if (hash)
    {
      string h;
      ABG_ASSERT(hashing::serialize_hash(*hash, h));
      o << " hash='" << h;
      if (t->priv_->canonical_type_index)
	o << "#" << t->priv_->canonical_type_index;
      o << "'";
    }
}

/// Write the native offset of a given artifact.
///
/// @param ctxt the write_context to use.
///
/// @param t the artifact to consider.
///
/// @param o the output stream toemit the native offset to.
static void
write_artifact_native_offset(write_context& ctxt,
			     const type_or_decl_base_sptr& t,
			     ostream& o)
{
  if (!ctxt.get_write_native_offsets() || !t)
    return;

  if (offset_t offset = t->get_native_offset())
    {
      std::ostringstream os;
      os << std::hex << *offset;
      o << " native-offset='" << os.str() << "'";
    }
}

/// Emit several XML properties related to type IR nodes.
///
/// @param t the type IR node to emit the XML properties for.
///
/// @param ctxt the write context to use.
///
/// @param id the type-ID to use in the XML properties emitted.
static void
write_common_type_info(const type_base_sptr& t,
		       write_context& ctxt,
		       const string& id)
{
  decl_base_sptr d = is_decl(t);

  ostream& o = ctxt.get_ostream();

  write_is_non_reachable(t, o);

  if (!d || (d && !d->get_is_declaration_only()))
    {
      if (!is_qualified_type(t) && !is_array_type(t))
	write_size_and_alignment(t, o);
      else if (array_type_def_sptr a = is_array_type(t))
	write_array_size_and_alignment(a, o);
    }

  if (d)
    {
      write_is_anonymous(d, o);
      write_is_declaration_only(d, o);
      write_location(d, ctxt);
    }

  write_artifact_native_offset(ctxt, t, o);
  write_type_hash_and_cti(t, o);

  string i = id;
  if (i.empty())
    i = ctxt.get_id_for_type(t);
  o << " id='" << i << "'";

  ctxt.record_type_as_emitted(t);
}

/// Helper to serialize a type artifact.
///
/// The function actually serializes the canonical type of the type
/// artifact.  It's the ultimate way to de-duplicate the resulting
/// ABIXML.
///
/// @param t the type to serialize.
///
/// @param ctxt the @ref write_context to use.
///
/// @param indent the number of white space to use for indentation.
///
/// @return true upon successful completion.
static bool
write_type(const type_base_sptr&	t,
	   write_context&		ctxt,
	   unsigned			indent)
{
  type_base_sptr type(get_exemplar_type(t));

  if (!ctxt.type_to_be_emitted(type))
    return false;

  if (write_type_decl(dynamic_pointer_cast<type_decl> (type),
		      ctxt, indent)
      || write_qualified_type_def (dynamic_pointer_cast<qualified_type_def>
				   (type),
				   ctxt, indent)
      || write_pointer_type_def(dynamic_pointer_cast<pointer_type_def>(type),
				ctxt, indent)
      || write_reference_type_def(dynamic_pointer_cast
				  <reference_type_def>(type), ctxt, indent)
      || write_ptr_to_mbr_type(dynamic_pointer_cast
			       <ptr_to_mbr_type>(type),
			       ctxt, indent)
      || write_array_type_def(dynamic_pointer_cast
			      <array_type_def>(type), ctxt, indent)
      || write_enum_type_decl(dynamic_pointer_cast<enum_type_decl>(type),
			      ctxt, indent)
      || write_typedef_decl(dynamic_pointer_cast<typedef_decl>(type),
			    ctxt, indent)
      || write_class_decl(is_class_type(type), ctxt, indent)
      || write_union_decl(is_union_type(type), ctxt, indent)
      || write_function_type(is_function_type(type), ctxt, indent)
      || (write_function_tdecl
	  (dynamic_pointer_cast<function_tdecl>(type), ctxt, indent))
      || (write_class_tdecl
	  (dynamic_pointer_cast<class_tdecl>(type), ctxt, indent)))
    return true;

  return false;
}

/// Serialize a pointer to an of decl_base into an output stream.
///
/// @param decl the pointer to decl_base to serialize
///
/// @param ctxt the context of the serialization.  It contains e.g, the
/// output stream to serialize to.
///
/// @param indent how many indentation spaces to use during the
/// serialization.
///
/// @return true upon successful completion, false otherwise.
static bool
write_decl(const decl_base_sptr& decl, write_context& ctxt, unsigned indent)
{
  type_base_sptr type(get_exemplar_type(is_type(decl).get()), noop_deleter());

  if (type && !ctxt.type_to_be_emitted(type))
    return false;

  if (write_type_decl(is_type_decl(type), ctxt, indent)
      || write_namespace_decl(is_namespace(decl), ctxt, indent)
      || write_qualified_type_def (is_qualified_type(type), ctxt, indent)
      || write_pointer_type_def(is_pointer_type(type), ctxt, indent)
      || write_reference_type_def(is_reference_type(type), ctxt, indent)
      || write_ptr_to_mbr_type(is_ptr_to_mbr_type(type), ctxt, indent)
      || write_array_type_def(is_array_type(type), ctxt, indent)
      || write_array_subrange_type(is_subrange_type(type), ctxt, indent)
      || write_enum_type_decl(is_enum_type(type), ctxt, indent)
      || write_typedef_decl(is_typedef(type), ctxt, indent)
      || write_var_decl(is_var_decl(decl), ctxt, /*write_linkage_name=*/true, indent)
      || write_function_decl(is_method_decl(decl), ctxt, indent)
      || write_function_decl(is_function_decl(decl), ctxt, indent)
      || write_class_decl(is_class_type(type), ctxt, indent)
      || write_union_decl(is_union_type(type), ctxt, indent)
      || (write_function_tdecl
	  (dynamic_pointer_cast<function_tdecl>(decl), ctxt, indent))
      || (write_class_tdecl
	  (dynamic_pointer_cast<class_tdecl>(decl), ctxt, indent)))
    return true;

  return false;
}

/// Emit a declaration, along with its scope.
///
/// This function is called at the end of emitting a translation unit,
/// to emit type declarations that were referenced by types that were
/// emitted in the TU already, but that were not emitted themselves.
///
/// @param decl the decl to emit.
///
/// @param ctxt the write context to use.
///
/// @param initial_indent the number of indentation spaces to use.
static void
write_artifact_in_scope(const type_or_decl_base_sptr&	artf,
			write_context&			ctxt,
			unsigned			initial_indent)
{
  type_base_sptr type = is_type(artf);
  decl_base_sptr decl = is_decl(artf);
  if ((type && ctxt.type_is_emitted(type))
      || (!type && ctxt.decl_is_emitted(decl)))
    return;

  list<scope_decl_sptr> scopes;
  if (decl)
    for (auto s = decl->get_scope();
	 s && !is_global_scope(s);
	 s = s->get_scope())
      scopes.push_front(s);

  ostream& o = ctxt.get_ostream();
  const config& c = ctxt.get_config();
  stack<string> closing_tags;
  stack<unsigned> closing_indents;
  unsigned indent = initial_indent;
  for (auto i = scopes.begin();
       i != scopes.end();
       ++i)
    {
      ABG_ASSERT(!is_global_scope(*i));

      // A type scope is either a namespace ...
      if (namespace_decl_sptr n = is_namespace(*i))
	{
	  do_indent(o, indent);
	  o << "<namespace-decl name='"
	    << xml::escape_xml_string(n->get_name())
	    << "'>\n";
	  closing_tags.push("</namespace-decl>");
	  closing_indents.push(indent);
	}
      // ... or a class.
      else if (class_decl_sptr c = is_class_type(*i))
	{
	  c = is_class_type(look_through_decl_only_class(c));
	  class_decl_sptr class_type = c;

	  bool do_break = false;
	  if (!ctxt.type_is_emitted(c) && ctxt.type_to_be_emitted(c))
	    {
	      write_type(class_type, ctxt, initial_indent);
	      // So, we've written class_type, which is a scope of
	      // 'decl'.  So normally, decl should have been emitted
	      // by the emitting of class_type.
	      //
	      // But there can be times where 'decl' is not emitted.
	      //
	      // 'decl' can still be not emitted if the canonical type
	      // of class_type (the one that is emitted) is not the
	      // variant that contains the member type 'decl'.  In
	      // that case, 'decl' still needs to be emitted after
	      // emitting tags for its scope.  That is done by the
	      // 'if' block below.
	      do_break = true;
	    }

	  if (!do_break
	      // if decl/type is still not emitted, then it means the
	      // canonical type for 'class_type' (the scope) above was
	      // emitted but wasn't the variant containing the member
	      // type 'decl/type', or it is not a reachable type.
	      //
	      // In that case, we'll need to emit the tags for the
	      // scope of decl and then emit decl.
	      || (type && !ctxt.type_is_emitted(type))
	      || (!type && !ctxt.decl_is_emitted(decl)))
	    {
	      annotate(class_type, ctxt, indent);
	      write_class_decl_opening_tag(class_type, "", ctxt, indent,
					   /*prepare_to_handle_empty=*/false);
	      closing_tags.push("</class-decl>");
	      closing_indents.push(indent);
	      unsigned nb_ws = get_indent_to_level(ctxt, indent, 1);
	      indent = nb_ws;
	      auto type_to_emit = type;
	      auto decl_to_emit = decl;
	      auto next_scope_index = i;
	      ++next_scope_index;
	      if (next_scope_index != scopes.end())
		{
		  type_to_emit = is_type(*next_scope_index);
		  decl_to_emit = is_decl(*next_scope_index);
		}
	      if (type_to_emit)
		{
		  write_member_type_opening_tag(type_to_emit, ctxt, nb_ws);
		  closing_tags.push("</member-type>");
		  closing_indents.push(indent);
		}
	      else if (function_decl_sptr fn_decl = is_function_decl(decl_to_emit))
		{
		  write_member_function_opening_tag(fn_decl, ctxt, nb_ws);
		  closing_tags.push("</member-function>");
		  closing_indents.push(indent);
		}
	      closing_indents.push(nb_ws);
	    }
	  if (do_break)
	    break;
	}
      else if (union_decl_sptr u = is_union_type(*i))
	{
	  u = is_union_type(look_through_decl_only(u));
	  union_decl_sptr union_type = u;
	  bool do_break = false;
	  if (!ctxt.type_is_emitted(u))
	    {
	      write_type(union_type, ctxt, initial_indent);
	      do_break = true;
	    }

	  if (!do_break
	      // type is still not emitted then it means the canonical
	      // union was emitted but it was not variant containing
	      // the member type.
	      || !ctxt.type_is_emitted(type))
	    {
	      annotate(union_type, ctxt, indent);
	      write_union_decl_opening_tag(union_type, "", ctxt, indent,
					   /*prepare_to_handle_empty=*/false);
	      closing_tags.push("</union-decl>");
	      closing_indents.push(indent);

	      unsigned nb_ws = get_indent_to_level(ctxt, indent, 1);
	      write_member_type_opening_tag(type, ctxt, nb_ws);
	      indent = nb_ws;
	      closing_tags.push("</member-type>");
	      closing_indents.push(nb_ws);
	    }

	  if (do_break)
	    break;
	}
      else if (function_decl_sptr f = is_function_decl(*i))
	{
	  function_decl_sptr fn = f;
	  write_member_function_opening_tag(fn, ctxt, indent);
	  write_function_decl_opening_tag(fn, ctxt, get_indent_to_level(ctxt, indent, 1));
	  if (is_function_decl(decl->get_scope()) == f)
	    {
	      write_function_member_decls(fn, ctxt,
					  get_indent_to_level(ctxt, indent, 2));
	      if (type)
		ABG_ASSERT(ctxt.type_is_emitted(type));
	      else
		ABG_ASSERT(ctxt.decl_is_emitted(decl));
	      write_fn_parm_and_return_types(fn->get_type(),
					     /*skip_first_parm=*/false,
					     ctxt, indent);
	      write_function_decl_closing_tag(fn, ctxt, indent);
	      write_member_function_closing_tag(fn, ctxt, indent);
	      ctxt.record_decl_as_emitted(fn);
	    }
	  else
	    ABG_ASSERT_NOT_REACHED; // TODO: this case is not yet
				    // supported. Basically, it
				    // requires writting the fn parms
				    // and return types, but not the
				    // decl closing tags nor mem-fn
				    // closing tags.  Those would be
				    // witten down later.
	}
      else
	// What other kind of decls can we emit?  Let's assume we
	// should never reach this point for now ...
	ABG_ASSERT_NOT_REACHED;
      indent += c.get_xml_element_indent();
    }

  bool do_write = false;
  if (type_base_sptr type = is_type(decl))
    {
      if (ctxt.type_to_be_emitted(type))
	do_write= true;
    }
  else
    {
      if (!ctxt.decl_is_emitted(decl))
	do_write= true;
    }

  if (do_write)
    write_decl(decl, ctxt, indent);

  while (!closing_tags.empty())
    {
      do_indent(o, closing_indents.top());
      o << closing_tags.top() << "\n";
      closing_tags.pop();
      closing_indents.pop();
    }
}

/// Create a @ref write_context object that can be used to emit abixml
/// files.
///
/// @param env the environment for the @ref write_context object to use.
///
/// @param default_output_stream the default output stream to use.
///
/// @return the new @ref write_context object.
write_context_sptr
create_write_context(const environment& env,
		     ostream& default_output_stream)
{
  write_context_sptr ctxt(new write_context(env, default_output_stream));
  return ctxt;
}

/// Set the "show-locs" flag.
///
/// When this flag is set then the XML writer emits location (///
/// information (file name, line and column) for the ABI artifacts
/// that it emits.
///
/// @param ctxt the @ref write_context to set the option for.
///
/// @param flag the new value of the option.
void
set_show_locs(write_context& ctxt, bool flag)
{ctxt.set_show_locs(flag);}

/// Set the 'annotate' flag.
///
/// When this flag is set then the XML writer annotates ABI artifacts
/// with a human readable description.
///
/// @param ctxt the context to set this flag on to.
///
/// @param flag the new value of the 'annotate' flag.
void
set_annotate(write_context& ctxt, bool flag)
{ctxt.set_annotate(flag);}

/// Setter of the "write-member-hashes" flag.
///
/// @param ctxt the context to consider.
///
/// @param flag the new value of the flag.
void
set_write_member_hashes(write_context& ctxt, bool flag)
{ctxt.set_write_member_hashes(flag);}

/// Setter of the "write-non-reachable-types" flag.
///
/// @param ctxt the context to consider.
///
/// @param flag the new value of the flag.
void
set_write_non_reachable_types(write_context& ctxt, bool flag)
{ctxt.set_write_non_reachable_types(flag);}

/// Set the new ostream.
///
/// The ostream refers to the object, writers should stream new output to.
///
/// @param ctxt the context to set this to.
///
/// @param os the new ostream
void
set_ostream(write_context& ctxt, ostream& os)
{ctxt.set_ostream(os);}

/// Set the 'write-architecture' flag.
///
/// When this flag is set then the XML writer will emit architecture
/// information
///
/// @param ctxt the context to set this flag on to.
///
/// @param flag the new value of the 'write-architecture' flag.
void
set_write_architecture(write_context& ctxt, bool flag)
{ctxt.set_write_architecture(flag);}

/// Set the 'write-corpus-path' flag.
///
/// When this flag is set then the XML writer will emit corpus-path
/// information
///
/// @param ctxt the context to set this flag on to.
///
/// @param flag the new value of the 'write-corpus-path' flag.
void
set_write_corpus_path(write_context& ctxt, bool flag)
{ctxt.set_write_corpus_path(flag);}

/// Set the 'write-comp-dir' flag.
///
/// When this flag is set then the XML writer will emit compilation dir
/// information
///
/// @param ctxt the context to set this flag on to.
///
/// @param flag the new value of the 'write-comp-dir' flag.
void
set_write_comp_dir(write_context& ctxt, bool flag)
{ctxt.set_write_comp_dir(flag);}

/// Set the 'short-locs' flag.
///
/// When this flag is set then the XML writer will emit only file names
/// rather than full paths.
///
/// @param ctxt the context to set this flag on to.
///
/// @param flag the new value of the 'short-locs' flag.
void
set_short_locs(write_context& ctxt, bool flag)
{ctxt.set_short_locs(flag);}

/// Set the 'parameter-names' flag.
///
/// When this flag is set then the XML writer will emit the names of
/// function parameters.
///
/// @param ctxt the context to set this flag on to.
///
/// @param flag the new value of the 'parameter-names' flag.
void
set_write_parameter_names(write_context& ctxt, bool flag)
{ctxt.set_write_parameter_names(flag);}

/// Set the 'elf-needed' flag.
///
/// When this flag is set then the XML writer will emit corpus
/// get_needed() (DT_NEEDED) information.
///
/// @param ctxt the context to set this flag on to.
///
/// @param flag the new value of the 'elf-needed' flag.
void
set_write_elf_needed(write_context& ctxt, bool flag)
{ctxt.set_write_elf_needed(flag);}

/// Set the 'undefined-symbols' flag.
///
/// When this flag is set then the XML writer will emit corpus
/// information about the undefined function and variable symbols.
///
/// @param ctxt the context to set this flag on to.
///
/// @param flag the new value of the 'undefined-symbols' flag.
void
set_write_undefined_symbols(write_context& ctxt, bool flag)
{ctxt.set_write_undefined_symbols(flag);}

/// Set the 'default-sizes' flag.
///
/// When this flag is set then the XML writer will emit default
/// size-in-bits attributes for pointer type definitions, reference
/// type definitions, function declarations and function types even
/// when they are equal to the default address size of the translation
/// unit.
///
/// @param ctxt the context to set this flag on to.
///
/// @param flag the new value of the 'default-sizes' flag.
void
set_write_default_sizes(write_context& ctxt, bool flag)
{ctxt.set_write_default_sizes(flag);}

/// Set the 'type-id-style' property.
///
/// This property controls the kind of type ids used in XML output.
///
/// @param ctxt the context to set this property on.
///
/// @param style the new value of the 'type-id-style' property.
void
set_type_id_style(write_context& ctxt, type_id_style_kind style)
{ctxt.set_type_id_style(style);}

/// Set the "emit-native-offsets' property.
///
/// When set to "true", the ABIXML entries for types contain the
/// native offsets of the Debug Information Entries they derive from.
/// This is a debugging tool.
///
/// @param ctxt the context to set this property on.
///
/// @param flag the new value of the property.
void
set_write_native_offsets(write_context& ctxt, bool flag)
{ctxt.set_write_native_offsets(flag);}

/// Serialize the canonical types of a given scope.
///
/// @param scope the scope to consider.
///
/// @param ctxt the write context to use.
///
/// @param indent the number of white space indentation to use.
 //
 // @param is_member_type if true, the canonical types are emitted as
 // member types (of a class).
 //
 // return true upon successful completion.
static bool
write_canonical_types_of_scope(const scope_decl	&scope,
			       write_context		&ctxt,
			       const unsigned		indent,
			       bool			is_member_type = false)
{
  const type_base_sptrs_type &canonical_types =
    scope.get_sorted_canonical_types();

  for (type_base_sptrs_type::const_iterator i = canonical_types.begin();
       i != canonical_types.end();
       ++i)
    {
      if (!ctxt.type_to_be_emitted(*i))
	continue;
      if (is_member_type)
	write_member_type(*i, ctxt, indent);
      else
	write_type(*i, ctxt, indent);
    }

  return true;
}

/// Emit the types that were referenced by other emitted types.
///
/// This is a sub-routine of write_translation_unit.
///
/// @param ctxt the write context to use.
///
///
/// @param indent the indentation string.
///
/// @param is_last whether @p tu is the last translation unit or not.
static void
write_referenced_types(write_context &		ctxt,
		       const unsigned		indent)
{
  const config& c = ctxt.get_config();
  // Now let's handle types that were referenced, but not yet
  // emitted because they are either:
  //   1/ Types without canonical type
  //   2/ or function types (these might have no scope).

  // So this map of type -> string is to contain the referenced types
  // we need to emit.
  type_ptr_set_type referenced_types_to_emit;

  // For each referenced type, ensure that it is either emitted in the
  // translation unit to which it belongs or in the last translation
  // unit as a last resort.
  for (type_ptr_set_type::const_iterator i =
	 ctxt.get_referenced_types().begin();
       i != ctxt.get_referenced_types().end();
       ++i)
    if (ctxt.type_to_be_emitted(*i))
      referenced_types_to_emit.insert(*i);

  for (fn_type_ptr_set_type::const_iterator i =
	 ctxt.get_referenced_function_types().begin();
       i != ctxt.get_referenced_function_types().end();
       ++i)
    if (ctxt.type_to_be_emitted(*i))
      referenced_types_to_emit.insert(*i);

  // Ok, now let's emit the referenced type for good.
  while (!referenced_types_to_emit.empty())
    {
      // But first, we need to sort them, otherwise, emitting the ABI
      // (in xml) of the same binary twice will yield different
      // results, because we'd be walking an *unordered* hash table.
      vector<type_base*> sorted_referenced_types;
      ctxt.sort_types(referenced_types_to_emit,
		      sorted_referenced_types);

      // Now, emit the referenced decls in a sorted order.
      for (vector<type_base*>::const_iterator i =
	     sorted_referenced_types.begin();
	   i != sorted_referenced_types.end();
	   ++i)
	{
	  // We handle types which have declarations *and* function
	  // types here.
	  type_base* t = *i;
	  if (ctxt.type_to_be_emitted(t))
	    {
	      if (decl_base* d = get_type_declaration(t))
		{
		  decl_base_sptr decl(d, noop_deleter());
		  write_artifact_in_scope(decl, ctxt,
					  indent + c.get_xml_element_indent());
		}
	      else if (function_type* f = is_function_type(t))
		{
		  function_type_sptr fn_type(f, noop_deleter());
		  write_function_type(fn_type, ctxt,
				      indent + c.get_xml_element_indent());
		}
	      else
		ABG_ASSERT_NOT_REACHED;
	    }
	}

      // So all the (referenced) types that we wanted to emit were
      // emitted.
      referenced_types_to_emit.clear();

      // But then, while emitting those referenced type, other types
      // might have been referenced by those referenced types
      // themselves!  So let's look at the sets of referenced type
      // that are maintained for the entire ABI corpus and see if
      // there are still some referenced types in there that are not
      // emitted yet.  If yes, then we'll emit those again.

      // For each referenced type, ensure that it is either emitted in
      // the translation unit to which it belongs or in the last
      // translation unit as a last resort.
      for (type_ptr_set_type::const_iterator i =
	     ctxt.get_referenced_types().begin();
	   i != ctxt.get_referenced_types().end();
	   ++i)
	if (ctxt.type_to_be_emitted(*i))
	  referenced_types_to_emit.insert(*i);
    }
}

/// From a given decl (function or var), get the variant of that decl
/// that is actually defined in the ABI corpus.
///
/// @param ctxt the write context.
///
/// @param decl the decl to consider.
///
/// @return a pointer to the variant of @p decl that is defined in the
/// ABI corpus, or nullptr if none is present, meaning @p decl is not
/// defined in the ABI corpus.
static decl_base*
get_variant_of_decl_from_abi(const write_context& ctxt,
			     const decl_base_sptr decl)
{
  if (!decl)
    return nullptr;

  const corpus* abi = decl->get_corpus();
  if (!abi)
    return decl.get();

  if (function_decl_sptr function = is_function_decl(decl))
    {
      interned_string id = function->get_id();
      if (const std::unordered_set<const function_decl*>* fns =
	  abi->lookup_functions(id))
	for (auto f : *fns)
	  if (elf_symbol_sptr sym = f->get_symbol())
	    if (ctxt.get_write_undefined_symbols() || sym->is_defined())
	      return const_cast<function_decl*>(f);

      if (ctxt.get_write_undefined_symbols())
	if (abi->get_undefined_functions().find(function.get())
	    != abi->get_undefined_functions().end())
	  return function.get();
    }
  else if (var_decl_sptr variable = is_var_decl(decl))
    {
      interned_string id = variable->get_id();
      if (const std::unordered_set<var_decl_sptr>* vars =
	  abi->lookup_variables(id))
	for (auto v : *vars)
	  if (elf_symbol_sptr sym = v->get_symbol())
	    if (ctxt.get_write_undefined_symbols() || sym->is_defined())
	      return v.get();
    }

  return nullptr;
}

/// Test if a translation unit is empty or if all of its content has
/// already been emitted out to ABIXML through a given write context.
///
/// @param ctxt the writer context to consider.
///
/// @param tu the translation unit to consider.
///
/// @return true if the translation unit @p tu is empty or if all of
/// its content has already been emitted.
static bool
translation_unit_is_essentially_empty(const write_context& ctxt,
				      const translation_unit& tu)
{
  if (tu.is_empty())
    return true;

  const scope_decl_sptr scope = tu.get_global_scope();
  if (scope_is_essentially_empty(scope, ctxt))
    return true;

  return false;
}

/// Emit all the decls that are present in a scope.
///
/// @param ctxt the write context to use.
///
/// @param scope the scope to consider.
///
/// @param indent the number of white space to indent the new bloc to.
static void
write_decls_from_scope(write_context&		ctxt,
		       const scope_decl&	scope,
		       const unsigned		indent)
{
  typedef scope_decl::declarations declarations;
  const declarations& decls = scope.get_sorted_member_decls();
  const config& c = ctxt.get_config();

  for (const decl_base_sptr& decl : decls)
    {
      if (type_base_sptr t = is_type(decl))
	{
	  if (!ctxt.type_to_be_emitted(t))
	    continue;
	  ABG_ASSERT(ctxt.type_is_emitted(t));
	}
      else if (var_decl_sptr var = is_var_decl(decl))
	{
	  var_decl_sptr v(is_var_decl(get_variant_of_decl_from_abi(ctxt, var)),
			  noop_deleter());
	  if (!v)
	    // The variable is not part of the ABI, drop it on the floor.
	    continue;

	  if (!ctxt.decl_is_emitted(v))
	    write_artifact_in_scope(v, ctxt, indent);
	}
      else if (function_decl_sptr f = is_function_decl(decl))
	{
	  function_decl_sptr fn(is_function_decl(get_variant_of_decl_from_abi(ctxt, f)),
				noop_deleter());
	  if (!fn)
	    continue;

	  if (!ctxt.decl_is_emitted(fn))
	    write_decl(fn, ctxt, indent + c.get_xml_element_indent());
	}
      else
	// namespaces and templates ...
	write_decl(decl, ctxt, indent);
    }
}

/// Serialize a translation unit to an output stream.
///
/// @param ctxt the context of the serialization.  It contains e.g,
/// the output stream to serialize to.
///
/// @param tu the translation unit to serialize.
///
/// @param indent how many indentation spaces to use during the
/// serialization.
///
/// @return true upon successful completion, false otherwise.
bool
write_translation_unit(write_context&		ctxt,
		       const translation_unit&	tu,
		       const unsigned		indent)
{
  if (tu.is_empty() || translation_unit_is_essentially_empty(ctxt, tu))
    return false;

  ostream& o = ctxt.get_ostream();
  const config& c = ctxt.get_config();

  do_indent(o, indent);

  o << "<abi-instr";

  if (tu.get_address_size() != 0)
    o << " address-size='" << static_cast<int>(tu.get_address_size()) << "'";

  std::string tu_path = tu.get_path();
  if (ctxt.get_short_locs())
    tools_utils::base_name(tu_path, tu_path);
  if (!tu_path.empty())
    o << " path='" << xml::escape_xml_string(tu_path) << "'";

  if (!tu.get_compilation_dir_path().empty() && ctxt.get_write_comp_dir())
    o << " comp-dir-path='"
      << xml::escape_xml_string(tu.get_compilation_dir_path()) << "'";

  if (tu.get_language() != translation_unit::LANG_UNKNOWN)
    o << " language='"
      << translation_unit_language_to_string(tu.get_language())
      <<"'";

  if (tu.is_empty())
    {
      o << "/>\n";
      return true;
    }

  o << ">\n";

  const corpus* abi = tu.get_corpus();
  if (!abi)
    // This is for reading legacy ABIXML files that don't have a
    // corpus.
    write_canonical_types_of_scope(*tu.get_global_scope(),
				   ctxt, indent + c.get_xml_element_indent());

  write_decls_from_scope(ctxt, *tu.get_global_scope(),
			 indent + c.get_xml_element_indent());

  // Write the undefined functions that belong to this translation
  // unit
  if (abi)
    for (auto undefined_function : abi->get_sorted_undefined_functions())
      {
	function_decl_sptr f(const_cast<function_decl*>(undefined_function),
			     noop_deleter());
	function_decl_sptr fn(is_function_decl(get_variant_of_decl_from_abi(ctxt, f)),
			      noop_deleter());

	if ((fn && fn->get_translation_unit() != &tu)
	    || ctxt.decl_is_emitted(fn))
	  continue;

	if (fn)
	  write_decl(fn, ctxt, indent + c.get_xml_element_indent());
      }

  // Write the undefined variables that belong to this translation
  // unit
  if (abi)
    for (auto undefined_var : abi->get_sorted_undefined_variables())
      {
	var_decl_sptr v = undefined_var;
	var_decl_sptr var(is_var_decl(get_variant_of_decl_from_abi(ctxt, v)),
			  noop_deleter());
	if ((var && var->get_translation_unit() != &tu)
	    || ctxt.decl_is_emitted(v))
	  continue;

	if (var)
	  write_decl(var, ctxt, indent + c.get_xml_element_indent());
      }

  if (!abi)
    {
      // Now handle all function types that were not only referenced by
      // emitted types.
      const type_sptr_set_type& t = tu.get_live_fn_types();
      vector<type_base_sptr> sorted_types;
      ctxt.sort_types(t, sorted_types);

      for (vector<type_base_sptr>::const_iterator i = sorted_types.begin();
	   i != sorted_types.end();
	   ++i)
	{
	  function_type_sptr fn_type = is_function_type(*i);

	  if (fn_type->get_is_artificial() || ctxt.type_is_emitted(fn_type))
	    // This function type is either already emitted or it's
	    // artificial (i.e, artificially created just to represent the
	    // conceptual type of a function), so skip it.
	    continue;

	  ABG_ASSERT(fn_type);
	  write_function_type(fn_type, ctxt, indent + c.get_xml_element_indent());
	}

      // After we've written out the live function types, we need to write
      // the types they referenced.
      write_referenced_types(ctxt, indent);
    }

  do_indent(o, indent);
  o << "</abi-instr>\n";

  return true;
}

/// Emit the 'abi-types' element which contains all the types used by
/// the interfaces of the ABI.
///
/// @param ctxt the write context to use.
///
/// @param abi_corpus the ABI corpus to consider.
///
/// @param indent the number of white spaces to use for indentation.
///
/// @return true iff anything was emitted at all.
bool
write_abi_types(write_context&		ctxt,
		const corpus&		abi_corpus,
		const unsigned		indent)
{
  std::ostream& o = ctxt.get_ostream();
  const config &conf = ctxt.get_config();

  do_indent(o, indent);

  const vector<type_base_wptr>& types =
    abi_corpus.get_types().get_types_sorted();

  if (types.empty())
    {
      o << "<abi-types/>\n";
      return false;
    }

  o << "<abi-types>\n";

  for (auto& type_wptr : types)
    {
      type_base_sptr t(type_wptr);

      if (!ctxt.type_to_be_emitted(t))
	continue;

      if (function_type_sptr fn_type = is_function_type(t))
	write_type(fn_type, ctxt, indent + conf.get_xml_element_indent());
      else if (decl_base_sptr type_decl = is_decl(t))
	write_artifact_in_scope(type_decl, ctxt,
				indent + conf.get_xml_element_indent());
      else
	ABG_ASSERT_NOT_REACHED;
    }

  write_referenced_types(ctxt, indent + conf.get_xml_element_indent());

  do_indent(o, indent);
  o << "</abi-types>\n";

  return true;
}

/// Serialize a pointer to an instance of basic type declaration, into
/// an output stream.
///
/// @param d the basic type declaration to serialize.
///
/// @param ctxt the context of the serialization.  It contains e.g, the
/// output stream to serialize to.
///
/// @param indent how many indentation spaces to use during the
/// serialization.
///
/// @return true upon successful completion, false otherwise.
static bool
write_type_decl(const type_decl_sptr& d, write_context& ctxt, unsigned indent)
{
  if (!d)
    return false;

  ostream& o = ctxt.get_ostream();

  annotate(d, ctxt, indent);

  do_indent(o, indent);

  o << "<type-decl name='" << xml::escape_xml_string(d->get_name()) << "'";

  write_common_type_info(d, ctxt);

  o << "/>\n";

  return true;
}

/// Test if a scope is essentially empty.
///
/// "Essentially empty" means that the scope doesn't have any artifact
/// to be emitted.
///
/// @param scope the scope to consider.
///
/// @param ctxt the write context to consider.
///
/// @return true iff the scope is essentially empty.
static bool
scope_is_essentially_empty(const scope_decl *scope, const write_context& ctxt)
{
  // Look at the canonical types of this scope.
  for (auto& type : scope->get_canonical_types())
    if (ctxt.type_to_be_emitted(type))
      return false;

  for (auto& decl : scope->get_member_decls())
    {
      if (type_base_sptr type = is_type(decl))
	{
	  if (ctxt.type_to_be_emitted(type))
	    return false;
	}
      else if (var_decl_sptr var = is_var_decl(decl))
	{
	  var_decl_sptr v(is_var_decl(get_variant_of_decl_from_abi(ctxt, var)),
			  noop_deleter());
	  if (!v)
	    // The variable is not part of the ABI, drop it on the floor.
	    continue;
	  if (!ctxt.decl_is_emitted(v))
	    return false;
	}
      else if (function_decl_sptr f = is_function_decl(decl))
	{
	  function_decl_sptr fn(is_function_decl(get_variant_of_decl_from_abi(ctxt, f)),
				noop_deleter());
	  if (!fn)
	    continue;

	  if (!ctxt.decl_is_emitted(fn))
	    return false;
	}
      else if (scope_decl_sptr s = is_scope_decl(decl))
	{
	  if (!scope_is_essentially_empty(s, ctxt))
	    return false;
	}
      else // is a non-scope decl (templates ...)
	return false;
    }

  return true;
}

/// Test if a scope is essentially empty.
///
/// "Essentially empty" means that the scope doesn't have any artifact
/// to be emitted.
///
/// @param scope the scope to consider.
///
/// @param ctxt the write context to consider.
///
/// @return true iff the scope is essentially empty.
static bool
scope_is_essentially_empty(const scope_decl_sptr& decl,
			   const write_context& ctxt)
{return scope_is_essentially_empty(decl.get(), ctxt);}

/// Serialize a namespace declaration int an output stream.
///
/// @param decl the namespace declaration to serialize.
///
/// @param ctxt the context of the serialization.  It contains e.g, the
/// output stream to serialize to.
///
/// @param indent how many indentation spaces to use during the
/// serialization.
///
/// @return true upon successful completion, false otherwise.
static bool
write_namespace_decl(const namespace_decl_sptr& decl,
		     write_context& ctxt, unsigned indent)
{
  if (!decl)
    return false;

  if (scope_is_essentially_empty(decl, ctxt))
    return false;

  ostream& o = ctxt.get_ostream();
  const config &c = ctxt.get_config();

  annotate(decl, ctxt, indent);

  do_indent(o, indent);

  o << "<namespace-decl name='"
    << xml::escape_xml_string(decl->get_name())
    << "'>\n";

  write_canonical_types_of_scope(*decl, ctxt,
				 indent + c.get_xml_element_indent());

  write_decls_from_scope(ctxt, *decl, indent + c.get_xml_element_indent());

  do_indent(o, indent);
  o << "</namespace-decl>\n";

  return true;
}

/// Serialize a qualified type declaration to an output stream.
///
/// @param decl the qualfied type declaration to write.
///
/// @param id the type id identitifier to use in the serialized
/// output.  If this is empty, the function will compute an
/// appropriate one.  This is useful when this function is called to
/// serialize the underlying type of a member type; in that case, the
/// caller has already computed the id of the *member type*, and that
/// id is the one to be written as the value of the 'id' attribute of
/// the XML element of the underlying type.
///
/// @param ctxt the write context.
///
/// @param indent the number of space to indent to during the
/// serialization.
///
/// @return true upon successful completion, false otherwise.
static bool
write_qualified_type_def(const qualified_type_def_sptr&	decl,
			 const string&				id,
			 write_context&			ctxt,
			 unsigned				indent)
{
  if (!decl)
    return false;

  ostream& o = ctxt.get_ostream();


  type_base_sptr underlying_type = decl->get_underlying_type();

  annotate(decl, ctxt, indent);

  do_indent(o, indent);
  o << "<qualified-type-def type-id='"
    << ctxt.get_id_for_type(underlying_type)
    << "'";

  ctxt.record_type_as_referenced(underlying_type);

  if (decl->get_cv_quals() & qualified_type_def::CV_CONST)
    o << " const='yes'";
  if (decl->get_cv_quals() & qualified_type_def::CV_VOLATILE)
    o << " volatile='yes'";
  if (decl->get_cv_quals() & qualified_type_def::CV_RESTRICT)
    o << " restrict='yes'";

  write_common_type_info(decl, ctxt, id);

  o << "/>\n";

  return true;
}

/// Serialize a qualified type declaration to an output stream.
///
/// @param decl the qualfied type declaration to write.
///
/// @param ctxt the write context.
///
/// @param indent the number of space to indent to during the
/// serialization.
///
/// @return true upon successful completion, false otherwise.
static bool
write_qualified_type_def(const qualified_type_def_sptr&	decl,
			 write_context&			ctxt,
			 unsigned				indent)
{return write_qualified_type_def(decl, "", ctxt, indent);}

/// Serialize a pointer to an instance of pointer_type_def.
///
/// @param decl the pointer_type_def to serialize.
///
/// @param id the type id identitifier to use in the serialized
/// output.  If this is empty, the function will compute an
/// appropriate one.  This is useful when this function is called to
/// serialize the underlying type of a member type; in that case, the
/// caller has already computed the id of the *member type*, and that
/// id is the one to be written as the value of the 'id' attribute of
/// the XML element of the underlying type.
///
/// @param ctxt the context of the serialization.
///
/// @param indent the number of indentation white spaces to use.
///
/// @return true upon succesful completion, false otherwise.
static bool
write_pointer_type_def(const pointer_type_def_sptr&	decl,
		       const string&			id,
		       write_context&			ctxt,
		       unsigned			indent)
{
  if (!decl)
    return false;

  ostream& o = ctxt.get_ostream();

  annotate(decl, ctxt, indent);

  do_indent(o, indent);

  string i;

  o << "<pointer-type-def ";

  type_base_sptr pointed_to_type = decl->get_pointed_to_type();

  i = ctxt.get_id_for_type(pointed_to_type);

  o << "type-id='" << i << "'";

  ctxt.record_type_as_referenced(pointed_to_type);

  write_common_type_info(decl, ctxt, id);

  o << "/>\n";

  return true;
}

/// Serialize a pointer to an instance of pointer_type_def.
///
/// @param decl the pointer_type_def to serialize.
///
/// @param ctxt the context of the serialization.
///
/// @param indent the number of indentation white spaces to use.
///
/// @return true upon succesful completion, false otherwise.
static bool
write_pointer_type_def(const pointer_type_def_sptr&	decl,
		       write_context&			ctxt,
		       unsigned			indent)
{return write_pointer_type_def(decl, "", ctxt, indent);}

/// Serialize a pointer to an instance of reference_type_def.
///
/// @param decl the reference_type_def to serialize.
///
/// @param id the type id identitifier to use in the serialized
/// output.  If this is empty, the function will compute an
/// appropriate one.  This is useful when this function is called to
/// serialize the underlying type of a member type; in that case, the
/// caller has already computed the id of the *member type*, and that
/// id is the one to be written as the value of the 'id' attribute of
/// the XML element of the underlying type.
///
/// @param ctxt the context of the serialization.
///
/// @param indent the number of indentation white spaces to use.
///
/// @return true upon succesful completion, false otherwise.
static bool
write_reference_type_def(const reference_type_def_sptr&	decl,
			 const string&				id,
			 write_context&			ctxt,
			 unsigned				indent)
{
  if (!decl)
    return false;

  annotate(decl->get_canonical_type(), ctxt, indent);

  ostream& o = ctxt.get_ostream();

  do_indent(o, indent);

  o << "<reference-type-def kind='";
  if (decl->is_lvalue())
    o << "lvalue";
  else
    o << "rvalue";
  o << "'";

  type_base_sptr pointed_to_type = decl->get_pointed_to_type();
  o << " type-id='" << ctxt.get_id_for_type(pointed_to_type) << "'";

  ctxt.record_type_as_referenced(pointed_to_type);

  if (function_type_sptr f = is_function_type(decl->get_pointed_to_type()))
    ctxt.record_type_as_referenced(f);

  write_common_type_info(decl, ctxt, id);

  o << "/>\n";

  return true;
}

/// Serialize a pointer to an instance of reference_type_def.
///
/// @param decl the reference_type_def to serialize.
///
/// @param ctxt the context of the serialization.
///
/// @param indent the number of indentation white spaces to use.
///
/// @return true upon succesful completion, false otherwise.
static bool
write_reference_type_def(const reference_type_def_sptr&	decl,
			 write_context&			ctxt,
			 unsigned				indent)
{return write_reference_type_def(decl, "", ctxt, indent);}

/// Serialize a pointer to an instance of @ref ptr_to_mbr_type.
///
/// @param decl a pointer to the @ref ptr_to_mbr_type to serialize.
///
/// @param id the ID of the type.  If it's an empty string then a new
/// ID is generated.
///
/// @param ctxt the context of the serialization.
///
/// @param indent the number of indentation white spaces to use.
///
/// @return true upon succesful completion, false otherwise.
static bool
write_ptr_to_mbr_type(const ptr_to_mbr_type_sptr& decl,
		      const string& id, write_context& ctxt,
		      unsigned indent)
{
  if (!decl)
    return false;

  annotate(decl->get_canonical_type(), ctxt, indent);

  ostream& o = ctxt.get_ostream();

  do_indent(o, indent);

  o << "<pointer-to-member-type";

  type_base_sptr member_type = decl->get_member_type();
  string i = ctxt.get_id_for_type(member_type);
  o << " member-type-id='" << i << "'";
  ctxt.record_type_as_referenced(member_type);

  type_base_sptr containing_type = decl->get_containing_type();
  i = ctxt.get_id_for_type(containing_type);
  o << " containing-type-id='" << i << "'";
  ctxt.record_type_as_referenced(containing_type);

  write_common_type_info(decl, ctxt, id);

  o << "/>\n";

  return true;
}

/// Serialize a pointer to an instance of @ref ptr_to_mbr_type.
///
/// @param decl a pointer to the @ref ptr_to_mbr_type to serialize.
///
/// @param ctxt the context of the serialization.
///
/// @param indent the number of indentation white spaces to use.
///
/// @return true upon succesful completion, false otherwise.
static bool
write_ptr_to_mbr_type(const ptr_to_mbr_type_sptr& decl,
		      write_context& ctxt, unsigned indent)
{return write_ptr_to_mbr_type(decl, "", ctxt, indent);}

/// Serialize an instance of @ref array_type_def::subrange_type.
///
/// @param decl the array_type_def::subrange_type to serialize.
///
/// @param ctxt the context of the serialization.
///
/// @param indent the number of indentation white spaces to use.
///
/// return true upon successful completion, false otherwise.
static bool
write_array_subrange_type(const array_type_def::subrange_sptr&	decl,
			  write_context&			ctxt,
			  unsigned				indent)
{
  if (!decl)
    return false;

  annotate(decl, ctxt, indent);

  ostream& o = ctxt.get_ostream();

  do_indent(o, indent);

  o << "<subrange";

  if (!decl->get_name().empty())
    o << " name='" << decl->get_name() << "'";

  o << " length='";
  if (decl->is_non_finite())
    o << "unknown";
  else
    o << decl->get_length();

  o << "'";

  ABG_ASSERT(decl->is_non_finite()
	     || decl->get_length() == 0
	     || (decl->get_length() ==
		 (uint64_t) (decl->get_upper_bound()
			     - decl->get_lower_bound() + 1)));
  o << " lower-bound='" << decl->get_lower_bound() << "' upper-bound='"
    << decl->get_upper_bound() << "'";

  type_base_sptr underlying_type = decl->get_underlying_type();
  if (underlying_type)
    {
      o << " type-id='"
	<< ctxt.get_id_for_type(underlying_type)
	<< "'";
      ctxt.record_type_as_referenced(underlying_type);
    }

  write_common_type_info(decl, ctxt);

  o << "/>\n";

  return true;
}

/// Serialize a pointer to an instance of array_type_def.
///
/// @param decl the array_type_def to serialize.
///
/// @param id the type id identitifier to use in the serialized
/// output.  If this is empty, the function will compute an
/// appropriate one.  This is useful when this function is called to
/// serialize the underlying type of a member type; in that case, the
/// caller has already computed the id of the *member type*, and that
/// id is the one to be written as the value of the 'id' attribute of
/// the XML element of the underlying type.
///
/// @param ctxt the context of the serialization.
///
/// @param indent the number of indentation white spaces to use.
///
/// @return true upon succesful completion, false otherwise.
static bool
write_array_type_def(const array_type_def_sptr&	decl,
		     const string&			id,
		     write_context&			ctxt,
		     unsigned				indent)
{
  if (!decl)
    return false;

  annotate(decl, ctxt, indent);

  ostream& o = ctxt.get_ostream();

  do_indent(o, indent);
  o << "<array-type-def";

  o << " dimensions='" << decl->get_dimension_count() << "'";

  type_base_sptr element_type = decl->get_element_type();
  string type_id = ctxt.get_id_for_type(element_type);
  o << " type-id='" << type_id << "'";

  ctxt.record_type_as_referenced(element_type);

  write_common_type_info(decl, ctxt, id);

  if (!decl->get_dimension_count())
    o << "/>\n";
  else
    {
      o << ">\n";

      vector<array_type_def::subrange_sptr>::const_iterator si;

      for (si = decl->get_subranges().begin();
           si != decl->get_subranges().end(); ++si)
        {
	  unsigned local_indent =
	    indent + ctxt.get_config().get_xml_element_indent();
	  write_array_subrange_type(*si, ctxt, local_indent);
	}

      do_indent(o, indent);
      o << "</array-type-def>\n";
    }

  return true;
}

/// Serialize a pointer to an instance of array_type_def.
///
/// @param decl the array_type_def to serialize.
///
/// @param ctxt the context of the serialization.
///
/// @param indent the number of indentation white spaces to use.
///
/// @return true upon succesful completion, false otherwise.
static bool
write_array_type_def(const array_type_def_sptr& decl,
		     write_context&		ctxt,
		     unsigned			indent)
{return write_array_type_def(decl, "", ctxt, indent);}

/// Serialize a pointer to an instance of enum_type_decl.
///
/// @param decl the enum_type_decl to serialize.
///
/// @param id the type id identitifier to use in the serialized
/// output.  If this is empty, the function will compute an
/// appropriate one.  This is useful when this function is called to
/// serialize the underlying type of a member type; in that case, the
/// caller has already computed the id of the *member type*, and that
/// id is the one to be written as the value of the 'id' attribute of
/// the XML element of the underlying type.
///
/// @param ctxt the context of the serialization.
///
/// @param indent the number of indentation white spaces to use.
///
/// @return true upon succesful completion, false otherwise.
static bool
write_enum_type_decl(const enum_type_decl_sptr& d,
		     const string& id,
		     write_context& ctxt,
		     unsigned indent)
{
  if (!d)
    return false;

  enum_type_decl_sptr decl = is_enum_type(look_through_decl_only_enum(d));

  annotate(decl->get_canonical_type(), ctxt, indent);

  ostream& o = ctxt.get_ostream();

  do_indent(o, indent);
  o << "<enum-decl name='" << xml::escape_xml_string(decl->get_name()) << "'";

  write_is_artificial(decl, o);

  if (!decl->get_linkage_name().empty())
    o << " linkage-name='"
      << xml::escape_xml_string(decl->get_linkage_name())
      << "'";

  write_common_type_info(decl, ctxt, id);

  o << ">\n";

  do_indent(o, indent + ctxt.get_config().get_xml_element_indent());
  auto underlying_type = decl->get_underlying_type();
  o << "<underlying-type type-id='"
    << ctxt.get_id_for_type(underlying_type)
    << "'/>\n";

  ctxt.record_type_as_referenced(underlying_type);

  for (enum_type_decl::enumerators::const_iterator i =
	 decl->get_enumerators().begin();
       i != decl->get_enumerators().end();
       ++i)
    {
      do_indent(o, indent + ctxt.get_config().get_xml_element_indent());
      o << "<enumerator name='"
	<< i->get_name()
	<< "' value='"
	<< i->get_value()
	<< "'/>\n";
    }

  do_indent(o, indent);
  o << "</enum-decl>\n";

  return true;
}

/// Serialize a pointer to an instance of enum_type_decl.
///
/// @param decl the enum_type_decl to serialize.
///
/// @param ctxt the context of the serialization.
///
/// @param indent the number of indentation white spaces to use.
///
/// @return true upon succesful completion, false otherwise.
static bool
write_enum_type_decl(const enum_type_decl_sptr& decl,
		     write_context&		ctxt,
		     unsigned			indent)
{return write_enum_type_decl(decl, "", ctxt, indent);}

/// Serialize an @ref elf_symbol to an XML element of name
/// 'elf-symbol'.
///
/// @param sym the elf symbol to serialize.
///
/// @param ctxt the read context to use.
///
/// @param indent the number of white spaces to use as indentation.
///
/// @return true iff the function completed successfully.
static bool
write_elf_symbol(const elf_symbol_sptr&	sym,
		 write_context&		ctxt,
		 unsigned			indent)
{
  if (!sym)
    return false;

  ostream &o = ctxt.get_ostream();

  annotate(sym, ctxt, indent);
  do_indent(o, indent);
  o << "<elf-symbol name='" << xml::escape_xml_string(sym->get_name()) << "'";
  if (sym->is_variable() && sym->get_size())
  o << " size='" << sym->get_size() << "'";

  if (!sym->get_version().is_empty())
    {
      o << " version='" << sym->get_version().str() << "'";
      o << " is-default-version='";
      if (sym->get_version().is_default())
	o <<  "yes";
      else
	o << "no";
      o << "'";
    }

  write_elf_symbol_type(sym->get_type(), o);

  write_elf_symbol_binding(sym->get_binding(), o);

  write_elf_symbol_visibility(sym->get_visibility(), o);

  write_elf_symbol_aliases(*sym, o);

  o << " is-defined='";
  if (sym->is_defined())
    o << "yes";
  else
    o << "no";
  o << "'";

  if (sym->is_common_symbol())
    o << " is-common='yes'";

  if (sym->get_crc().has_value())
    o << " crc='"
      << std::hex << std::showbase << sym->get_crc().value()
      << std::dec << std::noshowbase << "'";

  if (sym->get_namespace().has_value())
    o << " namespace='" << sym->get_namespace().value() << "'";

  o << "/>\n";

  return true;
}

/// Write the elf symbol database to the output associated to the
/// current context.
///
/// @param syms the sorted elf symbol data to write out.
///
/// @param ctxt the context to consider.
///
/// @param indent the number of white spaces to use as indentation.
///
/// @return true upon successful completion.
static bool
write_elf_symbols_table(const elf_symbols&	syms,
			write_context&		ctxt,
			unsigned		indent)
{
  if (syms.empty())
    return false;

  for (elf_symbol_sptr symbol : syms)
    {
      if (ctxt.get_environment().analyze_exported_interfaces_only())
	if (!symbol->is_defined())
	  continue;
      write_elf_symbol(symbol, ctxt, indent);
    }

  return true;
}

/// Write a vector of dependency names for the current corpus we are
/// writting.
///
/// @param needed the vector of dependency names to write.
///
/// @param ctxt the write context to use for the writting.
///
/// @param indent the number of indendation spaces to use.
///
/// @return true upon successful completion, false otherwise.
static bool
write_elf_needed(const vector<string>&	needed,
		 write_context&	ctxt,
		 unsigned		indent)
{
  if (needed.empty())
    return false;

  ostream& o = ctxt.get_ostream();

  for (vector<string>::const_iterator i = needed.begin();
       i != needed.end();
       ++i)
    {
      do_indent(o, indent);
      o << "<dependency name='" << *i << "'/>\n";
    }
  return true;
}

/// Serialize a pointer to an instance of typedef_decl.
///
/// @param decl the typedef_decl to serialize.
///
/// @param id the type id identitifier to use in the serialized
/// output.  If this is empty, the function will compute an
/// appropriate one.  This is useful when this function is called to
/// serialize the underlying type of a member type; in that case, the
/// caller has already computed the id of the *member type*, and that
/// id is the one to be written as the value of the 'id' attribute of
/// the XML element of the underlying type.
///
/// @param ctxt the context of the serialization.
///
/// @param indent the number of indentation white spaces to use.
///
/// @return true upon succesful completion, false otherwise.
static bool
write_typedef_decl(const typedef_decl_sptr&	decl,
		   const string&		id,
		   write_context&		ctxt,
		   unsigned			indent)
{
  if (!decl)
    return false;

  ostream &o = ctxt.get_ostream();

  annotate(decl, ctxt, indent);

  do_indent(o, indent);

  o << "<typedef-decl name='"
    << xml::escape_xml_string(decl->get_name())
    << "'";

  type_base_sptr underlying_type = decl->get_underlying_type();
  string type_id = ctxt.get_id_for_type(underlying_type);
  o << " type-id='" <<  type_id << "'";
  ctxt.record_type_as_referenced(underlying_type);

  write_common_type_info(decl, ctxt, id);

  o << "/>\n";

  return true;
}

/// Serialize a pointer to an instance of typedef_decl.
///
/// @param decl the typedef_decl to serialize.
///
/// @param ctxt the context of the serialization.
///
/// @param indent the number of indentation white spaces to use.
///
/// @return true upon succesful completion, false otherwise.
static bool
write_typedef_decl(const typedef_decl_sptr&	decl,
		   write_context&		ctxt,
		   unsigned			indent)
{return write_typedef_decl(decl, "", ctxt, indent);}

/// Serialize a pointer to an instances of var_decl.
///
/// @param decl the var_decl to serialize.
///
/// @param ctxt the context of the serialization.
///
/// @param write_linkage_name if true, serialize the mangled name of
/// this variable.
///
/// @param indent the number of indentation white spaces to use.
///
/// @return true upon succesful completion, false otherwise.
static bool
write_var_decl(const var_decl_sptr& decl, write_context& ctxt,
	       bool write_linkage_name, unsigned indent)
{
  if (!decl)
    return false;

  string annotation_addendum;
  if (is_data_member(decl) && decl->get_type())
    annotation_addendum = maybe_emit_type_hash(ctxt, decl->get_type());
  annotate(decl, ctxt, indent, annotation_addendum);

  ostream &o = ctxt.get_ostream();

  do_indent(o, indent);

  o << "<var-decl name='" << xml::escape_xml_string(decl->get_name()) << "'";
  type_base_sptr var_type = decl->get_type();

  o << " type-id='" << ctxt.get_id_for_type(var_type) << "'";
  ctxt.record_type_as_referenced(var_type);

  if (write_linkage_name)
    {
      const string& linkage_name = decl->get_linkage_name();
      if (!linkage_name.empty())
	o << " mangled-name='" << linkage_name << "'";
    }

  write_visibility(decl, o);

  write_binding(decl, o);

  write_location(decl, ctxt);

  if (elf_symbol_sptr sym = decl->get_symbol())
    if (corpus* abi = decl->get_corpus())
      write_elf_symbol_reference(ctxt, decl->get_symbol(), *abi, o);

  o << "/>\n";

  ctxt.record_decl_as_emitted(decl);

  return true;
}

/// Test if a function and all its original artefacts (i.e, the origin
/// from where it got copied from, potentially copied from) have any
/// member decl.
///
/// @param decl the function decl to consider.
///
/// @return true iff the function and all its potential original
/// artefact have at least one member decl.
static bool
function_has_member_decls(const function_decl_sptr& decl)
{
  bool has_member_decls = false;
  for (function_decl* fn = decl.get();
       fn;
       fn = is_function_decl(fn->get_original_artefact()))
    if (!fn->get_member_decls().empty())
      {
	has_member_decls = true;
	break;
      }

  return has_member_decls;
}

/// Write the function-decl opening tag for a given function decl.
///
/// @param decl the function decl to consider.
///
/// @param ctxt the writer context to consider.
///
/// @param indent the number of white spaces to emit for indentation.
///
/// @return true iff the function decl opening tag was emitted.
static bool
write_function_decl_opening_tag(const function_decl_sptr& decl,
				write_context& ctxt, unsigned indent)
{
  if (!decl || !is_function_decl(decl))
    return false;

  ostream &o = ctxt.get_ostream();
  string annotation_addendum;
  if (is_member_function(decl)
      && get_member_function_is_virtual(decl)
      && decl->get_type())
    annotation_addendum = maybe_emit_type_hash(ctxt, decl->get_type());
  annotate(decl, ctxt, indent, annotation_addendum);

  do_indent(o, indent);
  o << "<function-decl name='"
    << xml::escape_xml_string(decl->get_name())
    << "'";

  type_base_sptr fn_type = decl->get_type();
  string type_id = ctxt.get_id_for_type(fn_type);
  o << " type-id='" << type_id << "'";
  ctxt.record_type_as_referenced(fn_type);

  if (!decl->get_linkage_name().empty())
    o << " mangled-name='"
      << xml::escape_xml_string(decl->get_linkage_name()) << "'";

  write_location(decl, ctxt);

  if (decl->is_declared_inline())
    o << " declared-inline='yes'";

  write_visibility(decl, o);

  write_binding(decl, o);

  write_size_and_alignment(decl->get_type(), o,
			   (ctxt.get_write_default_sizes()
			    ? 0
			    : decl->get_translation_unit()->get_address_size()),
			   0);
  if (elf_symbol_sptr sym = decl->get_symbol())
    if (corpus* abi = decl->get_corpus())
      write_elf_symbol_reference(ctxt, decl->get_symbol(), *abi, o);

  if (!function_has_member_decls(decl))
    o << "/";
  o << ">\n";

  return true;
}

/// Write the function-decl closing tag for a given function decl.
///
/// @param decl the function decl to consider.
///
/// @param ctxt the writer context to consider.
///
/// @param indent the number of white spaces to emit for indentation.
///
/// @return true iff the function decl closing tag was emitted.
static bool
write_function_decl_closing_tag(const function_decl_sptr& decl,
				write_context& ctxt, unsigned indent)
{
  if (!decl
      || !is_function_decl(decl)
      || !function_has_member_decls(decl))
    return false;

  ostream &o = ctxt.get_ostream();
  do_indent(o, indent);
  o << "</function-decl>\n";

  return true;
}

/// Write the parameters and return part of the ABIXML description of
/// a function_type.
///
/// Note that the function emits the canonical type of the function
/// type.  This is mainly to de-duplicate the ABIXML output.
///
/// @param fun_type the function type to consider.
///
/// @param skip_first_parm if true, the function skips the first
/// parameter of the function type.  This is useful for emitting
/// parameters of method_type IR nodes.
///
/// @param ctxt the write context to use.
///
/// @param indent the number of indentation spaces to use.
static void
write_fn_parm_and_return_types(const function_type_sptr fun_type,
			       bool skip_first_parm,
			       write_context& ctxt,
			       unsigned indent)
{
  function_type_sptr t(is_function_type(get_exemplar_type(fun_type)));

  unsigned cur_indent =
    indent + ctxt.get_config().get_xml_element_indent();

  ostream &o = ctxt.get_ostream();

  type_base_sptr parm_type;
  auto pi = t->get_parameters().begin();
  for ((skip_first_parm && pi != t->get_parameters().end()) ? ++pi: pi;
       pi != t->get_parameters().end();
       ++pi)
    {
      if ((*pi)->get_variadic_marker())
        {
          do_indent(o, cur_indent);
          o << "<parameter is-variadic='yes'";
        }
      else
	{
	  parm_type = get_exemplar_type((*pi)->get_type());

          annotate(*pi, ctxt, cur_indent);
          do_indent(o, cur_indent);

	  o << "<parameter type-id='"
	    << ctxt.get_id_for_type(parm_type)
	    << "'";
	  ctxt.record_type_as_referenced(parm_type);

	  if (ctxt.get_write_parameter_names() && !(*pi)->get_name().empty())
	    o << " name='" << xml::escape_xml_string((*pi)->get_name()) << "'";
	}
      write_is_artificial(*pi, o);
      write_location((*pi)->get_location(), ctxt);
      o << "/>\n";
    }

  type_base_sptr return_type = get_exemplar_type(t->get_return_type());
  if (return_type)
    {
      annotate(return_type , ctxt, cur_indent);
      do_indent(o, cur_indent);
      o << "<return type-id='"
	<< ctxt.get_id_for_type(return_type)
	<< "'/>\n";
      ctxt.record_type_as_referenced(return_type);
    }
}

/// Write the member declarations of a given function decl.
///
/// @param decl the function decl to consider.
///
/// @param ctxt the writer context to consider.
///
/// @param indent the number spaces to use for indentation.
static void
write_function_member_decls(const function_decl_sptr& decl,
			    write_context& ctxt, unsigned indent)
{
  const config &c = ctxt.get_config();
  function_decl* fn = decl.get();
  for (; fn; fn = is_function_decl(fn->get_original_artefact()))
    {
      write_canonical_types_of_scope(*fn, ctxt,
				     indent + c.get_xml_element_indent());
      const scope_decl::declarations& decls = fn->get_sorted_member_decls();
      for (const auto& d :decls)
	{
	  if (type_base_sptr t = is_type(d))
	    if (ctxt.type_is_emitted(t))
	      // This type has already been emitted to the current
	      // translation unit so do not emit it again.
	      continue;
	  write_decl(d, ctxt, indent + c.get_xml_element_indent());
	}
    }
}

/// Serialize a pointer to a function_decl.
///
/// @param decl the pointer to function_decl to serialize.
///
/// @param ctxt the context of the serialization.
///
///
/// @param indent the number of indentation white spaces to use.
///
/// @return true upon succesful completion, false otherwise.
static bool
write_function_decl(const function_decl_sptr& decl,
		    write_context& ctxt, unsigned indent)
{
  if (!write_function_decl_opening_tag(decl, ctxt, indent))
    return false;

  write_function_member_decls(decl, ctxt, indent);

  write_function_decl_closing_tag(decl, ctxt, indent);

  ctxt.record_decl_as_emitted(decl);

  return true;
}

/// Serialize a function_type.
///
/// @param fun_type the pointer to function_type to serialize.
///
/// @param ctxt the context of the serialization.
///
/// @param indent the number of indentation white spaces to use.
///
/// @return true upon succesful completion, false otherwise.
static bool
write_function_type(const function_type_sptr& fun_type,
		    write_context& ctxt, unsigned indent)
{
  if (!fun_type)
    return false;

  ABG_ASSERT(fun_type->get_canonical_type()
	     || is_non_canonicalized_type(fun_type));

  function_type_sptr fn_type =
    fun_type->get_canonical_type()
    ? is_function_type(fun_type->get_canonical_type())
    : fun_type;

  ostream &o = ctxt.get_ostream();

  annotate(fn_type, ctxt, indent);

  do_indent(o, indent);

  o << "<function-type";


  if (method_type_sptr method_type = is_method_type(fn_type))
    {
      o << " method-class-id='"
	<< ctxt.get_id_for_type(method_type->get_class_type())
	<< "'";

      write_cdtor_const_static(/*is_ctor=*/false, /*is_dtor=*/false,
			       /*is_const=*/method_type->get_is_const(),
			       /*is_static=*/false, o);
    }

  write_common_type_info(fn_type, ctxt);

  o << ">\n";

  write_fn_parm_and_return_types(fn_type, /*skip_first_parm=*/false,
				 ctxt, indent);

  do_indent(o, indent);

  o << "</function-type>\n";

  return true;
}

/// Write the opening tag of a member-function XML element
/// representing a member function.
///
/// @param @fn_decl the member function to emit the opening tag for.
///
/// @param ctxt the write context to consider.
///
/// @param indent the number of white space indentation to use.
///
/// @return true iff the opening tag was emitted.
static bool
write_member_function_opening_tag(const function_decl_sptr& fn_decl,
				  write_context& ctxt, unsigned indent)
{
  if (!fn_decl || !is_member_function(fn_decl))
    return false;

  ostream& o = ctxt.get_ostream();

  do_indent_to_level(ctxt, indent, 0);

  o << "<member-function";
  write_access(get_member_access_specifier(fn_decl), o);
  write_cdtor_const_static(get_member_function_is_ctor(fn_decl),
			   get_member_function_is_dtor(fn_decl),
			   get_member_function_is_const(fn_decl),
			   get_member_is_static(fn_decl),
			   o);
  if (get_member_function_is_virtual(fn_decl))
    write_voffset(fn_decl, o);
  o << ">\n";

  return true;
}

/// Write the closing tag of a member-function XML element
/// representing a member function.
///
/// @param @fn_decl the member function to emit the closing tag for.
///
/// @param ctxt the write context to consider.
///
/// @param indent the number of white space indentation to use.
///
/// @return true iff the closing tag was emitted.
static bool
write_member_function_closing_tag(const function_decl_sptr& fn_decl,
				  write_context& ctxt, unsigned indent)
{
  if (!fn_decl || !is_member_function(fn_decl))
    return false;

  ostream& o = ctxt.get_ostream();

  do_indent_to_level(ctxt, indent, 0);
  o << "</member-function>\n";

  return true;
}

/// Emit the XML element for a member function.
///
/// @param fn_decl the function decl to write the XML element for.
///
/// @param ctxt the write context to consider.
///
/// @param indent the number of white space indentation to use.
///
/// @return true iff the function emitted the XML element for the
/// member function.
static bool
write_member_function(const function_decl_sptr& fn_decl,
		      write_context& ctxt, unsigned indent)
{
  if (!fn_decl || !is_member_function(fn_decl))
    return false;

  write_member_function_opening_tag(fn_decl, ctxt,
				    get_indent_to_level(ctxt, indent, 0));

  write_function_decl(fn_decl, ctxt, get_indent_to_level(ctxt, indent, 1));

  write_member_function_closing_tag(fn_decl, ctxt,
				    get_indent_to_level(ctxt, indent, 0));

  return true;
}

/// Write the opening tag of a 'class-decl' element.
///
/// @param decl the class declaration to serialize.
///
/// @param the type ID to use for the 'class-decl' element,, or empty
/// if we need to build a new one.
///
/// @param ctxt the write context to use.
///
/// @param indent the number of white space to use for indentation.
///
/// @param prepare_to_handle_empty if set to true, then this function
/// figures out if the opening tag should be for an empty element or
/// not.  If set to false, then the opening tag is unconditionnaly for
/// a non-empty element.
///
/// @return true upon successful completion.
static bool
write_class_decl_opening_tag(const class_decl_sptr&	decl,
			     const string&		id,
			     write_context&		ctxt,
			     unsigned			indent,
			     bool			prepare_to_handle_empty)
{
  if (!decl)
    return false;

  ostream& o = ctxt.get_ostream();

  do_indent_to_level(ctxt, indent, 0);

  o << "<class-decl name='" << xml::escape_xml_string(decl->get_name()) << "'";

  write_is_struct(decl, o);

  write_is_artificial(decl, o);

  write_visibility(decl, o);

  if (decl->get_earlier_declaration())
    {
      // This instance is the definition of an earlier declaration.
      o << " def-of-decl-id='"
	<< ctxt.get_id_for_type(is_type(decl->get_earlier_declaration()))
	<< "'";
    }

  write_common_type_info(decl, ctxt, id);

  if (prepare_to_handle_empty && decl->has_no_base_nor_member())
    o << "/>\n";
  else
    o << ">\n";

  return true;
}

/// Write the opening tag of a 'union-decl' element.
///
/// @param decl the union declaration to serialize.
///
/// @param the type ID to use for the 'union-decl' element, or empty
/// if we need to build a new one.
///
/// @param ctxt the write context to use.
///
/// @param indent the number of white space to use for indentation.
///
/// @param prepare_to_handle_empty if set to true, then this function
/// figures out if the opening tag should be for an empty element or
/// not.  If set to false, then the opening tag is unconditionnaly for
/// a non-empty element.
///
/// @return true upon successful completion.
static bool
write_union_decl_opening_tag(const union_decl_sptr&	decl,
			     const string&		id,
			     write_context&		ctxt,
			     unsigned			indent,
			     bool			prepare_to_handle_empty)
{
  if (!decl)
    return false;

  ostream& o = ctxt.get_ostream();

  do_indent_to_level(ctxt, indent, 0);

  o << "<union-decl name='" << xml::escape_xml_string(decl->get_name()) << "'";

  write_visibility(decl, o);

  write_is_artificial(decl, o);

  write_common_type_info(decl, ctxt, id);

  if (prepare_to_handle_empty && decl->has_no_member())
    o << "/>\n";
  else
    o << ">\n";

  return true;
}

/// Emit the type has of a given type into the comment.
///
/// @param ctxt the write context.
///
/// @param type the type to consider.
///
/// @return string the resulting comment.
static string
maybe_emit_type_hash(write_context& ctxt, type_base_sptr type)
{
  if (ctxt.get_write_member_hashes())
    {
      ostringstream os;
      if (hash_t h = peek_hash_value(*type))
	{
	  os << ", type hash: " << std::hex << *h;
	  return os.str();
	}
    }

  return string();
}

/// Serialize a class_decl type.
///
/// @param d the pointer to class_decl to serialize.
///
/// @param id the type id identitifier to use in the serialized
/// output.  If this is empty, the function will compute an
/// appropriate one.  This is useful when this function is called to
/// serialize the underlying type of a member type; in that case, the
/// caller has already computed the id of the *member type*, and that
/// id is the one to be written as the value of the 'id' attribute of
/// the XML element of the underlying type.
///
/// @param ctxt the context of the serialization.
///
/// @param indent the initial indentation to use.
static bool
write_class_decl(const class_decl_sptr& d,
		 const string&		id,
		 write_context&	ctxt,
		 unsigned		indent)
{
  if (!d)
    return false;

  class_decl_sptr decl = is_class_type(look_through_decl_only_class(d));

  annotate(decl, ctxt, indent);

  ostream& o = ctxt.get_ostream();

  write_class_decl_opening_tag(decl, id, ctxt, indent,
			       /*prepare_to_handle_empty=*/true);

  if (!decl->has_no_base_nor_member())
    {
      unsigned nb_ws = get_indent_to_level(ctxt, indent, 1);
      type_base_sptr base_type;
      for (class_decl::base_specs::const_iterator base =
	     decl->get_base_specifiers().begin();
	   base != decl->get_base_specifiers().end();
	   ++base)
	{
	  class_decl_sptr base_class = (*base)->get_base_class();
	  base_class =
	    is_class_type(look_through_decl_only_class(base_class)->get_canonical_type());
	  annotate(base_class, ctxt, nb_ws,
		   maybe_emit_type_hash(ctxt, base_class));
	  do_indent(o, nb_ws);
	  o << "<base-class";

	  write_access((*base)->get_access_specifier(), o);

	  write_layout_offset (*base, o);

	  if ((*base)->get_is_virtual ())
	    o << " is-virtual='yes'";

	  base_type = (*base)->get_base_class();
	  o << " type-id='"
	    << ctxt.get_id_for_type(base_type)
	    << "'/>\n";

	  ctxt.record_type_as_referenced(base_type);
	}

      // Now emit the member types of this class as well as the member
      // types of the other canonical classes of the same name as this
      // one.
      abigail::ir::canonical_type_sptr_set_type mem_types;
      for (auto member_type : decl->get_sorted_member_types())
	if (ctxt.type_to_be_emitted(member_type))
	  mem_types.insert(member_type);

      vector<type_base_sptr> sorted_types;
      sort_types(mem_types, sorted_types);

      for (auto type : sorted_types)
	write_member_type(type, ctxt, nb_ws);

      // Write static data members
      for (const auto& s_dm : decl->get_static_data_members())
	{
	  do_indent(o, nb_ws);
	  o << "<data-member";
	  write_access(get_member_access_specifier(s_dm), o);

	  bool is_static = get_member_is_static(s_dm);
	  ABG_ASSERT(is_static);
	  write_cdtor_const_static(/*is_ctor=*/false,
				   /*is_dtor=*/false,
				   /*is_const=*/false,
				   /*is_static=*/is_static,
				   o);
	  write_layout_offset(s_dm, o);
	  o << ">\n";

	  write_var_decl(s_dm, ctxt, is_static,
			 get_indent_to_level(ctxt, indent, 2));

	  do_indent_to_level(ctxt, indent, 1);
	  o << "</data-member>\n";
	}

      // Write non-static data members
      for (const auto& dm : decl->get_non_static_data_members())
	{
	  do_indent(o, nb_ws);
	  o << "<data-member";
	  write_access(get_member_access_specifier(dm), o);

	  bool is_static = get_member_is_static(dm);
	  write_cdtor_const_static(/*is_ctor=*/false,
				   /*is_dtor=*/false,
				   /*is_const=*/false,
				   /*is_static=*/is_static,
				   o);
	  write_layout_offset(dm, o);
	  o << ">\n";

	  write_var_decl(dm, ctxt, is_static,
			 get_indent_to_level(ctxt, indent, 2));

	  do_indent_to_level(ctxt, indent, 1);
	  o << "</data-member>\n";
	}

      for (class_decl::member_functions::const_iterator f =
	     decl->get_member_functions().begin();
	   f != decl->get_member_functions().end();
	   ++f)
	{
	  function_decl_sptr fn = *f;
	  if (get_member_function_is_virtual(fn))
	    // All virtual member functions are emitted together,
	    // later.
	    continue;

	  ABG_ASSERT(!get_member_function_is_virtual(fn));

	  write_member_function(fn, ctxt, get_indent_to_level(ctxt, indent, 1));
	}

      for (auto& fn : decl->get_virtual_mem_fns())
	{
	  ABG_ASSERT(get_member_function_is_virtual(fn));
	  write_member_function(fn, ctxt, get_indent_to_level(ctxt, indent, 1));
	}

      for (member_function_templates::const_iterator fn =
	     decl->get_member_function_templates().begin();
	   fn != decl->get_member_function_templates().end();
	   ++fn)
	{
	  do_indent(o, nb_ws);
	  o << "<member-template";
	  write_access((*fn)->get_access_specifier(), o);
	  write_cdtor_const_static((*fn)->is_constructor(),
				   /*is_dtor=*/false,
				   (*fn)->is_const(),
				   (*fn)->get_is_static(), o);
	  o << ">\n";
	  write_function_tdecl((*fn)->as_function_tdecl(), ctxt,
			       get_indent_to_level(ctxt, indent, 2));
	  do_indent(o, nb_ws);
	  o << "</member-template>\n";
	}

      for (member_class_templates::const_iterator cl =
	     decl->get_member_class_templates().begin();
	   cl != decl->get_member_class_templates().end();
	   ++cl)
	{
	  do_indent(o, nb_ws);
	  o << "<member-template";
	  write_access((*cl)->get_access_specifier(), o);
	  write_cdtor_const_static(false, false, false,
				   (*cl)->get_is_static(), o);
	  o << ">\n";
	  write_class_tdecl((*cl)->as_class_tdecl(), ctxt,
			    get_indent_to_level(ctxt, indent, 2));
	  do_indent(o, nb_ws);
	  o << "</member-template>\n";
	}

      do_indent_to_level(ctxt, indent, 0);

      o << "</class-decl>\n";
    }

  ctxt.record_type_as_emitted(decl);

  return true;
}

/// Serialize a class_decl type.
///
/// @param decl the pointer to class_decl to serialize.
///
/// @param ctxt the context of the serialization.
///
/// @param indent the initial indentation to use.
///
/// @return true upon successful completion.
static bool
write_class_decl(const class_decl_sptr& decl,
		 write_context&	ctxt,
		 unsigned		indent)
{return write_class_decl(decl, "", ctxt, indent);}

/// Serialize a @ref union_decl type.
///
/// @param d the pointer to @ref union_decl to serialize.
///
/// @param ctxt the context of the serialization.
///
/// @param indent the initial indentation to use.
///
/// @return true upon successful completion.
static bool
write_union_decl(const union_decl_sptr& d,
		 const string& id,
		 write_context& ctxt,
		 unsigned indent)
{
  if (!d)
    return false;

  union_decl_sptr decl = is_union_type(look_through_decl_only_class(d));

  annotate(decl, ctxt, indent);

  ostream& o = ctxt.get_ostream();

  write_union_decl_opening_tag(decl, id, ctxt, indent,
			       /*prepare_to_handle_empty=*/true);
  if (!decl->has_no_member())
    {
      unsigned nb_ws = get_indent_to_level(ctxt, indent, 1);
      for (auto& member_type: decl->get_sorted_member_types())
	if (!ctxt.type_is_emitted(member_type))
	  write_member_type(member_type, ctxt, nb_ws);

      write_canonical_types_of_scope(*decl, ctxt, nb_ws,
				     /*is_member_type=*/true);

      for (union_decl::data_members::const_iterator data =
	     decl->get_data_members().begin();
	   data != decl->get_data_members().end();
	   ++data)
	{
	  do_indent(o, nb_ws);
	  o << "<data-member";
	  write_access(get_member_access_specifier(*data), o);

	  bool is_static = get_member_is_static(*data);
	  write_cdtor_const_static(/*is_ctor=*/false,
				   /*is_dtor=*/false,
				   /*is_const=*/false,
				   /*is_static=*/is_static,
				   o);
	  o << ">\n";

	  write_var_decl(*data, ctxt, is_static,
			 get_indent_to_level(ctxt, indent, 2));

	  do_indent_to_level(ctxt, indent, 1);
	  o << "</data-member>\n";
	}

      for (union_decl::member_functions::const_iterator f =
	     decl->get_member_functions().begin();
	   f != decl->get_member_functions().end();
	   ++f)
	{
	  function_decl_sptr fn = *f;
	  if (get_member_function_is_virtual(fn))
	    // All virtual member functions are emitted together,
	    // later.
	    continue;

	  ABG_ASSERT(!get_member_function_is_virtual(fn));

	  write_member_function(fn, ctxt, get_indent_to_level(ctxt, indent, 1));
	}

      for (member_function_templates::const_iterator fn =
	     decl->get_member_function_templates().begin();
	   fn != decl->get_member_function_templates().end();
	   ++fn)
	{
	  do_indent(o, nb_ws);
	  o << "<member-template";
	  write_access((*fn)->get_access_specifier(), o);
	  write_cdtor_const_static((*fn)->is_constructor(),
				   /*is_dtor=*/false,
				   (*fn)->is_const(),
				   (*fn)->get_is_static(), o);
	  o << ">\n";
	  write_function_tdecl((*fn)->as_function_tdecl(), ctxt,
			       get_indent_to_level(ctxt, indent, 2));
	  do_indent(o, nb_ws);
	  o << "</member-template>\n";
	}

      for (member_class_templates::const_iterator cl =
	     decl->get_member_class_templates().begin();
	   cl != decl->get_member_class_templates().end();
	   ++cl)
	{
	  do_indent(o, nb_ws);
	  o << "<member-template";
	  write_access((*cl)->get_access_specifier(), o);
	  write_cdtor_const_static(false, false, false,
				   (*cl)->get_is_static(), o);
	  o << ">\n";
	  write_class_tdecl((*cl)->as_class_tdecl(), ctxt,
			    get_indent_to_level(ctxt, indent, 2));
	  do_indent(o, nb_ws);
	  o << "</member-template>\n";
	}

      do_indent_to_level(ctxt, indent, 0);

      o << "</union-decl>\n";
    }

  return true;
}

static bool
write_union_decl(const union_decl_sptr& decl,
		 write_context& ctxt,
		 unsigned indent)
{return write_union_decl(decl, "", ctxt, indent);}

/// Write the opening tag for a 'member-type' element.
///
/// @param t the member type to consider.
///
/// @param ctxt the write context to use.
///
/// @param indent the number of white spaces to use for indentation.
///
/// @return true upon successful completion.
static bool
write_member_type_opening_tag(const type_base_sptr& t,
			      write_context& ctxt,
			      unsigned indent)
{
  ostream& o = ctxt.get_ostream();

  do_indent_to_level(ctxt, indent, 0);

  decl_base_sptr decl = get_type_declaration(t);
  ABG_ASSERT(decl);

  o << "<member-type";
  if (is_member_type(is_type(decl)))
    write_access(decl, o);
  o << ">\n";

  return true;
}

/// Serialize a member type.
///
/// Note that the id written as the value of the 'id' attribute of the
/// underlying type is actually the id of the member type, not the one
/// for the underying type.  That id takes in account, the access
/// specifier and the qualified name of the member type.
///
/// @param decl the declaration of the member type to serialize.
///
/// @param ctxt the write context to use.
///
/// @param indent the number of levels to use for indentation
static bool
write_member_type(const type_base_sptr& t, write_context& ctxt, unsigned indent)
{
  if (!t)
    return false;

  ostream& o = ctxt.get_ostream();

  write_member_type_opening_tag(t, ctxt, indent);

  string id = ctxt.get_id_for_type(t);

  unsigned nb_ws = get_indent_to_level(ctxt, indent, 1);
  ABG_ASSERT(write_qualified_type_def(dynamic_pointer_cast<qualified_type_def>(t),
				      id, ctxt, nb_ws)
	     || write_pointer_type_def(dynamic_pointer_cast<pointer_type_def>(t),
				   id, ctxt, nb_ws)
	     || write_reference_type_def(dynamic_pointer_cast<reference_type_def>(t),
					 id, ctxt, nb_ws)
	     || write_ptr_to_mbr_type(dynamic_pointer_cast<ptr_to_mbr_type>(t),
				      id, ctxt, nb_ws)
	     || write_array_type_def(dynamic_pointer_cast<array_type_def>(t),
				     id, ctxt, nb_ws)
	     || write_enum_type_decl(dynamic_pointer_cast<enum_type_decl>(t),
				     id, ctxt, nb_ws)
	     || write_typedef_decl(dynamic_pointer_cast<typedef_decl>(t),
				   id, ctxt, nb_ws)
	     || write_union_decl(dynamic_pointer_cast<union_decl>(t),
				 id, ctxt, nb_ws)
	     || write_class_decl(is_class_type(t), id, ctxt, nb_ws));

  do_indent_to_level(ctxt, indent, 0);
  o << "</member-type>\n";

  return true;
}

/// Serialize an instance of type_tparameter.
///
/// @param decl the instance to serialize.
///
/// @param ctxt the context of the serialization.
///
/// @param indent the initial indentation to use.
///
/// @return true upon successful completion, false otherwise.
static bool
write_type_tparameter(const type_tparameter_sptr	decl,
		      write_context&			ctxt,
		      unsigned				indent)
{
  if (!decl)
    return false;

  ostream &o = ctxt.get_ostream();
  do_indent_to_level(ctxt, indent, 0);

  string id_attr_name;
  if (ctxt.type_has_existing_id(decl))
    id_attr_name = "type-id";
  else
    id_attr_name = "id";

  o << "<template-type-parameter "
    << id_attr_name << "='" <<  ctxt.get_id_for_type(decl) << "'";

  std::string name = xml::escape_xml_string(decl->get_name ());
  if (!name.empty())
    o << " name='" << name << "'";

  write_location(decl, ctxt);

  o << "/>\n";

  ctxt.record_type_as_emitted(decl);

  return true;
}

/// Serialize an instance of non_type_tparameter.
///
/// @param decl the instance to serialize.
///
/// @param ctxt the context of the serialization.
///
/// @param indent the intial indentation to use.
///
/// @return true open successful completion, false otherwise.
static bool
write_non_type_tparameter(
 const shared_ptr<non_type_tparameter>	decl,
 write_context&	ctxt, unsigned indent)
{
  if (!decl)
    return false;

  ostream &o = ctxt.get_ostream();
  do_indent_to_level(ctxt, indent, 0);

  o << "<template-non-type-parameter type-id='"
    << ctxt.get_id_for_type(decl->get_type())
    << "'";

  string name = xml::escape_xml_string(decl->get_name());
  if (!name.empty())
    o << " name='" << name << "'";

  write_location(decl, ctxt);

  o << "/>\n";

  return true;
}

/// Serialize an instance of template template parameter.
///
/// @param decl the instance to serialize.
///
/// @param ctxt the context of the serialization.
///
/// @param indent the initial indentation to use.
///
/// @return true upon successful completion, false otherwise.

static bool
write_template_tparameter (const template_tparameter_sptr	decl,
			   write_context&			ctxt,
			   unsigned				indent)
{
  if (!decl)
    return false;

  ostream& o = ctxt.get_ostream();
  do_indent_to_level(ctxt, indent, 0);

  string id_attr_name = "id";
  if (ctxt.type_has_existing_id(decl))
    id_attr_name = "type-id";

  o << "<template-template-parameter " << id_attr_name << "='"
    << ctxt.get_id_for_type(decl) << "'";

  string name = xml::escape_xml_string(decl->get_name());
  if (!name.empty())
    o << " name='" << name << "'";

  o << ">\n";

  unsigned nb_spaces = get_indent_to_level(ctxt, indent, 1);
  for (list<shared_ptr<template_parameter> >::const_iterator p =
	 decl->get_template_parameters().begin();
       p != decl->get_template_parameters().end();
       ++p)
    write_template_parameter(decl, ctxt, nb_spaces);

  do_indent_to_level(ctxt, indent, 0);
  o << "</template-template-parameter>\n";

  ctxt.record_type_as_emitted(decl);

  return true;
}

/// Serialize an instance of type_composition.
///
/// @param decl the decl to serialize.
///
/// @param ctxt the context of the serialization.
///
/// @param indent the initial indentation to use.
///
/// @return true upon successful completion, false otherwise.
static bool
write_type_composition
(const shared_ptr<type_composition> decl,
 write_context& ctxt, unsigned indent)
{
  if (!decl)
    return false;

  ostream& o = ctxt.get_ostream();

  do_indent_to_level(ctxt, indent, 0);

  o << "<template-parameter-type-composition>\n";

  unsigned nb_spaces = get_indent_to_level(ctxt, indent, 1);
  (write_pointer_type_def
   (dynamic_pointer_cast<pointer_type_def>(decl->get_composed_type()),
			  ctxt, nb_spaces)
   || write_reference_type_def
   (dynamic_pointer_cast<reference_type_def>(decl->get_composed_type()),
    ctxt, nb_spaces)
   || write_array_type_def
   (dynamic_pointer_cast<array_type_def>(decl->get_composed_type()),
    ctxt, nb_spaces)
   || write_qualified_type_def
   (dynamic_pointer_cast<qualified_type_def>(decl->get_composed_type()),
    ctxt, nb_spaces));

  do_indent_to_level(ctxt, indent, 0);
  o << "</template-parameter-type-composition>\n";

  return true;
}

/// Serialize an instance of template_parameter.
///
/// @param decl the instance to serialize.
///
/// @param ctxt the context of the serialization.
///
/// @param indent the initial indentation to use.
///
/// @return true upon successful completion, false otherwise.
static bool
write_template_parameter(const shared_ptr<template_parameter> decl,
			 write_context& ctxt, unsigned indent)
{
  if ((!write_type_tparameter
       (dynamic_pointer_cast<type_tparameter>(decl), ctxt, indent))
      && (!write_non_type_tparameter
	  (dynamic_pointer_cast<non_type_tparameter>(decl),
	   ctxt, indent))
      && (!write_template_tparameter
	  (dynamic_pointer_cast<template_tparameter>(decl),
	   ctxt, indent))
      && (!write_type_composition
	  (dynamic_pointer_cast<type_composition>(decl),
	   ctxt, indent)))
    return false;

  return true;
}

/// Serialize the template parameters of the a given template.
///
/// @param tmpl the template for which to emit the template parameters.
static void
write_template_parameters(const shared_ptr<template_decl> tmpl,
			  write_context& ctxt, unsigned indent)
{
  if (!tmpl)
    return;

  unsigned nb_spaces = get_indent_to_level(ctxt, indent, 1);
  for (list<shared_ptr<template_parameter> >::const_iterator p =
	 tmpl->get_template_parameters().begin();
       p != tmpl->get_template_parameters().end();
       ++p)
    write_template_parameter(*p, ctxt, nb_spaces);
}

/// Serialize an instance of function_tdecl.
///
/// @param decl the instance to serialize.
///
/// @param ctxt the context of the serialization
///
/// @param indent the initial indentation.
static bool
write_function_tdecl(const shared_ptr<function_tdecl> decl,
		     write_context& ctxt, unsigned indent)
{
  if (!decl)
    return false;

  ostream& o = ctxt.get_ostream();

  do_indent_to_level(ctxt, indent, 0);

  o << "<function-template-decl id='" << ctxt.get_id_for_fn_tmpl(decl) << "'";

  write_location(decl, ctxt);

  write_visibility(decl, o);

  write_binding(decl, o);

  o << ">\n";

  write_template_parameters(decl, ctxt, indent);

  write_function_decl(decl->get_pattern(), ctxt,
		      get_indent_to_level(ctxt, indent, 1));

  do_indent_to_level(ctxt, indent, 0);

  o << "</function-template-decl>\n";

  return true;
}


/// Serialize an instance of class_tdecl
///
/// @param decl a pointer to the instance of class_tdecl to serialize.
///
/// @param ctxt the context of the serializtion.
///
/// @param indent the initial number of white space to use for
/// indentation.
///
/// @return true upon successful completion, false otherwise.
static bool
write_class_tdecl(const shared_ptr<class_tdecl> decl,
		  write_context& ctxt, unsigned indent)
{
  if (!decl)
    return false;

  ostream& o = ctxt.get_ostream();

  do_indent_to_level(ctxt, indent, 0);

  o << "<class-template-decl id='" << ctxt.get_id_for_class_tmpl(decl) << "'";

  write_location(decl, ctxt);

  write_visibility(decl, o);

  o << ">\n";

  write_template_parameters(decl, ctxt, indent);

  write_class_decl(decl->get_pattern(), ctxt,
		   get_indent_to_level(ctxt, indent, 1));

  do_indent_to_level(ctxt, indent, 0);

  o << "</class-template-decl>\n";

  return true;
}

/// Serialize the current version number of the ABIXML format.
///
/// @param ctxt the writing context to use.
static void
write_version_info(write_context& ctxt)
{
  ostream& o = ctxt.get_ostream();
  const config& c = ctxt.get_config();

  o << "version='"
    << c.get_format_major_version_number()
    << "." << c.get_format_minor_version_number()
    << "'";
}

/// Serialize an ABI corpus to a single native xml document.  The root
/// note of the resulting XML document is 'abi-corpus'.
///
/// Note: If either corpus is null or corpus does not contain serializable
///       content (i.e. corpus.is_empty()), nothing is emitted to the ctxt's
///       output stream.
///
/// @param ctxt the write context to use.
///
/// @param corpus the corpus to serialize.
///
/// @param indent the number of white space indentation to use.
///
/// @return true upon successful completion, false otherwise.
bool
write_corpus(write_context&	ctxt,
	     const corpus_sptr& corpus,
	     unsigned		indent,
	     bool		member_of_group)
{
  if (!corpus)
    return false;

  if (corpus->is_empty())
    return true;

  do_indent_to_level(ctxt, indent, 0);

  std::ostream& out = ctxt.get_ostream();
  const config &conf = ctxt.get_config();

  out << "<abi-corpus ";

  write_version_info(ctxt);

  // For an abi-corpus as part of an abi-corpus group, only omit the path, but
  // keep the filename.
  std::string corpus_path = corpus->get_path();
  if (!ctxt.get_write_corpus_path())
    {
      if (member_of_group)
	tools_utils::base_name(corpus_path, corpus_path);
      else
	corpus_path.clear();
    }
  else
    {
      if (ctxt.get_short_locs())
	tools_utils::base_name(corpus_path, corpus_path);
    }
  if (!corpus_path.empty())
    out << " path='" << xml::escape_xml_string(corpus_path) << "'";

  if (!corpus->get_architecture_name().empty()
      && ctxt.get_write_architecture())
    out << " architecture='" << corpus->get_architecture_name()<< "'";

  if (!corpus->get_soname().empty())
    out << " soname='" << corpus->get_soname()<< "'";

  write_tracking_non_reachable_types(corpus, out);

  out << ">\n";

  // Write the list of needed corpora.

  if (ctxt.get_write_elf_needed () && !corpus->get_needed().empty())
    {
      do_indent_to_level(ctxt, indent, 1);
      out << "<elf-needed>\n";
      write_elf_needed(corpus->get_needed(), ctxt,
		       get_indent_to_level(ctxt, indent, 2));
      do_indent_to_level(ctxt, indent, 1);
      out << "</elf-needed>\n";
    }

  // Write the function symbols data base.
  if (!corpus->get_fun_symbol_map().empty())
    {
      do_indent_to_level(ctxt, indent, 1);
      out << "<elf-function-symbols>\n";

      write_elf_symbols_table(corpus->get_sorted_fun_symbols(), ctxt,
			      get_indent_to_level(ctxt, indent, 2));

      do_indent_to_level(ctxt, indent, 1);
      out << "</elf-function-symbols>\n";
    }

  // Write the variable symbols data base.
  if (!corpus->get_var_symbol_map().empty())
    {
      do_indent_to_level(ctxt, indent, 1);
      out << "<elf-variable-symbols>\n";

      write_elf_symbols_table(corpus->get_sorted_var_symbols(), ctxt,
			      get_indent_to_level(ctxt, indent, 2));

      do_indent_to_level(ctxt, indent, 1);
      out << "</elf-variable-symbols>\n";
    }

  // Write the undefined function symbols database.
  if (ctxt.get_write_undefined_symbols()
      && !corpus->get_sorted_undefined_fun_symbols().empty())
    {
      do_indent_to_level(ctxt, indent, 1);
      out << "<undefined-elf-function-symbols>\n";

      write_elf_symbols_table(corpus->get_sorted_undefined_fun_symbols(), ctxt,
			      get_indent_to_level(ctxt, indent, 2));

      do_indent_to_level(ctxt, indent, 1);
      out << "</undefined-elf-function-symbols>\n";
    }


  // Write the undefined variable symbols database.
  if (ctxt.get_write_undefined_symbols()
      && !corpus->get_sorted_undefined_var_symbols().empty())
    {
      do_indent_to_level(ctxt, indent, 1);
      out << "<undefined-elf-variable-symbols>\n";

      write_elf_symbols_table(corpus->get_sorted_undefined_var_symbols(), ctxt,
			      get_indent_to_level(ctxt, indent, 2));

      do_indent_to_level(ctxt, indent, 1);
      out << "</undefined-elf-variable-symbols>\n";
    }

  // Write the types of the corpus
  write_abi_types(ctxt, *corpus, indent + conf.get_xml_element_indent());

  // Now write the translation units.
  for (auto tu : corpus->get_translation_units())
    write_translation_unit(ctxt, *tu, get_indent_to_level(ctxt, indent, 1));

  do_indent_to_level(ctxt, indent, 0);
  out << "</abi-corpus>\n";

  ctxt.clear_referenced_types();
  ctxt.record_corpus_as_emitted(corpus);

  return true;
}

/// Serialize an ABI corpus group to a single native xml document.
/// The root note of the resulting XML document is 'abi-corpus-group'.
///
/// @param ctxt the write context to use.
///
/// @param group the corpus group to serialize.
///
/// @param indent the number of white space indentation to use.
///
/// @return true upon successful completion, false otherwise.
bool
write_corpus_group(write_context&	    ctxt,
		   const corpus_group_sptr& group,
		   unsigned		    indent)

{
  if (!group)
    return false;

  do_indent_to_level(ctxt, indent, 0);

std::ostream& out = ctxt.get_ostream();

  out << "<abi-corpus-group ";
  write_version_info(ctxt);

  if (!group->get_path().empty() && ctxt.get_write_corpus_path())
    out << " path='" << xml::escape_xml_string(group->get_path()) << "'";

  if (!group->get_architecture_name().empty() && ctxt.get_write_architecture())
    out << " architecture='" << group->get_architecture_name()<< "'";

  write_tracking_non_reachable_types(group, out);

  if (group->is_empty())
    {
      out << "/>\n";
      return true;
    }

  out << ">\n";

  // Write the list of corpora
  for (corpus_group::corpora_type::const_iterator c =
	 group->get_corpora().begin();
       c != group->get_corpora().end();
       ++c)
    {
      ABG_ASSERT(!ctxt.corpus_is_emitted(*c));
      write_corpus(ctxt, *c, get_indent_to_level(ctxt, indent, 1), true);
    }

  do_indent_to_level(ctxt, indent, 0);
  out << "</abi-corpus-group>\n";

  return true;
}

} //end namespace xml_writer

// <Debugging routines>

using namespace abigail::ir;

/// Serialize a pointer to decl_base to an output stream.
///
/// @param d the pointer to decl_base to serialize.
///
/// @param o the output stream to consider.
///
/// @param annotate whether ABIXML output should be annotated.
void
dump(const decl_base_sptr d, std::ostream& o, const bool annotate)
{
  xml_writer::write_context ctxt(d->get_environment(), o);
  xml_writer::set_annotate(ctxt, annotate);
  write_decl(d, ctxt, /*indent=*/0);
}

/// Serialize a pointer to decl_base to stderr.
///
/// @param d the pointer to decl_base to serialize.
///
/// @param annotate whether ABIXML output should be annotated.
void
dump(const decl_base_sptr d, const bool annotate)
{dump(d, cerr, annotate);}

/// Serialize a pointer to type_base to an output stream.
///
/// @param t the pointer to type_base to serialize.
///
/// @param o the output stream to serialize the @ref type_base to.
///
/// @param annotate whether ABIXML output should be annotated.
void
dump(const type_base_sptr t, std::ostream& o, const bool annotate)
{dump(get_type_declaration(t), o, annotate);}

/// Serialize a pointer to type_base to stderr.
///
/// @param t the pointer to type_base to serialize.
///
/// @param annotate whether ABIXML output should be annotated.
void
dump(const type_base_sptr t, const bool annotate)
{dump(t, cerr, annotate);}

/// Serialize a pointer to var_decl to an output stream.
///
/// @param v the pointer to var_decl to serialize.
///
/// @param o the output stream to serialize the @ref var_decl to.
///
/// @param annotate whether ABIXML output should be annotated.
void
dump(const var_decl_sptr v, std::ostream& o, const bool annotate)
{
  xml_writer::write_context ctxt(v->get_environment(), o);
  xml_writer::set_annotate(ctxt, annotate);
  write_var_decl(v, ctxt, /*linkage_name*/true, /*indent=*/0);
}

/// Serialize a pointer to var_decl to stderr.
///
/// @param v the pointer to var_decl to serialize.
///
/// @param annotate whether ABIXML output should be annotated.
void
dump(const var_decl_sptr v, const bool annotate)
{dump(v, cerr, annotate);}

/// Serialize a @ref translation_unit to an output stream.
///
/// @param t the translation_unit to serialize.
///
/// @param o the outpout stream to serialize the translation_unit to.
///
/// @param annotate whether ABIXML output should be annotated.
void
dump(const translation_unit& t, std::ostream& o, const bool annotate)
{
  xml_writer::write_context ctxt(t.get_environment(), o);
  xml_writer::set_annotate(ctxt, annotate);
  write_translation_unit(ctxt, t, /*indent=*/0);
}

/// Serialize an instance of @ref translation_unit to stderr.
///
/// @param t the translation_unit to serialize.
void
dump(const translation_unit& t, const bool annotate)
{dump(t, cerr, annotate);}

/// Serialize a pointer to @ref translation_unit to an output stream.
///
/// @param t the @ref translation_unit_sptr to serialize.
///
/// @param o the output stream to serialize the translation unit to.
///
/// @param annotate whether ABIXML output should be annotated.
void
dump(const translation_unit_sptr t, std::ostream& o, const bool annotate)
{
  if (t)
    dump(*t, o, annotate);
}

/// Serialize a pointer to @ref translation_unit to stderr.
///
/// @param t the translation_unit_sptr to serialize.
///
/// @param annotate whether ABIXML output should be annotated.
void
dump(const translation_unit_sptr t, const bool annotate)
{
  if (t)
    dump(*t, annotate);
}

/// Serialize a source location to an output stream.
///
/// @param l the declaration to consider.
///
/// @param o the output stream to serialize to.
void
dump_location(const location& l, ostream& o)
{
  string path;
  unsigned line = 0, col = 0;

  l.expand(path, line, col);
  o << path << ":" << line << "," << col << "\n";
}

/// Serialize a source location for debugging purposes.
///
/// The location is serialized to the standard error output stream.
///
/// @param l the declaration to consider.
///
void
dump_location(const location& l)
{dump_location(l, cerr);}

/// Serialize the source location of a decl to an output stream for
/// debugging purposes.
///
/// @param d the declaration to consider.
///
/// @param o the output stream to serizalize the location to.
void
dump_decl_location(const decl_base& d, ostream& o)
{dump_location(d.get_location(), o);}

/// Serialize the source location of a decl to stderr for debugging
/// purposes.
///
/// @param d the declaration to consider.
void
dump_decl_location(const decl_base& d)
{dump_decl_location(d, cerr);}

/// Serialize the source location of a dcl to stderr for debugging
/// purposes.
///
/// @param d the declaration to consider.
void
dump_decl_location(const decl_base* d)
{
  if (d)
    dump_decl_location(*d);
}

/// Serialize the source location of a decl to stderr for debugging
/// purposes.
///
/// @param d the declaration to consider.
void
dump_decl_location(const decl_base_sptr d)
{dump_decl_location(d.get());}

#ifdef WITH_DEBUG_SELF_COMPARISON
/// Write one of the records of the "type-ids" debugging file.
///
/// This is a sub-routine of write_canonical_type_ids.
///
/// @param ctxt the context to use.
///
/// @param type the type which canonical type pointer value to emit.
///
/// @param o the output stream to write to.
static void
write_type_record(xml_writer::write_context&	ctxt,
		  const type_base*		type,
		  ostream&			o)
{
  // We want to serialize a type record which content looks like:
  //
  //     <type>
  //       <id>type-id-573</id>
  //       <c>0x262ee28</c>
  //     </type>
  //     <type>
  //       <id>type-id-569</id>
  //       <c>0x2628298</c>
  //     </type>
  //     <type>
  //       <id>type-id-575</id>
  //       <c>0x25f9ba8</c>
  //     </type>

  type_base_sptr canonical = type->get_canonical_type();
  ABG_ASSERT(canonical);
  string id ;
  id = ctxt.get_id_for_type (const_cast<type_base*>(type));

  o << "  <type>\n"
    << "    <id>" << id << "</id>\n"
    << "    <c>"
    << std::hex
    << reinterpret_cast<uintptr_t>(canonical.get())
    << "</c>\n"
    << "  </type>\n";
}

/// Serialize the map that is stored at
/// environment::get_type_id_canonical_type_map() to an output stream.
///
/// This is for debugging purposes and is triggered ultimately by
/// invoking the command 'abidw --debug-abidiff <binary>'.
///
/// @param ctxt the write context.
///
/// @param o the output stream to serialize the map to.
void
write_canonical_type_ids(xml_writer::write_context& ctxt, ostream& o)
{
  // We want to serialize a file which content looks like:
  //
  // <abixml-types-check>
  //     <type>
  //       <id>type-id-573</id>
  //       <c>0x262ee28</c>
  //     </type>
  //     <type>
  //       <id>type-id-569</id>
  //       <c>0x2628298</c>
  //     </type>
  //     <type>
  //       <id>type-id-575</id>
  //       <c>0x25f9ba8</c>
  //     </type>
  // <abixml-types-check>

  o << "<abixml-types-check>\n";

  for (const auto &type : ctxt.get_emitted_types_set())
    write_type_record(ctxt, type, o);

  o << "</abixml-types-check>\n";
}

/// Serialize the map that is stored at
/// environment::get_type_id_canonical_type_map() to a file.
///
/// This is for debugging purposes and is triggered ultimately by
/// invoking the command 'abidw --debug-abidiff <binary>'.
///
/// @param ctxt the write context.
///
/// @param file_path the file to serialize the map to.
bool
write_canonical_type_ids(xml_writer::write_context& ctxt,
			const string &file_path)
{
  std:: ofstream o (file_path);

  if (!o.is_open())
    return true;
  write_canonical_type_ids(ctxt, o);
  o.close();
  return true;
}
#endif
// </Debugging routines>
} //end namespace abigail
