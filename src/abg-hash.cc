// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// -*- mode: C++ -*-
//
// Copyright (C) 2013-2026 Red Hat, Inc.

/// @file

#include <functional>
#include <cstring>
#include <xxhash.h>
#include "abg-internal.h"
#include "abg-ir-priv.h"

// <headers defining libabigail's API go under here>
ABG_BEGIN_EXPORT_DECLARATIONS

#include "abg-hash.h"
#include "abg-ir.h"

ABG_END_EXPORT_DECLARATIONS
// </headers defining libabigail's API>

namespace abigail
{

namespace hashing
{

/// Read a character representing an hexadecimal digit (from '0' to
/// 'f' or to 'F'), and return an integer representing the value of
/// that digit.  For instance, for the character '0', the function
/// returns the integer 0.  For the character 'A' (or 'a'), the
/// function returns the integer 10; for the character 'b' (or 'B')
/// the function returns the integer 11.
///
/// @param c the input character to transform into an integer.
///
/// @param integer output value.  This is set by the function to the
/// integer representing the character @p c.
///
/// @return true iff @p c is a character representing an hexadecimal
/// number which value could be set to @p integer.
static bool
char_to_int(char c, unsigned char& integer)
{
  if (c >= '0' && c <= '9')
    integer = c - '0';
  else if (c >= 'a' && c <= 'z')
    integer = 10 + c - 'a';
  else if (c >= 'A' && c <= 'Z')
    integer = 10 + c  - 'A';
  else
    return false;

  return true;
}

/// Given an integer value representing an hexadecimal digit (from 0
/// to F), emit the character value which prints that digit.  For the
/// integer 11, the function returns the character 'b'.  For the
/// integer 10, it returns the character 'a'.
///
/// @param integer the input hexadecimal integer digit to take into
/// account.
///
/// @param c the output character representing the digit @p integer.
///
/// @return true iff @p integer is a valid hexadecimal digit that
/// could could be represented by a character @p c.
static bool
int_to_char(unsigned char integer, unsigned char& c)
{
  if (integer <= 9)
    c = integer + '0';
  else if (integer >= 0xA && integer <= 0xF)
    c = 'a' + (integer - 0xA);
  else
    return false;

  return true;
}

/// Read a string of characters representing a string of hexadecimal
/// digits which itself represents a hash value that was computed
/// using the XH64 algorithm from the xxhash project.
///
/// That string of digit (characters) is laid out in the "canonical
/// form" requested by the xxhash project.  That form is basically the
/// hash number, represented in big endian.
///
/// @param input the input string of characters to consider.
///
/// @param hash the resulting hash value de-serialized from @p input.
/// This is set by the function iff it returns true.
///
/// @return true iff the function could de-serialize the characters
/// string @p input into the hash value @p hash.
bool
deserialize_hash(const string& input, uint64_t& hash)
{
  unsigned char byte = 0;
  string xxh64_canonical_form;
  for (size_t i = 0; i + 1 < input.size(); i += 2)
    {
      unsigned char first_nibble = 0, second_nibble = 0;
      ABG_ASSERT(char_to_int(input[i], first_nibble));
      ABG_ASSERT(char_to_int(input[i+1], second_nibble));
      byte = (first_nibble << 4) | second_nibble;
      xxh64_canonical_form.push_back(byte);
    }

  XXH64_canonical_t canonical_hash = {};
  size_t size = sizeof(canonical_hash.digest);
  memcpy(canonical_hash.digest,
	 xxh64_canonical_form.c_str(),
	 size);
  hash = XXH64_hashFromCanonical(&canonical_hash);

  return true;
}

/// Serialiaze a hash value computed using the XH64 algorithm (from the
/// xxhash project) into a string of characters representing the
/// digits of the hash in the canonical form requested by the xxhash
/// project.  That canonical form is basically a big endian
/// representation of the hexadecimal hash number.
///
/// @param hash the hash number to serialize.
///
/// @param output the resulting string of characters representing the
/// hash value @p hash in its serialized form.  This is set iff the
/// function return true.
///
/// @return true iff the function could serialize the hash value @p
/// hash into a serialized form that is set into the output parameter
/// @p output.
bool
serialize_hash(uint64_t hash, string& output)
{
  XXH64_canonical_t canonical_output = {};
  XXH64_canonicalFromHash(&canonical_output, hash);
  for (unsigned i = 0; i < sizeof(canonical_output.digest); ++i)
    {
      unsigned char first_nibble = 0, second_nibble = 0;
      unsigned char byte = canonical_output.digest[i];
      first_nibble = (0xf0 & byte) >> 4;
      second_nibble = 0xf & byte;
      unsigned char c = 0;
      int_to_char(first_nibble, c);
      output.push_back(c);
      int_to_char(second_nibble, c);
      output.push_back(c);
    }

  return true;
}

// </serialized_hash_type definitions>

/// Combine two hash values to produce a third hash value.
///
/// If one of the hash values is empty then the other one is returned,
/// intact.  If the two hash values are empty then an empty hash value
/// is returned as a result.
///
/// @param val1 the first hash value.
///
/// @param val2 the second hash value.
///
/// @return a combination of the hash values @p val1 and @p val2.
hash_t
combine_hashes(hash_t val1, hash_t val2)
{
  hash_t result;
  if (val1.has_value() && val2.has_value())
    result = hash(*val2, *val1);
  else if (val1.has_value())
    result = *val1;
  else if (val2.has_value())
    result = *val2;

  return result;
}

/// Hash an integer value and combine it with a hash previously
/// computed.
///
/// @param v the value to hash.
///
/// @param seed a previous hash value that is to be combined with the
/// result of hashing @p v.  This is can be zero if no previous hash
/// value is available.
///
/// @return the resulting hash value.
hash_t
hash(uint64_t v, uint64_t seed)
{
  // THe XXH hashing functions take an array of bytes representing the
  // value to hash.  So let's represent 'v' as a big endian input and
  // pass it to XXH3_64bits_withSeed.
  unsigned char data[sizeof(uint64_t)] = {};
  uint64_t t = v;
  size_t data_size = sizeof(data);
  for (unsigned i = 0; i < data_size; ++i)
    {
      data[data_size - i - 1] = t & 0xff;
      t = t >> 8;
    }
  hash_t h = XXH3_64bits_withSeed(data, data_size, seed);
  return h;
}

/// Hash a string.
///
/// @param str the string to hash.
///
/// @return the resulting hash value.
hash_t
hash(const std::string& str)
{
  hash_t h = XXH3_64bits(str.c_str(), str.size());
  return h;
}

/// Compute a stable string hash.
///
/// std::hash has no portability or stability guarantees so is
/// unsuitable where reproducibility is a requirement such as in XML
/// output.
///
/// This is the 32-bit FNV-1a algorithm. The algorithm, reference code
/// and constants are all unencumbered. It is fast and has reasonable
/// distribution properties.
///
/// https://en.wikipedia.org/wiki/Fowler-Noll-Vo_hash_function
///
/// @param str the string to hash.
///
/// @return an unsigned 32 bit hash value.
uint32_t
fnv_hash(const std::string& str)
{
  const uint32_t prime = 0x01000193;
  const uint32_t offset_basis = 0x811c9dc5;
  uint32_t hash = offset_basis;
  for (std::string::const_iterator i = str.begin(); i != str.end(); ++i)
    {
      uint8_t byte = *i;
      hash = hash ^ byte;
      hash = hash * prime;
    }
  return hash;
}

/// Get the hashing state of an IR node.
///
/// @param tod the type or decl IR node to get the hashing state for.
///
/// @return the hashing state of @p tod.
hashing::hashing_state
get_hashing_state(const type_or_decl_base& tod)
{
  const type_or_decl_base* todp = &tod;
  if (decl_base *d = is_decl(todp))
    {
      d = look_through_decl_only(d);
      return d->type_or_decl_base::priv_->get_hashing_state();
    }
  else
    return tod.priv_->get_hashing_state();
}

/// Set the hashing state of an IR node.
///
/// @param tod the type or decl IR node to set the hashing state for.
///
///
/// @param s the new hashing state to set.
void
set_hashing_state(const type_or_decl_base& tod,
		  hashing::hashing_state s)
{
  const type_or_decl_base* todp = &tod;
  if (decl_base* d = is_decl(todp))
    {
      d = look_through_decl_only(d);
      d->type_or_decl_base::priv_->set_hashing_state(s);
    }
  else
    tod.priv_->set_hashing_state(s);
}

/// Add a state to the hashing state of a given IR node.
///
/// @param tod the type of decl IR node to set the hashing state for.
///
/// @param s the new state to add to the hashing state of the node.
void
add_to_hashing_state(const ir::type_or_decl_base& tod,
		     hashing::hashing_state s)
{
  hashing::hashing_state h = get_hashing_state(tod);

  h = static_cast<hashing::hashing_state>(h | s);
  hashing::set_hashing_state(tod, h);

}

/// Remove a state to the hashing state of a given IR node.
///
/// @param tod the type of decl IR node to set the hashing state for.
///
/// @param s the new state to add to the hashing state of the node.
void
remove_from_hashing_state(const ir::type_or_decl_base& tod,
			  hashing::hashing_state s)
{
  hashing::hashing_state h = get_hashing_state(tod);

  h = static_cast<hashing::hashing_state>(h & ~s);
  hashing::set_hashing_state(tod, h);
}

}//end namespace hashing

using std::list;
using std::vector;

using namespace abigail::ir;

namespace ir
{

/// Compute (or get) the hash of a sub-type of the type for which we
/// are currently computing the hash.  It does so by preventing
/// infinite loops.
///
/// The function detects if the sub-type is currently already being
/// walked for hash computation; if it's the case then it returns an
/// empty hash to prevent infinite looping.  Otherwise it computes the
/// hash of the sub-type.
///
/// @param t the sub-type node to consider.
///
/// @return the resulting hash.  It can be empty to prevent infinite
/// looping.
///
hash_t
get_subtype_hash(const type_or_decl_base* t)
{
  if (!t)
    return hash_t();

  if (hashing::get_hashing_state(*t) == hashing::HASHING_FINISHED_STATE)
    return peek_hash_value(*t);
  else if (hashing::get_hashing_state(*t) & hashing::HASHING_CYCLED_TYPE_STATE)
    return hash_t();
  else if (hashing::get_hashing_state(*t) & hashing::HASHING_STARTED_STATE)
    {
      hashing::add_to_hashing_state(*t, hashing::HASHING_CYCLED_TYPE_STATE);
      return hash_t();
    }

  hash_t v = t->hash_value();
  hashing::remove_from_hashing_state(*t, hashing::HASHING_CYCLED_TYPE_STATE);

  return v;
}

/// Compute (or get) the hash of a sub-type of the type for which we
/// are currently computing the hash.  It does so by preventing
/// infinite loops.
///
/// The function detects if the sub-type is currently already being
/// walked for hash computation; if it's the case then it returns an
/// empty hash to prevent infinite looping.  Otherwise it computes the
/// hash of the sub-type.
///
/// @param t the sub-type node to consider.
///
/// @return the resulting hash.  It can be empty to prevent infinite
/// looping.
///
hash_t
get_subtype_hash(type_or_decl_base_sptr t)
{
  return get_subtype_hash(t.get());
}

}// end namespace ir

// See forward declarations in abg-ir.h.

// Definitions.

#define MAYBE_RETURN_EARLY_FROM_HASHING_TO_AVOID_CYCLES(type)		\
  do									\
    {									\
      if (hashing::get_hashing_state(type) == hashing::HASHING_FINISHED_STATE) \
	return peek_hash_value(type);					\
      else if (hashing::get_hashing_state(type) & hashing::HASHING_CYCLED_TYPE_STATE) \
	return hash_t();						\
    }									\
  while(false)

#if 0
/// This is for debugging purposes.
///
/// It emits the hash value of an artifact, if its DWARF offset is
/// among a set of given DWARF offset numbers.
///
/// @param artifact the artifact to consider.
///
/// @param native_offsets the set of DWARF offsets the artifact should
/// have to for its hash value to be emitted.
///
/// @param artifact_number this is an arbitrary number provided by the
/// caller to identify the artifact in the output.
///
/// @param h the hash value of the artifact to emit.
void
debug_hash_value(const type_or_decl_base& artifact,
		 std::unordered_set<uint32_t> native_offsets,
		 int artifact_number,
		 hash_t h)
{
  if (offset_t o = artifact.get_native_offset())
    if (native_offsets.find(*o) != native_offsets.end())
      {
	std::cerr << "hash of '"
		  << artifact.get_pretty_representation()
		  << "'/"
		  << artifact_number
		  << " at offset "
		  << std::hex
		  << *o
		  << std::dec
		  << " is '"
		  << std::hex
		  << *h
		  << std::dec
		  << std::endl;
	  }
}
#endif

/// Hash a type as a basic type having a name and size attributes.
///
/// This hashing takes into account the name of the type as as its
/// size.  The name taken into account is the same name that is taken
/// into account by the type canonicalization process in
/// type_base::get_canonical_type_for.
///
/// @param t the type to hash.
///
/// @return the hash value.
static hash_t
hash_as_basic_type(const abigail::ir::type_base& t)
{
  type_base::hash hash_as_type;
  decl_base::hash hash_as_decl;
  qualified_type_def::hash hash_as_qualified;

  hash_t h = hash_as_type(t);
  if (auto q = is_qualified_type(&t))
    h = hashing::combine_hashes(h, hash_as_qualified(*q));
  else if (auto d = is_decl(&t))
    h = hashing::combine_hashes(h, hash_as_decl(*d));

  return h;
}

/// Hash a type as a basic type having a name and size attributes.
///
/// This hashing takes into account the name of the type as as its
/// size.  The name taken into account is the same name that is taken
/// into account by the type canonicalization process in
/// type_base::get_canonical_type_for.
///
/// @param t the type to hash.
///
/// @return the hash value.
static hash_t
hash_as_basic_type(const type_base_sptr t)
{
  if (!t)
    return hash_t();
  return hash_as_basic_type(*t);
}

/// Depending on the kind of the type, hash it as a pointer, a
/// reference or a typedef.
///
/// @param t the type to hash.
///
/// @return the hash value.
static hash_t
hash_as_ptr_ref_or_typedef_type(const type_base_sptr& t)
{
  reference_type_def::hash hash_reference;
  pointer_type_def::hash hash_pointer;
  typedef_decl::hash hash_typedef;
  hash_t h;

  if (auto p = is_pointer_type(t))
    h = hash_pointer(*p);
  else if (auto r = is_reference_type(t))
    h = hash_reference(*r);
  else if (auto ty = is_typedef(t))
    h = hash_typedef(*ty);

  return h;
}

/// Hash a given type as a basic type, then depending on its type,
/// hash it further as a pointer, reference, or typedef type.
///
/// @param t the type to consider.
///
/// @return the resulting hash value.
static hash_t
hash_as_basic_then_ptr_ref_or_typedef_type(const type_base_sptr& t)
{
  hash_t h = hash_as_basic_type(t);
  h = hashing::combine_hashes(h, hash_as_ptr_ref_or_typedef_type(t));
  return h;
}

/// The hashing functor for using instances of @ref type_or_decl_base
/// as values in a hash map or set.

/// Hash function for an instance of @ref type_base.
///
/// @param t the type to hash.
///
/// @return the type value.
hash_t
type_base::hash::operator()(const type_base& t) const
{
  hash_t v = hashing::hash(t.get_size_in_bits());
  v = hashing::combine_hashes(v, hashing::hash(t.get_alignment_in_bits()));
  return v;
}

/// Hash function for an instance of @ref type_base.
///
/// @param t the type to hash.
///
/// @return the type value.
hash_t
type_base::hash::operator()(const type_base* t) const
{return operator()(*t);}

/// Hash function for an instance of @ref type_base.
///
/// @param t the type to hash.
///
/// @return the hash value.
hash_t
type_base::hash::operator()(const type_base_sptr t) const
{return operator()(*t);}

/// Hash function for an instance of @ref decl_base.
///
/// @param d the decl to hash.
///
/// @return the hash value.
hash_t
decl_base::hash::operator()(const decl_base& d) const
{
  hash_t v = 0;

  string repr;

  // Make sure the representation doesn't make the difference between
  // a struct and a class so that we have the same hash for both.
  repr = d.get_cached_pretty_representation(/*internal=*/true);

  v = hashing::hash(repr);

  if (is_member_decl(d))
    v = hashing::combine_hashes(v, hashing::hash(get_member_is_static(d)));

  return v;
}

/// Hash function for an instance of @ref decl_base.
///
/// @param d the decl to hash.
///
/// @return the hash value.
hash_t
decl_base::hash::operator()(const decl_base* d) const
{
  if (!d)
    return 0;
  return operator()(*d);
}

/// Hashing function for a @ref type_decl IR node.
///
/// @param t the @ref type_decl IR node t hash.
///
/// @return the resulting hash value.
hash_t
type_decl::hash::operator()(const type_decl& t) const
{
  MAYBE_RETURN_EARLY_FROM_HASHING_TO_AVOID_CYCLES(t);

  decl_base::hash decl_hash;
  type_base::hash type_hash;

  hashing::add_to_hashing_state(t, hashing::HASHING_STARTED_STATE);

  hash_t v = decl_hash(t);
  v = hashing::combine_hashes(v, type_hash(t));

  hashing::set_hashing_state(t, hashing::HASHING_NOT_DONE_STATE);

  t.set_hash_value(v);

  return v;
}

/// Hashing function for a @ref type_decl IR node.
///
/// @param t the @ref type_decl IR node to hash.
///
/// @return the resulting hash value.
hash_t
type_decl::hash::operator()(const type_decl* t) const
{
  if (!t)
    return 0;
  return operator()(*t);
}

/// Hashing function for a @ref typedef_decl IR node.
///
/// @param t the @ref typedef_decl IR node to hash
///
/// @return the resulting hash value.
hash_t
typedef_decl::hash::operator()(const typedef_decl& t) const
{
  MAYBE_RETURN_EARLY_FROM_HASHING_TO_AVOID_CYCLES(t);

  hashing::add_to_hashing_state(t, hashing::HASHING_STARTED_STATE);

  auto u = peel_typedef_type(&t);
  hash_t h = get_subtype_hash(u);

  hashing::set_hashing_state(t, hashing::HASHING_NOT_DONE_STATE);

  return h;
}

/// Hashing function for a @ref typedef_decl IR node.
///
/// @param t the @ref typedef_decl IR node to hash
///
/// @return the resulting hash value.
hash_t
typedef_decl::hash::operator()(const typedef_decl* t) const
{
  if (!t)
    return 0;
  return operator()(*t);
}

/// Hashing function for a @ref qualified_type_def IR node.
///
/// @param t the @ref qualified_type_def IR node to hash.
///
/// @return the resulting hash value.
hash_t
qualified_type_def::hash::operator()(const qualified_type_def& t) const
{
  MAYBE_RETURN_EARLY_FROM_HASHING_TO_AVOID_CYCLES(t);

  type_base::hash type_hash;
  decl_base::hash decl_hash;

  hashing::add_to_hashing_state(t, hashing::HASHING_STARTED_STATE);

  hash_t v = type_hash(t);
  v = hashing::combine_hashes(v, hashing::hash(t.get_cv_quals()));
  type_base_sptr u = look_through_decl_only_type(t.get_underlying_type());
  v = hashing::combine_hashes(v, hash_as_basic_then_ptr_ref_or_typedef_type(u));
  if (auto d = is_decl(u))
    v = hashing::combine_hashes(v, decl_hash(*d));

  hashing::set_hashing_state(t, hashing::HASHING_NOT_DONE_STATE);

  return v;
}

/// Hashing function for a @ref qualified_type_def IR node.
///
/// @param t the @ref qualified_type_def IR node to hash.
///
/// @return the resulting hash value.
hash_t
qualified_type_def::hash::operator()(const qualified_type_def* t) const
{
  if (!t)
    return 0;
  return operator()(*t);
}


/// Hashing function for a @ref pointer_type_def IR node.
///
/// @param t the @ref pointer_type_def IR node to hash.
///
/// @return the resulting hash value.
hash_t
pointer_type_def::hash::operator()(const pointer_type_def& t) const
{
  MAYBE_RETURN_EARLY_FROM_HASHING_TO_AVOID_CYCLES(t);

  type_base::hash type_base_hash;
  decl_base::hash decl_hash;

  hashing::add_to_hashing_state(t, hashing::HASHING_STARTED_STATE);

  hash_t v = type_base_hash(t);
  v = hashing::combine_hashes(v, decl_hash(t));
  type_base_sptr u = look_through_decl_only_type(t.get_pointed_to_type());
  hashing::combine_hashes(v, type_base_hash(t));
  if (auto d = is_decl(u))
    v = hashing::combine_hashes(v, decl_hash(*d));


  hashing::set_hashing_state(t, hashing::HASHING_NOT_DONE_STATE);

  return v;
}

/// Hashing function for a @ref pointer_type_def IR node.
///
/// @param t the @ref pointer_type_def IR node to hash.
///
/// @return the resulting hash value.
hash_t
pointer_type_def::hash::operator()(const pointer_type_def* t) const
{
  if (!t)
    return 0;
  return operator()(*t);
}

/// Hashing function for a @ref reference_type_def IR node.
///
/// @param t the @ref reference_type_def IR node to hash.
///
/// @return the resulting hash value.
hash_t
reference_type_def::hash::operator()(const reference_type_def& t) const
{
  MAYBE_RETURN_EARLY_FROM_HASHING_TO_AVOID_CYCLES(t);

  hashing::add_to_hashing_state(t, hashing::HASHING_STARTED_STATE);

  hash_t v = hash_as_basic_type(t);
  v = hashing::combine_hashes(v, hashing::hash(t.is_lvalue()));
  type_base_sptr u = t.get_pointed_to_type();
  v = hashing::combine_hashes(v, hash_as_basic_type(u));

  hashing::set_hashing_state(t, hashing::HASHING_NOT_DONE_STATE);

  return v;
}

/// Hashing function for a @ref reference_type_def IR node.
///
/// @param t the @ref reference_type_def IR node to hash.
///
/// @return the resulting hash value.
hash_t
reference_type_def::hash::operator()(const reference_type_def* t) const
{
  if (!t)
    return 0;
  return operator()(*t);
}

/// Hashing function for a @ref array_type_def::subrange_type IR node.
///
/// @param t the @ref array_type_def::subrange_type IR node to hash.
///
/// @return the resulting hash value.
hash_t
array_type_def::subrange_type::hash::operator()(const array_type_def::subrange_type& t) const
{
  hashing::add_to_hashing_state(t, hashing::HASHING_STARTED_STATE);

  hash_t v = hashing::hash(t.get_lower_bound());
  v = hashing::combine_hashes(v, hashing::hash(t.get_upper_bound()));
  v = hashing::combine_hashes(v, hashing::hash(t.get_name()));

  hashing::set_hashing_state(t, hashing::HASHING_NOT_DONE_STATE);

  return v;
}

/// Hashing function for a @ref array_type_def::subrange_type IR node.
///
/// @param t the @ref array_type_def::subrange_type IR node to hash.
///
/// @return the resulting hash value.
hash_t
array_type_def::subrange_type::hash::operator()(const array_type_def::subrange_type* s) const
{
  if (!s)
    return 0;
  return operator()(*s);
}

/// Hashing function for a @ref array_type_def IR node.
///
/// @param t the @ref array_type_def IR node to hash.
///
/// @return the resulting hash value.
hash_t
array_type_def::hash::operator()(const array_type_def& t) const
{
  MAYBE_RETURN_EARLY_FROM_HASHING_TO_AVOID_CYCLES(t);

  type_base::hash hash_as_type_base;
  decl_base::hash hash_as_decl_base;

  hashing::add_to_hashing_state(t, hashing::HASHING_STARTED_STATE);

  hash_t v = hash_as_type_base(t), h = 0;
  v = hashing::combine_hashes(v, hash_as_decl_base(t));

  for (vector<array_type_def::subrange_sptr >::const_iterator i =
	 t.get_subranges().begin();
       i != t.get_subranges().end();
       ++i)
    {
      auto type = *i;
      h = hash_as_type_base(*type);
      v = hashing::combine_hashes(v, h);
      v = hashing::combine_hashes(v, hash_as_decl_base(*type));
    }

  type_base_sptr e = t.get_element_type();

  if (auto d = is_decl(e))
    {
      v = hashing::combine_hashes(v, hash_as_decl_base(*d));
      v = hashing::combine_hashes(v, hash_as_type_base(*e));
    }

  hashing::set_hashing_state(t, hashing::HASHING_NOT_DONE_STATE);

  return v;
}

/// Hashing function for a @ref array_type_def IR node.
///
/// @param t the @ref array_type_def IR node to hash.
///
/// @return the resulting hash value.
hash_t
array_type_def::hash::operator()(const array_type_def* t) const
{
  if (!t)
    return 0;
  return operator()(*t);
}

/// Hashing function for a @ref ptr_to_mbr_type IR node.
///
/// @param t the @ref ptr_to_mbr_type IR node to hash.
///
/// @return the resulting hash value.
hash_t
ptr_to_mbr_type::hash::operator() (const ptr_to_mbr_type& t) const
{
  MAYBE_RETURN_EARLY_FROM_HASHING_TO_AVOID_CYCLES(t);

  type_base::hash hash_as_type_base;
  decl_base::hash hash_as_decl_base;

  hashing::add_to_hashing_state(t, hashing::HASHING_STARTED_STATE);

  hash_t v = hash_as_type_base(t);
  v = hashing::combine_hashes(v, hash_as_decl_base(t));
  type_base_sptr e = t.get_member_type();

  hash_t h = hash_as_basic_then_ptr_ref_or_typedef_type(e);
  v = hashing::combine_hashes(v, h);

  e = t.get_containing_type();

  h = hash_as_basic_then_ptr_ref_or_typedef_type(e);
  v = hashing::combine_hashes(v, h);

  hashing::set_hashing_state(t, hashing::HASHING_NOT_DONE_STATE);

  return v;
}

/// Hashing function for a @ref ptr_to_mbr_type IR node.
///
/// @param t the @ref ptr_to_mbr_type IR node to hash.
///
/// @return the resulting hash value.
hash_t
ptr_to_mbr_type::hash::operator() (const ptr_to_mbr_type* t) const
{
  if (!t)
    return 0;
  return operator()(*t);
}

/// Hashing function for a @ref ptr_to_mbr_type IR node.
///
/// @param t the @ref ptr_to_mbr_type IR node to hash.
///
/// @return the resulting hash value.
hash_t
ptr_to_mbr_type::hash::operator() (const ptr_to_mbr_type_sptr& t) const
{return operator()(t.get());}

/// Hashing function for a @ref enum_type_decl IR node.
///
/// @param t the @ref enum_type_decl IR node to hash.
///
/// @return the resulting hash value.
hash_t
enum_type_decl::hash::operator()(const enum_type_decl& t) const
{
  MAYBE_RETURN_EARLY_FROM_HASHING_TO_AVOID_CYCLES(t);

  if (t.get_is_declaration_only() && t.get_definition_of_declaration())
    {
      enum_type_decl_sptr e = is_enum_type(t.get_definition_of_declaration());

      hash_t v = hash_as_basic_then_ptr_ref_or_typedef_type(e);
      return v;
    }

  decl_base::hash hash_as_decl;
  type_base::hash hash_as_type;

  hashing::add_to_hashing_state(t, hashing::HASHING_STARTED_STATE);

  hash_t v = hash_as_type(t);
  v = hashing::combine_hashes(v, hash_as_decl(t));

  type_base_sptr u = t.get_underlying_type();

  hash_t h = hash_as_basic_then_ptr_ref_or_typedef_type(u);
  v = hashing::combine_hashes(v, h);

  for (enum_type_decl::enumerators::const_iterator i =
	 t.get_enumerators().begin();
       i != t.get_enumerators().end();
       ++i)
    {
      v = hashing::combine_hashes(v, hashing::hash(i->get_name()));
      v = hashing::combine_hashes(v, hashing::hash(i->get_value()));
    }

  hashing::set_hashing_state(t, hashing::HASHING_NOT_DONE_STATE);

  return v;
}

/// Hashing function for a @ref enum_type_decl IR node.
///
/// @param t the @ref enum_type_decl IR node to hash.
///
/// @return the resulting hash value.
hash_t
enum_type_decl::hash::operator()(const enum_type_decl* t) const
{
  if (!t)
    return 0;
  return operator()(*t);
}

/// Hashing function for @ref function_type.
///
/// @param t the function type to hash.
///
/// @return the resulting hash value.
hash_t
function_type::hash::operator()(const function_type& t) const
{
  MAYBE_RETURN_EARLY_FROM_HASHING_TO_AVOID_CYCLES(t);

  add_to_hashing_state(t, hashing::HASHING_STARTED_STATE);

  hash_t h = hash_as_basic_type(t);
  type_base_sptr r = t.get_return_type();

  h = hashing::combine_hashes(h, hash_as_basic_type(r));

  for (auto parm : t.get_parameters())
    {
      type_base_sptr type = parm->get_type();
      h = hashing::combine_hashes
	(h, hash_as_basic_then_ptr_ref_or_typedef_type(type));
    }

  hashing::set_hashing_state(t, hashing::HASHING_NOT_DONE_STATE);

  return h;
}

/// Hashing function for a pointer to @ref function_type.
///
/// @param t the pointer to @ref function_type to hash.
///
/// @return the resulting hash value.
hash_t
function_type::hash::operator()(const function_type* t) const
{
  if (!t)
    return 0;
  return operator()(*t);
}

/// Hashing function for a shared pointer to @ref function_type.
///
/// @param t the pointer to @ref function_type to hash.
///
/// @return the resulting hash value.
hash_t
function_type::hash::operator()(const function_type_sptr t) const
{return operator()(t.get());}

/// Hashing function for a @ref method_type IR node.
///
/// @param t the @ref method_type IR node to hash.
///
/// @return the resulting hash value.
hash_t
method_type::hash::operator()(const method_type& t) const
{
  MAYBE_RETURN_EARLY_FROM_HASHING_TO_AVOID_CYCLES(t);

  add_to_hashing_state(t, hashing::HASHING_STARTED_STATE);

  hash_t v = hash_as_basic_type(t);
  type_base_sptr r = t.get_return_type();
  v = hashing::combine_hashes(v, hash_as_basic_then_ptr_ref_or_typedef_type(r));

  bool c = t.get_is_const();
  v = hashing::combine_hashes(v, hashing::hash(c));

  // We are not hashing the class type.  Rather, we are hashing it
  // indirectly by hashing the first parameter which should be the
  // implicit "this" pointer for types of non-static method.
  //
  // Doing this allows not taking into account the class type for
  // method_type of static methods, while taking it into account for
  // non-static methods.

  for (auto parm : t.get_parameters())
    {
      type_base_sptr ty = parm->get_type();
      v = hashing::combine_hashes
	(v, hash_as_basic_then_ptr_ref_or_typedef_type(ty));
    }

  hashing::set_hashing_state(t, hashing::HASHING_NOT_DONE_STATE);

  return v;
}

/// Hashing function for a @ref method_type IR node.
///
/// @param t the @ref method_type IR node to hash.
///
/// @return the resulting hash value.
hash_t
method_type::hash::operator()(const method_type* t) const
{return operator()(*t);}

/// Hashing function for a @ref method_type IR node.
///
/// @param t the @ref method_type IR node to hash.
///
/// @return the resulting hash value.
hash_t
method_type::hash::operator()(const method_type_sptr t) const
{return operator()(t.get());}

/// Hashing function for a @ref member_base IR node.
///
/// @param t the @ref member_base IR node to hash.
///
/// @return the resulting hash value.
hash_t
member_base::hash::operator()(const member_base& m) const
{
  return hashing::hash(m.get_access_specifier());
}

/// Hashing function for a @ref class_decl::base_spec IR node.
///
/// @param t the @ref class_decl::base_spec IR node to hash.
///
/// @return the resulting hash value.
hash_t
class_decl::base_spec::hash::operator()(const base_spec& t) const
{
  MAYBE_RETURN_EARLY_FROM_HASHING_TO_AVOID_CYCLES(t);

  add_to_hashing_state(t, hashing::HASHING_STARTED_STATE);

  member_base::hash hash_member;

  hash_t v = hash_member(t), h = 0;;
  v = hashing::combine_hashes(v, hashing::hash(t.get_offset_in_bits()));
  v = hashing::combine_hashes(v, hashing::hash(t.get_is_virtual()));
  type_base_sptr b = t.get_base_class();

  h = hash_as_basic_then_ptr_ref_or_typedef_type(b);
  v = hashing::combine_hashes(v, h);

  hashing::set_hashing_state(t, hashing::HASHING_NOT_DONE_STATE);

  return v;
}

/// Hashing function for a @ref class_decl::base_spec IR node.
///
/// @param t the @ref class_decl::base_spec IR node to hash.
///
/// @return the resulting hash value.
hash_t
class_decl::base_spec::hash::operator()(const base_spec* t) const
{
  if (!t)
    return 0;
  return operator()(*t);
}

/// Compute a hash for a @ref class_or_union
///
/// @param t the class_or_union for which to compute the hash value.
///
/// @return the computed hash value.
hash_t
class_or_union::hash::operator()(const class_or_union& t) const
{
  // If the type is decl-only and now has a definition, then hash its
  // definition instead.

  if (t.get_is_declaration_only() && t.get_definition_of_declaration())
    {
      class_or_union_sptr cou = is_class_or_union_type(look_through_decl_only_class(t));
      hash_t v = operator()(*cou);
      return v;
    }

  type_base::hash hash_as_type_base;
  decl_base::hash hash_as_decl_base;

  hash_t v = hash_as_type_base(t);
  v = hashing::combine_hashes(v, hash_as_decl_base(t));

  // Hash data members.
  type_base_sptr ty;
  for (auto d = t.get_non_static_data_members().begin();
       d != t.get_non_static_data_members().end();
       ++d)
    {
      ty = (*d)->get_type();
      hash_t h = hash_as_basic_type(ty);
      v = hashing::combine_hashes(v, h);
      v = hashing::combine_hashes(v, hashing::hash((*d)->get_name()));
    }

  return v;
};

/// Compute a hash for a @ref class_or_union
///
/// @param t the class_or_union for which to compute the hash value.
///
/// @return the computed hash value.
hash_t
class_or_union::hash::operator()(const class_or_union *t) const
{return t ? operator()(*t) : 0;}

/// Compute a hash for a @ref class_decl
///
/// @param t the class_decl for which to compute the hash value.
///
/// @return the computed hash value.
hash_t
class_decl::hash::operator()(const class_decl& t) const
{
  MAYBE_RETURN_EARLY_FROM_HASHING_TO_AVOID_CYCLES(t);

  // If the type is decl-only and now has a definition, then hash its
  // definition instead.

  if (t.get_is_declaration_only() && t.get_definition_of_declaration())
    {
      class_decl_sptr c = is_class_type(look_through_decl_only_class(t));
      hash_t v = operator()(*c);
      hashing::set_hashing_state(t, hashing::HASHING_NOT_DONE_STATE);
      return v;
    }

  add_to_hashing_state(t, hashing::HASHING_STARTED_STATE);

  class_or_union::hash hash_as_class_or_union;

  hash_t v = hash_as_class_or_union(t);

  hash_t h;
  // Hash bases.
  for (auto b : t.get_base_specifiers())
    {
      h = hash_as_basic_then_ptr_ref_or_typedef_type(b->get_base_class());
      v = hashing::combine_hashes(v, h);
    }

  // Do not hash (virtual) member functions because in C++ at least,
  // due to the function cloning used to implement destructors (and
  // maybe other functions in the future) comparing two sets of
  // virtual destructors is a bit more involved than what we could
  // naively do with by just hashing the virtual member functions.
  // You can look at the overload of the equals function for
  // class_decl, in abg-ir.cc to see the dance involved in comparing
  // virtual member functions.  Maybe in the future we can come up
  // with a clever way to hash these.  For now, let's rely on
  // structural comparison to tell the virtual member functions part
  // of classes appart.

  // If we were to hash virtual member functions naively, please find
  // below what it would look like.  Note that it doesn't work in
  // practise as it creates spurious self-comparison errors.  You
  // might want to test it on this command and see for yourself:
  //
  //       fedabipkgdiff --self-compare --from fc37 gcc-gnat

  // TODO BIS: OK, actually, now that the DWARF reader improved enough
  // to better represent the virtual member functions, we can try
  // again to naively hash them.  Please find below how it would work.
  //
  // Hash virtual member functions.

  // TODO: hash the linkage names of the virtual member functions too.
  const_cast<class_decl&>(t).sort_virtual_mem_fns();
  for (const auto& method : t.get_virtual_mem_fns())
    {
      // Do not hash virtual destructors as these can be in different
      // numbers in two classes and yet the two classes can be
      // equivalent.
      if (get_member_function_is_dtor(method))
	continue;

      string linkage_name = method->get_linkage_name();
      ABG_ASSERT(!linkage_name.empty());
      h = hashing::hash(linkage_name);
      v = hashing::combine_hashes(v, h);
      ssize_t voffset = get_member_function_vtable_offset(method);
      h = hashing::hash(voffset);
      v = hashing::combine_hashes(v, h);
    }

  hashing::set_hashing_state(t, hashing::HASHING_NOT_DONE_STATE);

  return v;
}

/// Compute a hash for a @ref class_decl
///
/// @param t the class_decl for which to compute the hash value.
///
/// @return the computed hash value.
hash_t
class_decl::hash::operator()(const class_decl* t) const
{return t ? operator()(*t) : 0;}

/// Hashing function for a @ref union_decl IR node.
///
/// @param t the @ref union_decl IR node to hash.
///
/// @return the resulting hash value.
hash_t
union_decl::hash::operator()(const union_decl& t) const
{
  MAYBE_RETURN_EARLY_FROM_HASHING_TO_AVOID_CYCLES(t);

  // If the type is decl-only and now has a definition, then hash its
  // definition instead.

  if (t.get_is_declaration_only() && t.get_definition_of_declaration())
    {
      union_decl_sptr u = is_union_type(t.get_definition_of_declaration());
      hash_t v = operator()(*u);
      hashing::set_hashing_state(t, hashing::HASHING_NOT_DONE_STATE);
      return v;
    }

  add_to_hashing_state(t, hashing::HASHING_STARTED_STATE);

  class_or_union::hash hash_as_class_or_union;

  hash_t v = hash_as_class_or_union(t);

  hashing::set_hashing_state(t, hashing::HASHING_NOT_DONE_STATE);

  return v;
}

/// Hashing function for a @ref union_decl IR node.
///
/// @param t the @ref union_decl IR node to hash.
///
/// @return the resulting hash value.
hash_t
union_decl::hash::operator()(const union_decl*t) const
{
  if (!t)
    return 0;
  return operator()(*t);
}
}//end namespace abigail
