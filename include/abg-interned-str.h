// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// -*- Mode: C++ -*-
//
// Copyright (C) 2016-2026 Red Hat, Inc.
//
// Author: Dodji Seketeli

/// @file
///
/// Declaration of types pertaining to the interned string pool used
/// throughout Libabigail, for performance reasons.
///
/// For the record, the concept of the String Interning method is
/// explained at https://en.wikipedia.org/wiki/String_interning.

#ifndef __ABG_INTERNED_STR_H__
#define __ABG_INTERNED_STR_H__

#include <functional>
#include <memory>
#include <ostream>
#include <string>
#include <unordered_set>


namespace abigail
{
// Inject some std types into this namespace.
using std::unordered_set;
using std::string;
using std::ostream;

/// The abstraction of an interned string.
///
/// It's a wrapper around a pointer to a std::string, along with a set
/// of method that helps make this string integrate with std::string
/// seamlessly.  For instance, the type provides equality operators
/// that help compare it against std::string.
///
/// Note that this @ref interned_string type is design to have the
/// same size as a pointer to a string.
class interned_string
{
  interned_string(string* raw);

public:
  struct priv;
  std::unique_ptr<priv> priv_;

  interned_string();

  ~interned_string();

  interned_string(const interned_string& o);

  interned_string&
  operator=(const interned_string& o);

  void
  clear();

  bool
  empty() const;

  const string*
  raw() const;

  bool
  operator==(const interned_string& o) const;

  bool
  operator!=(const interned_string& o) const;

  bool
  operator==(const string& o) const;

  bool
  operator!=(const string& o) const;

  bool
  operator<(const interned_string& o) const;

  operator string() const;

  friend class interned_string_pool;
}; // end class interned_string

/// A functor to hash instances of @ref interned_string.
struct hash_interned_string
{
  /// The hash operator.
  ///
  /// It's super fast because hashing an interned string amounts to
  /// hashing the pointer to it's underlying string.  It's because
  /// every distinct string is present only in one copy in the
  /// environment.
  ///
  /// @param s the instance of @ref interned_string to hash.
  ///
  /// @return the returned hash value.
  size_t
  operator()(const interned_string& s) const
  {
    std::hash<size_t> hash_size_t;
    return hash_size_t(reinterpret_cast<size_t>(s.raw()));
  }
}; // end struct hash_interned_string

/// Convenience typedef for a set of @ref interned_string
typedef unordered_set<interned_string,
		      hash_interned_string> interned_string_set_type;

/// The interned string pool.
///
/// This is where all the distinct strings represented by the interned
/// strings leave.  The pool is the actor responsible for creating
/// interned strings.
class interned_string_pool
{
  struct priv;

public:
  std::unique_ptr<priv> priv_;
  interned_string_pool();

  interned_string
  create_string(const std::string&);

  interned_string
  create_string() const;

  bool
  has_string(const char* s) const;

  const char*
  get_string(const char* s) const;

  ~interned_string_pool();
}; // end class interned_string_pool

bool
operator==(const string& l, const interned_string& r);

bool
operator!=(const string& l, const interned_string& r);

ostream&
operator<<(ostream& o, const interned_string& s);

string
operator+(const interned_string& s1,const string& s2);

string
operator+(const string& s1, const interned_string& s2);

} // end namespace abigail

#endif // __ABG_INTERNED_STR_H__
