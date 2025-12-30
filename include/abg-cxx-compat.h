// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// -*- Mode: C++ -*-
//
// Copyright (C) 2019-2026 Google, Inc.

/// @file

#ifndef __ABG_CXX_COMPAT_H
#define __ABG_CXX_COMPAT_H

// C++17 support (via custom implementations if compiled with earlier standard)

#if __cplusplus >= 201703L

#include <optional>
#include <tuple>
#include <utility>

#else

#include <stdexcept> // for throwing std::runtime_error("bad_optional_access")
#include <tuple>
#include <utility>

#endif

namespace abg_compat
{

#if __cplusplus >= 201703L

using std::optional;
using std::tuple;

#else

// <optional>

/// Simplified implementation of std::optional just enough to be used as a
/// replacement for our purposes and when compiling with pre C++17.
///
/// The implementation intentionally does not support a whole lot of features
/// to minimize the maintenance effort with this.
template <typename T> class optional
{
  bool has_value_;
  T    value_;

public:
  optional() : has_value_(false), value_() {}
  optional(const T& value) : has_value_(true), value_(value) {}

  bool
  has_value() const noexcept
  {
    return has_value_;
  }

  const T&
  value() const
  {
    if (!has_value_)
      throw std::runtime_error("bad_optional_access");
    return value_;
  }

  const T
  value_or(const T& default_value) const
  {
    if (!has_value_)
      return default_value;
    return value_;
  }

  const T&
  operator*() const& noexcept
  { return value_; }

  T&
  operator*() & noexcept
  { return value_; }

  const T*
  operator->() const noexcept
  { return &value_; }

  T*
  operator->() noexcept
  { return &value_; }

  optional&
  operator=(const T& value)
  {
    has_value_ = true;
    value_ = value;
    return *this;
  }

  void
  reset()
  {
    has_value_ = false;
  }

  explicit operator bool() const noexcept { return has_value(); }
};

template <typename T, typename U>
bool
operator==(const optional<T>& lhs, const optional<U>& rhs)
{
  if (!lhs.has_value() && !rhs.has_value())
    return true;
  if (!lhs.has_value() || !rhs.has_value())
    return false;
  return lhs.value() == rhs.value();
}

template <typename T, typename U>
bool
operator!=(const optional<T>& lhs, const optional<U>& rhs)
{
  return !(lhs == rhs);
}

// </std::optional>

// <std::apply>

template <typename F, typename Tuple, std::size_t... I>
constexpr decltype(auto)
apply_impl(F&& f, Tuple&& t, std::index_sequence<I...>)
{
  return std::forward<F>(f)(std::get<I>(std::forward<Tuple>(t))...);
}

template <typename F, typename Tuple>
constexpr decltype(auto)
apply(F&& f, Tuple&& t)
{
  constexpr std::size_t tuple_size =
    std::tuple_size<std::remove_reference_t<Tuple>>::value;
    return apply_impl(std::forward<F>(f),
		      std::forward<Tuple>(t),
		      std::make_index_sequence<tuple_size>{});
}
// <std::apply/>

#endif // __cplusplus >= 201703L

#if __cplusplus >= 202002L
using std::views::reverse;
#else

namespace views
{

/// This is a wrapper class for an iterable container.  It's aim is to
/// allow the iteration in an order that is the reverse of the default
/// order of the underlying iterable container.
template <typename T>
struct reverse_wrapper
{
  T& iterable_;

  /// @return a 'begin' iterator which is actually the std::rbegin()
  /// iterator of the underlying container.
  auto begin() const
  {return std::rbegin(iterable_);}

  /// @return a 'end' iterator which is actually the std::rend()
  /// iterator of the underlying container.
  auto end() const
  {return std::rend(iterable_);}
}; // end struct reverse_wrapper


/// Return the @ref reverse_wrapper container associated with a given
/// container.
///
/// This is to be used as below to iterate over a container in the
/// reverse order:
///
///   for (auto& item : reverse(container))
///     do_something(item);
///
/// @param iterable the container to consider
///
/// @return the reverse_wrapper<T> associated to the container T.
template <typename T>
reverse_wrapper<T>
reverse(T& iterable)
{
  return {iterable};
}
}// end namespace views

#endif // __cplusplus >= 202002L

}

#endif  // __ABG_CXX_COMPAT_H
