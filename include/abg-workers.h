// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// -*- Mode: C++ -*-
//
// Copyright (C) 2013-2026 Red Hat, Inc.
//
// Author: Dodji Seketeli

/// @file
///
/// This file declares an interface for the worker threads (or thread
/// pool) design pattern.  It aims at performing a set of tasks in
/// parallel, using the multi-threading capabilities of the underlying
/// processor(s).
///

#ifndef __ABG_WORKERS_H__
#define __ABG_WORKERS_H__

#include <functional>
#include <memory>
#include <vector>

#include "abg-cxx-compat.h"

using std::shared_ptr;

namespace abigail
{

/// The namespace of the worker threads (or thread pool)
/// implementation of libabigail.  This was modelled after the article
/// https://en.wikipedia.org/wiki/Thread_pool.
namespace workers
{

size_t get_number_of_threads();

size_t get_number_of_available_threads();

/// This represents a task to be performed.
///
/// Each instance of this type represents a task that can be performed
/// concurrently to other instance of the same type.
///
/// An instance of @ref task is meant to be performed by a worker
/// (thread).  A set of tasks can be stored in a @ref queue.
class task
{
public:
  virtual void
  perform() = 0;

  virtual ~task(){};
}; // end class task.

typedef shared_ptr<task> task_sptr;

/// The template of a task to be performed.
///
/// The function to be performed by the task, its return type and its
/// list of parameters are parameters of the template.
///
/// @tparam Fn the function to be executed by the instantiation.
///
/// @tparam RetType the return type of the @p function type @p Fn.
///
/// @tparam Args the set of arugments of function @p Fn.
template<typename Fn, typename RetType, typename... Args>
class simple_task : public task
{
  std::function<RetType(Args...)> fn_;
  RetType ret_val_;
  std::tuple<Args...> args_;

  simple_task() = delete;

public:

  simple_task(Fn&& fn, Args... args)
    : fn_(fn), args_(args...)
  {
  }

  RetType
  get_return_value()
  {
    return ret_val_;
  }

  virtual void
  perform()
  {
   ret_val_ = abg_compat::apply(fn_, args_);
  }
}; // end class simple_task.

/// This is a specialization of the @ref simple_task class template
/// for which the function to be performed returns a void type.
///
/// It represents the template of a task to be performed.
///
/// The function to be performed by the task with a void return type
/// and its list of parameters are parameters of the template.
///
/// @tparam Fn the function to be executed by the instantiation.
///
/// @tparam Args the set of arugments of function @p Fn.
template<typename Fn, typename... Args>
class simple_task<Fn, void, Args...> : public task
{
  std::function<void(Args...)> fn_;
  std::tuple<Args...> args_;

  simple_task() = delete;

public:

  simple_task(Fn&& fn, Args... args)
    : fn_(fn), args_(args...)
  {
  }

  virtual void
  perform()
  {
   abg_compat::apply(fn_, args_);
  }
}; // end class simple_task.

/// A type alias for shared_ptr<simple_task<Fn, RetType, Args...>>.
template<typename Fn, typename RetType, typename... Args>
using simple_task_sptr = shared_ptr<simple_task<Fn, RetType, Args...>>;

/// This represents a queue of tasks to be performed.
///
/// Tasks are performed by a number of worker threads.
///
/// When a task is inserted into a @ref queue, the task is said to be
/// "scheduled for execution".
///
/// This is because there are worker threads waiting for tasks to be
/// added to the queue.  When a task is added to the queue, a worker
/// thread picks it up, executes it, notifies interested listeners
/// when the @ref task's execution is completed, and waits for another
/// task to be added to the queue.
///
/// Of course, several worker threads can execute tasks concurrently.
class queue
{
public:
  struct priv;

  /// A convenience typedef for a vector of @ref task_sptr
  typedef std::vector<task_sptr> tasks_type;

private:
  std::unique_ptr<priv> p_;

public:
  struct task_done_notify;
  queue();
  queue(unsigned number_of_workers);
  queue(unsigned number_of_workers,
	task_done_notify& notifier);
  size_t get_size() const;
  bool schedule_task(const task_sptr&);
  bool schedule_tasks(const tasks_type&);
  bool stage_task(const task_sptr&);
  void schedule_staged_tasks();
  void wait_for_workers_to_complete();
  tasks_type& get_completed_tasks() const;
  ~queue();
}; // end class queue

/// This functor is to notify listeners that a given task scheduled
/// for execution has been fully executed.
struct queue::task_done_notify
{
  virtual void
  operator()(const task_sptr& task_done);
};
} // end namespace workers
} // end namespace abigail
#endif // __ABG_WORKERS_H__
