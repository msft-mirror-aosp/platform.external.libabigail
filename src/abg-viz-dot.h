// Copyright (C) 2013 Free Software Foundation, Inc.
//
// This file is part of the GNU Application Binary Interface Generic
// Analysis and Instrumentation Library (libabigail).  This library is
// free software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the Free
// Software Foundation; either version 3, or (at your option) any
// later version.

// This library is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// General Public License for more details.

// Under Section 7 of GPL version 3, you are granted additional
// permissions described in the GCC Runtime Library Exception, version
// 3.1, as published by the Free Software Foundation.

// You should have received a copy of the GNU General Public License
// and a copy of the GCC Runtime Library Exception along with this
// program; see the files COPYING3 and COPYING.RUNTIME respectively.
// If not, see <http://www.gnu.org/licenses/>.

// -*- mode: C++ -*-
/// @file

#ifndef __ABG_VIZ_DOT_H__
#define __ABG_VIZ_DOT_H__

#include <abg-viz-common.h>

namespace abigail
{

/// Base class for graph nodes.
struct node_base
{ };

/*
  Parent node.

  Some characteristics:
  - horizontal name (text anchor = start ie left).
  - background box
  - (optional) template parameters

 */
struct parent
{
  std::string		_M_id;
  const style&		_M_style;

  //units_type		_M_size;
};

/// Useful parent constants.
extern const style parent_sty;


/*
  Child node.

  Some characteristics:
  - horizontal name (text anchor = start ie left).
  - background box
  - (optional) template parameters

 */
struct child
{
  std::string		_M_id;
  const style&		_M_style;

  //units_type		_M_size;
};

/// Useful child constants.
extern const style child_sty;


/*
  DOT "graph" style notation for class inheritance.

  This is a compact DOT representation of class inheritance.

  It is composed of a minimum of three data points for each member or
  base of a class:

  - parent classes
  - child classes
  - name

  Including typographic information to compute line length, and C++
  niceities like grouping and overload sets.

  It's constructed by creating a digraph, starting from the base node.
 */
struct dot
{

private:

  const std::string    	_M_title;
  const canvas&	       	_M_canvas;
  const typography&    	_M_typo;	

  const units_type	_M_x_size = 3;	// Number of columns
  units_type   		_M_x_space;	// Column spacing.
  units_type   		_M_x_origin;	// X origin

  units_type   		_M_y_size;	// Number of rows
  units_type   		_M_y_space;	// Row spacing.
  units_type   		_M_y_origin;	// Y origin

  std::ostringstream   	_M_sstream;
  
  // static const units_type _M_stroke_width = 1;
  // static const units_type _M_text_padding = 10;

public:

  dot(const std::string __title, 
      const canvas& __cv = ansi_letter_canvas,
      const typography& __typo = arial_typo) 
  : _M_title(__title), _M_canvas(__cv), _M_typo(__typo), _M_y_size(0)
  { 
    // Offsets require: typo, canvas units, size.
    _M_x_space = 40;
    _M_y_space = 40;
    _M_x_origin = _M_x_space * 1;
    _M_y_origin = _M_y_space * 2;
  }
  
  // Empty when the output buffer is.
  bool
  empty() { return _M_sstream.str().empty(); }

  void 
  start_element();
  
  void 
  finish_element();
  
  void 
  add_title();

  void
  add_parent(const parent&);

  void
  add_child(const child&);

  void
  write();

  void 
  start()
  {
    this->start_element();
    this->add_title();
  }

  void 
  finish()
  {
    this->finish_element();
    this->write();
  }
};

// XXX connect external xml file to input. 
// parse input, pick apart elements, attributes.

}// end namespace abigail

#endif //__ABG_VIZ_DOT_H__
