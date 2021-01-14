// -*- mode: C++ -*-
//
// Copyright (C) 2013 Red Hat, Inc.
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

/// @file

#include <string>
#include <iostream>
#include "abg-libxml-utils.h"

namespace abigail
{
namespace xml
{
using std::istream;

/// Instantiate an xmlTextReader that parses the content of an on-disk
/// file, wrap it into a smart pointer and return it.
///
/// @param path the path to the file to be parsed by the returned
/// instance of xmlTextReader.
reader_sptr
new_reader_from_file(const std::string& path)
{
  reader_sptr p =
    build_sptr(xmlNewTextReaderFilename (path.c_str()));

  return p;
}

/// Instanciate an xmlTextReader that parses the content of an
/// in-memory buffer, wrap it into a smart pointer and return it.
///
/// @param buffer the in-memory buffer to be parsed by the returned
/// instance of xmlTextReader.
reader_sptr
new_reader_from_buffer(const std::string& buffer)
{
  reader_sptr p =
    build_sptr(xmlReaderForMemory(buffer.c_str(),
				  buffer.length(),
				  "", 0, 0));
  return p;
}

/// This is an xmlInputReadCallback, meant to be passed to
/// xmlNewTextReaderForIO.  It reads a number of bytes from an istream.
///
/// @param context an std::istream* cast into a void*.  This is the
/// istream that the xmlTextReader is too read data from.
///
/// @param buffer the buffer where to copy the data read from the
/// input stream.
///
/// @param len the number of byte to read from the input stream and to
/// copy into @ref buffer.
///
/// @return the number of bytes read or -1 in case of error.
static int
xml_istream_input_read(void*	context,
		       char*	buffer,
		       int	len)
{
  istream* in = reinterpret_cast<istream*>(context);
  in->read(buffer, len);
  return in->gcount();
}

/// This is an xmlInputCloseCallback, meant to be passed to
/// xmlNewTextReaderForIO.  It's supposed to close the input stream
/// that the xmlTextReader is reading from.  This particular
/// implementation is noop; it does nothing.
///
/// @return 0.
static int
xml_istream_input_close(void*)
{return 0;}

/// Instanciate an xmlTextReader that parses a content coming from an
/// input stream.
///
/// @param in the input stream to consider.
///
/// @return reader_sptr a pointer to the newly instantiated xml
/// reader.
reader_sptr
new_reader_from_istream(std::istream* in)
{
  reader_sptr p =
    build_sptr(xmlReaderForIO(&xml_istream_input_read,
			      &xml_istream_input_close,
			      in, "", 0, 0));
  return p;
}

/// Build and return a shared_ptr for a pointer to xmlTextReader
template<>
shared_ptr<xmlTextReader>
build_sptr<xmlTextReader>(xmlTextReader *p)
{
  return shared_ptr<xmlTextReader>(p, textReaderDeleter());
}

/// Build and return a shared_ptr for a pointer to xmlChar
template<>
shared_ptr<xmlChar>
build_sptr<xmlChar>(xmlChar *p)
{
  return shared_ptr<xmlChar>(p, charDeleter());
}

/// Return the depth of an xml element node.
///
/// Note that the node must be attached to an XML document.
///
/// @param n the xml to consider.
///
/// @return a positive or zero number for an XML node properly
/// attached to an xml document, -1 otherwise.  Note that the function
/// returns -1 if passed an xml document as well.
int
get_xml_node_depth(xmlNodePtr n)
{
  if (n->type == XML_DOCUMENT_NODE || n->parent == NULL)
    return -1;

  if (n->parent->type == XML_DOCUMENT_NODE)
    return 0;

  return 1 + get_xml_node_depth(n->parent);
}

/// Escape the 5 characters representing the predefined XML entities.
///
/// The resulting entities and their matching characters are:
///
///   &lt; for the character '<', &gt; for the character '>', &apos; for
///   the character ''', &quot; for the character '"', and &amp; for the
///   character '&'.
///
//// @param str the input string to read to search for the characters
//// to escape.
////
//// @param the output string where to write the resulting string that
//// contains the pre-defined characters escaped as predefined
//// entitites.
void
escape_xml_string(const std::string& str,
		  std::string& escaped)
{
  for (std::string::const_iterator i = str.begin(); i != str.end(); ++i)
    switch (*i)
      {
      case '<':
	escaped += "&lt;";
	break;
      case '>':
	escaped += "&gt;";
	break;
      case '&':
	escaped += "&amp;";
	break;
      case '\'':
	escaped += "&apos;";
	break;
      case '"':
	escaped += "&quot;";
	break;
      default:
	escaped += *i;
      }
}

/// Escape the 5 characters representing the predefined XML entities.
///
/// The resulting entities and their matching characters are:
///
///   &lt; for the character '<', &gt; for the character '>', &apos; for
///   the character ''', &quot; for the character '"', and &amp; for the
///   character '&'.
///
//// @param str the input string to read to search for the characters
//// to escape.
////
//// @return the resulting string that contains the pre-defined
//// characters escaped as predefined entitites.
std::string
escape_xml_string(const std::string& str)
{
  std::string result;
  escape_xml_string(str, result);
  return result;
}

/// Read a string, detect the 5 predefined XML entities it may contain
/// and un-escape them, by writting their corresponding characters
/// back in.  The pre-defined entities are:
///
///   &lt; for the character '<', &gt; for the character '>', &apos; for
///   the character ''', &quot; for the character '"', and &amp; for the
///   character '&'.
///
///   @param str the input XML string to consider.
///
///   @param escaped where to write the resulting un-escaped string.
void
unescape_xml_string(const std::string& str,
		  std::string& escaped)
{
  std::string::size_type i = 0;
  while (i < str.size())
    {
      if (str[i] == '&')
	{
	  if (str[i+1]    == 'l'
	      && str[i+2] == 't'
	      && str[i+3] == ';')
	    {
	      escaped += '<';
	      i+= 4;
	    }
	  else if (str[i+1]    == 'g'
		   && str[i+2] == 't'
		   && str[i+3] == ';')
	    {
	      escaped += '>';
	      i += 4;
	    }
	  else if (str[i+1]    == 'a'
		   && str[i+2] == 'm'
		   && str[i+3] == 'p'
		   && str[i+4] == ';')
	    {
	      escaped += '&';
	      i += 5;
	    }
	  else if (str[i+1]    == 'a'
		   && str[i+2] == 'p'
		   && str[i+3] == 'o'
		   && str[i+4] == 's'
		   && str[i+5] == ';')
	    {
	      escaped += '\'';
	      i += 6;
	    }
	  else if (str[i+1]    == 'q'
		   && str[i+2] == 'u'
		   && str[i+3] == 'o'
		   && str[i+4] == 't'
		   && str[i+5] == ';')
	    {
	      escaped += '"';
	      i += 6;
	    }
	  else
	    {
	      escaped += str[i];
	      ++i;
	    }
	}
      else
	{
	  escaped += str[i];
	  ++i;
	}
    }
}

/// Read a string, detect the 5 predefined XML entities it may contain
/// and un-escape them, by writting their corresponding characters
/// back in.  The pre-defined entities are:
///
///   &lt; for the character '<', &gt; for the character '>', &apos; for
///   the character ''', &quot; for the character '"', and &amp; for the
///   character '&'.
///
///   @param str the input XML string to consider.
///
///   @return escaped where to write the resulting un-escaped string.
std::string
unescape_xml_string(const std::string& str)
{
  std::string result;
  unescape_xml_string(str, result);
  return result;
}

}//end namespace xml
}//end namespace abigail
