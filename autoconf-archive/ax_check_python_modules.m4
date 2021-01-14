# -*- Autoconf -*-
#
# ax_check_python_modules.m4 - Macros to locate python modules.
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
#
# As a special exception to the GNU General Public License, if you
# distribute this file as part of a program that contains a
# configuration script generated by Autoconf, you may include it under
# the same distribution terms that you use for the rest of that program.
#
# Author: Dodji Seketeli <dodji@seketeli.org>
#

#--------------------------------------------------------------------------------
#
# SYNOPSIS
#
#   AX_CHECK_PYTHON_MODULE(MODNAME,
#			   PYTHON,
#                          ACTION-IF-FOUND,
#			   ACTION-IF-NOT-FOUND)
#
# DESCRIPTION
#
#   Check that a python module is present on the system.
#
#   MODNAME is the name of the python module to check for.
#
#   PYTHON is either python2 or python3.  It's the python interpreter
#   to use.  By default, this is python3.
#
#   If the module MODNAME is found, the shell variable
#   HAVE_PYMOD_MODNAME is set to 'yes' and ACTION-IF_FOUND is
#   evaluated.  Otherwise the shell variable HAVE_PYMOD_MODNAME is set
#   to 'no' and ACTION-IF-NOT-FOUND is evaluated.
#
#   Note that this macro was inspired from the ax_python_module.m4
#   at
#   http://www.gnu.org/software/autoconf-archive/ax_python_module.html.
#
#----------------------------------------------------------------------------------
AU_ALIAS([AC_CHECK_PYTHON_MODULE], [AX_CHECK_PYTHON_MODULE])
AC_DEFUN([AX_CHECK_PYTHON_MODULE],[
  if test -z $PYTHON; then
    if test -z "$2"; then
      PYTHON="python3"
	else
	  PYTHON="$2"
	fi
      fi
    PYTHON_NAME=`basename $PYTHON`
    AC_MSG_CHECKING($PYTHON_NAME module: $1)
    $PYTHON -c "import $1" 2>/dev/null
    if test $? -eq 0; then
      AC_MSG_RESULT(yes)
      eval AS_TR_CPP(HAVE_PYMOD_$1)=yes
      $3
      #
    else
      AC_MSG_RESULT(no)
      eval AS_TR_CPP(HAVE_PYMOD_$1)=no
      $4
      #
  fi
])

#--------------------------------------------------------------------------------
#
# SYNOPSIS
#
#   AX_CHECK_PYTHON_MODULES(MODLIST,
#                           PYTHON,
#                           ACTION-IF-FOUND,
#                           ACTION-IF-NOT-FOUND)
#
# DESCRIPTION
#
#   Checks that a set of Python modules are present on the system.
#
#   MODLIST is a white space separated list of python modules to check
#   for.
#
#   PYTHON is either python2 or python3.  It's the name of the python
#   interpreter to use to perform the checking.  By default, uses
#   python3.
#
#   If there is a module from MODLIST that is not found the execution
#   of the test stops and ACTION-IF-NOT-FOUND is evaluated.
#   Otherwise, if all modules are found, ACTION-IF-FOUND is evaluated.
#
#--------------------------------------------------------------------------------
AU_ALIAS([AC_CHECK_PYTHON_MODULES], [AX_CHECK_PYTHON_MODULES])
AC_DEFUN([AX_CHECK_PYTHON_MODULES], [
  ax_python_modules_are_ok__=yes
  for m in $1; do
    AX_CHECK_PYTHON_MODULE([$m],
			   $2,
			   [ax_python_module_FOUND__=yes],
			   [ax_python_module_FOUND__=no])
    if test x$ax_python_module_FOUND__ = xno; then
      MISSING_PYTHON_MODULES="$MISSING_PYTHON_MODULES $m"
      ax_python_modules_are_ok__=no
    fi
  done

  if test x$ax_python_modules_are_ok__ = xyes; then
    $3
    #
  else
    $4
    #
  fi
])
