dnl @synopsis AX_LIB_MYSQL
dnl
dnl Copyright (c) 2009-2011, Christian Ferrari <tiian@users.sourceforge.net>
dnl All rights reserved.
dnl
dnl This file is part of LIXA.
dnl
dnl LIXA is free software: you can redistribute it and/or modify
dnl it under the terms of the GNU General Public License version 2 as published
dnl by the Free Software Foundation.
dnl
dnl LIXA is distributed in the hope that it will be useful,
dnl but WITHOUT ANY WARRANTY; without even the implied warranty of
dnl MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
dnl GNU General Public License for more details.
dnl
dnl You should have received a copy of the GNU General Public License
dnl along with LIXA.  If not, see <http://www.gnu.org/licenses/>.
dnl
dnl
dnl This macro provides tests of availability of MYSQL API.
dnl This macro uses "mysql_config" utility program to retrieve MYSQL
dnl necessary headers and libraries and define compilation flags
dnl
dnl Macro supports following options and their values:
dnl
dnl 1) Single-option usage: --with-mysql
dnl
dnl This macro calls:
dnl
dnl   AC_SUBST(MYSQL_CPPFLAGS)
dnl   AC_SUBST(MYSQL_LDFLAGS)
dnl
dnl And sets:
dnl
dnl   HAVE_MYSQL
dnl

AC_DEFUN([AX_LIB_MYSQL],
[
    HAVE_MYSQL="no"
    AC_ARG_WITH([mysql],
        AC_HELP_STRING([--with-mysql], [use MySQL API]), 
           [HAVE_MYSQL="maybe"], [HAVE_MYSQL="no"]
    )
    if test "$HAVE_MYSQL" != "no"
    then
        AC_CHECK_PROGS(MYSQL_CONFIG, [mysql_config], [])
        if test -z $MYSQL_CONFIG
        then
            AC_MSG_ERROR([cannot find mysql_config program])
        else
            MYSQL_CPPFLAGS=$($MYSQL_CONFIG --include)
            MYSQL_LDFLAGS=$($MYSQL_CONFIG --libs_r)
            HAVE_MYSQL="yes"
        fi
    fi

    AC_SUBST([MYSQL_CPPFLAGS])
    AC_SUBST([MYSQL_LDFLAGS])
    AC_SUBST([HAVE_MYSQL])
])
