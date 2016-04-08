dnl @synopsis AX_LIB_POSTGRESQL
dnl
dnl Copyright (c) 2009-2016, Christian Ferrari <tiian@users.sourceforge.net>
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
dnl This macro provides tests of availability of POSTGRESQL API.
dnl This macro uses "mysql_config" utility program to retrieve POSTGRESQL
dnl necessary headers and libraries and define compilation flags
dnl
dnl Macro supports following options and their values:
dnl
dnl 1) Single-option usage: --with-mysql
dnl
dnl This macro calls:
dnl
dnl   AC_SUBST(POSTGRESQL_CPPFLAGS)
dnl   AC_SUBST(POSTGRESQL_LDFLAGS)
dnl   AC_SUBST(POSTGRESQL_INCLUDE_DIR)
dnl   AC_SUBST(HAVE_POSTGRESQL)
dnl
dnl And sets:
dnl
dnl   HAVE_POSTGRESQL
dnl

AC_DEFUN([AX_LIB_POSTGRESQL],
[
    HAVE_POSTGRESQL="no"
    AC_ARG_WITH([postgresql],
        AC_HELP_STRING([--with-postgresql@<:@=FILE@:>@], [use PostgreSQL (FILE points to pg_config utility program)]), 
           [HAVE_POSTGRESQL="maybe"], [HAVE_POSTGRESQL="no"]
    )
    if test "$HAVE_POSTGRESQL" != "no"
    then
        if test "$withval" != "yes"
        then
            POSTGRESQL_CONFIG=$withval
        else
            AC_CHECK_PROGS(POSTGRESQL_CONFIG, [pg_config], [])
        fi
        if test -z $POSTGRESQL_CONFIG
        then
            AC_MSG_ERROR([cannot find pg_config program])
        else
            POSTGRESQL_CPPFLAGS="-I$($POSTGRESQL_CONFIG --includedir)"
            POSTGRESQL_LDFLAGS="-L$($POSTGRESQL_CONFIG --libdir) -lpq"
            POSTGRESQL_INCLUDE_DIR=${POSTGRESQL_CPPFLAGS##-I}
            HAVE_POSTGRESQL="yes"
        fi
    fi

    AC_SUBST([POSTGRESQL_CPPFLAGS])
    AC_SUBST([POSTGRESQL_LDFLAGS])
    AC_SUBST([POSTGRESQL_INCLUDE_DIR])
    AC_SUBST([HAVE_POSTGRESQL])
    if test "$HAVE_POSTGRESQL" = "yes"
    then
        AC_DEFINE([HAVE_POSTGRESQL], [1], [Define to 1 if you are using PostgreSQL])
    fi 
])
