dnl @synopsis AX_LIB_POSTGRESQL
dnl
dnl Copyright (c) 2009-2012, Christian Ferrari <tiian@users.sourceforge.net>
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
dnl This macro checks for POSTGRESQL headers and libraries and defines 
dnl compilation flags
dnl
dnl Macro supports following options and their values:
dnl
dnl 1) Single-option usage: --with-postgresql - path to PostgreSQL installation directory
dnl
dnl 2) Two-options usage (both options are required):
dnl       --with-postgresql-include - path to directory with PostgreSQL headers
dnl       --with-postgresql-lib - path to directory with PostgreSQL libraries
dnl
dnl This macro calls:
dnl
dnl   AC_SUBST(POSTGRESQL_CPPFLAGS)
dnl   AC_SUBST(POSTGRESQL_LDFLAGS)
dnl
dnl And sets:
dnl
dnl   HAVE_POSTGRESQL
dnl

AC_DEFUN([AX_LIB_POSTGRESQL],
[
    HAVE_POSTGRESQL="0"
    AC_ARG_WITH([postgresql],
        AC_HELP_STRING([--with-postgresql=@<:@DIR@:>@],
            [use PostgreSQL API from given path]
        ),
        [postgresql_home_dir="$withval";HAVE_POSTGRESQL="1"],
        [postgresql_home_dir=""]
    )

    AC_ARG_WITH([postgresql-include],
        AC_HELP_STRING([--with-postgresql-include=@<:@DIR@:>@],
            [use PostgreSQL API headers from given path]
        ),
        [postgresql_home_include_dir="$withval";HAVE_POSTGRESQL="1"],
        [postgresql_home_include_dir=""]
    )
    AC_ARG_WITH([postgresql-lib],
        AC_HELP_STRING([--with-postgresql-lib=@<:@DIR@:>@],
            [use PostgreSQL API libraries from given path]
        ),
        [postgresql_home_lib_dir="$withval";HAVE_POSTGRESQL="1"],
        [postgresql_home_lib_dir=""]
    )

    POSTGRESQL_CPPFLAGS=""
    POSTGRESQL_LDFLAGS=""

    dnl
    dnl host dependent values
    dnl
    case $host in
      i?86-*-*-*) 
        POSTGRESQL_LIB_NAME="lib32"
	;;
      x86_64-*-*-*)
        POSTGRESQL_LIB_NAME="lib64"
	;;
      *) AC_MSG_ERROR([LIXA is not yet ported on this architecture for PostgreSQL])
        ;;
    esac

    dnl
    dnl Collect include/lib paths
    dnl
    want_postgresql_but_no_path="no"

    if test -n "$postgresql_home_dir"; then

        if test "$postgresql_home_dir" != "no" -a "$postgresql_home_dir" != "yes"; then
            dnl POSTGRESQL_HOME path provided
            postgresql_include_dir="$postgresql_home_dir/include"
            postgresql_lib_dir="$postgresql_home_dir/$POSTGRESQL_LIB_NAME"
        elif test "$postgresql_home_dir" = "yes"; then
            want_postgresql_but_no_path="yes"
        fi

    elif test -n "$postgresql_home_include_dir" -o -n "$postgresql_home_lib_dir"; then

        if test "$postgresql_home_include_dir" != "no" -a "$postgresql_home_include_dir" != "yes"; then
            postgresql_include_dir="$postgresql_home_include_dir"
        elif test "$postgresql_home_include_dir" = "yes"; then
            want_postgresql_but_no_path="yes"
        fi

        if test "$postgresql_home_lib_dir" != "no" -a "$postgresql_home_lib_dir" != "yes"; then
            postgresql_lib_dir="$postgresql_home_lib_dir"
        elif test "$postgresql_home_lib_dir" = "yes"; then
            want_postgresql_but_no_path="yes"
        fi
    fi

    if test "$want_postgresql_but_no_path" = "yes"; then
        AC_MSG_WARN([POSTGRESQL support is requested but no POSTGRESQL paths have been provided. \
Please, locate POSTGRESQL directories using --with-postgresql or \
--with-postgresql-include and --with-postgresql-lib options.])
    fi

    dnl
    dnl Check files
    dnl
    if test -n "$postgresql_include_dir" -a -n "$postgresql_lib_dir"; then

        saved_CPPFLAGS="$CPPFLAGS"
        CPPFLAGS="$CPPFLAGS -I$postgresql_include_dir"

        saved_LDFLAGS="$LDFLAGS"
        pql_ldflags="-Wl,-rpath,$postgresql_lib_dir -L$postgresql_lib_dir -lpq"
        LDFLAGS="$LDFLAGS $pql_ldflags"

        dnl
        dnl Check headers
        dnl
        AC_MSG_CHECKING([for Postgresql headers in $postgresql_include_dir])

        AC_LANG_PUSH([C])
        AC_COMPILE_IFELSE([
            AC_LANG_PROGRAM([[@%:@include <libpq-fe.h>]],
                [[ ]]
            )],
            [
            POSTGRESQL_CPPFLAGS="-I$postgresql_include_dir"
            pql_header_found="yes"
            AC_MSG_RESULT([yes])
            ],
            [
            pql_header_found="no"
            AC_MSG_RESULT([not found])
            ]
        )
        AC_LANG_POP([C])

        dnl
        dnl Check libraries
        dnl
        if test "$pql_header_found" = "yes"; then

            AC_MSG_CHECKING([for Postgresql libraries in $postgresql_lib_dir])

            AC_LANG_PUSH([C])
            AC_LINK_IFELSE([
                AC_LANG_PROGRAM([[@%:@include <libpq-fe.h>]],
                    [[
PGconn *conn;
conn = PQconnectdb("");
return 0;
                    ]]
                )],
                [
                POSTGRESQL_LDFLAGS="$pql_ldflags"
                pql_lib_found="yes"
                AC_MSG_RESULT([yes])
                ],
                [
                pql_lib_found="no"
                AC_MSG_RESULT([not found])
                ]
            )
            AC_LANG_POP([C])
        fi

        CPPFLAGS="$saved_CPPFLAGS"
        LDFLAGS="$saved_LDFLAGS"
        POSTGRESQL_INCLUDE_DIR="$postgresql_include_dir"
    fi

    AC_SUBST([POSTGRESQL_CPPFLAGS])
    AC_SUBST([POSTGRESQL_LDFLAGS])
    AC_SUBST([POSTGRESQL_INCLUDE_DIR])
    if test "$HAVE_POSTGRESQL" = "1"
    then
	AC_DEFINE([HAVE_POSTGRESQL], [1], 
    	[Define to 1 if you are using PostgreSQL])
    fi
])
