dnl @synopsis AX_LIB_IBMDB2_CLI
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
dnl This macro provides tests of availability of DB2 CLI API.
dnl This macro checks for DB2 CLI headers and libraries and defines 
dnl compilation flags
dnl
dnl Macro supports following options and their values:
dnl
dnl 1) Single-option usage: --with-ibmdb2 - path to DB2 installation directory
dnl
dnl 2) Two-options usage (both options are required):
dnl       --with-ibmdb2-include - path to directory with CLI headers
dnl       --with-ibmdb2-lib - path to directory with CLI libraries
dnl
dnl This macro calls:
dnl
dnl   AC_SUBST(IBMDB2_CLI_CFLAGS)
dnl   AC_SUBST(IBMDB2_CLI_LDFLAGS)
dnl
dnl And sets:
dnl
dnl   HAVE_IBMDB2
dnl

AC_DEFUN([AX_LIB_IBMDB2_CLI],
[
    HAVE_IBMDB2="no"
    AC_ARG_WITH([ibmdb2],
        AC_HELP_STRING([--with-ibmdb2=@<:@DIR@:>@],
            [use IBM DB2 CLI API from given path to DB2 home directory]
        ),
        [ibmdb2_home_dir="$withval";HAVE_IBMDB2="yes"],
        [ibmdb2_home_dir=""]
    )

    AC_ARG_WITH([ibmdb2-include],
        AC_HELP_STRING([--with-ibmdb2-include=@<:@DIR@:>@],
            [use IBM DB2 CLI API headers from given path]
        ),
        [ibmdb2_home_include_dir="$withval";HAVE_IBMDB2="yes"],
        [ibmdb2_home_include_dir=""]
    )
    AC_ARG_WITH([ibmdb2-lib],
        AC_HELP_STRING([--with-ibmdb2-lib=@<:@DIR@:>@],
            [use IBM DB2 CLI API libraries from given path]
        ),
        [ibmdb2_home_lib_dir="$withval";HAVE_IBMDB2="yes"],
        [ibmdb2_home_lib_dir=""]
    )

    IBMDB2_CLI_CPPFLAGS=""
    IBMDB2_CLI_LDFLAGS=""

    dnl
    dnl host dependent values
    dnl
    case $host in
      i?86-*-*-*) 
        IBMDB2_LIB_NAME="lib32"
	;;
      x86_64-*-*-*)
        IBMDB2_LIB_NAME="lib64"
	;;
	  rs6000-ibm-aix)
	    IBMDB2_LIB_NAME="lib64"
	;;
      *) AC_MSG_ERROR([LIXA is not yet ported on this architecture for IBM DB2])
        ;;
    esac

    dnl
    dnl Collect include/lib paths
    dnl
    want_ibmdb2_but_no_path="no"

    if test -n "$ibmdb2_home_dir"; then

        if test "$ibmdb2_home_dir" != "no" -a "$ibmdb2_home_dir" != "yes"; then
            dnl IBMDB2_HOME path provided
            ibmdb2_include_dir="$ibmdb2_home_dir/include"
            ibmdb2_lib_dir="$ibmdb2_home_dir/$IBMDB2_LIB_NAME"
        elif test "$ibmdb2_home_dir" = "yes"; then
            want_ibmdb2_but_no_path="yes"
        fi

    elif test -n "$ibmdb2_home_include_dir" -o -n "$ibmdb2_home_lib_dir"; then

        if test "$ibmdb2_home_include_dir" != "no" -a "$ibmdb2_home_include_dir" != "yes"; then
            ibmdb2_include_dir="$ibmdb2_home_include_dir"
        elif test "$ibmdb2_home_include_dir" = "yes"; then
            want_ibmdb2_but_no_path="yes"
        fi

        if test "$ibmdb2_home_lib_dir" != "no" -a "$ibmdb2_home_lib_dir" != "yes"; then
            ibmdb2_lib_dir="$ibmdb2_home_lib_dir"
        elif test "$ibmdb2_home_lib_dir" = "yes"; then
            want_ibmdb2_but_no_path="yes"
        fi
    fi

    if test "$want_ibmdb2_but_no_path" = "yes"; then
        AC_MSG_WARN([IBMDB2 support is requested but no IBMDB2 paths have been provided. \
Please, locate IBMDB2 directories using --with-ibmdb2 or \
--with-ibmdb2-include and --with-ibmdb2-lib options.])
    fi

    dnl
    dnl Check CLI files
    dnl
    if test -n "$ibmdb2_include_dir" -a -n "$ibmdb2_lib_dir"; then

        saved_CPPFLAGS="$CPPFLAGS"
        CPPFLAGS="$CPPFLAGS -I$ibmdb2_include_dir"

        saved_LDFLAGS="$LDFLAGS"
        cli_ldflags="-Wl,-rpath,$ibmdb2_lib_dir -L$ibmdb2_lib_dir -ldb2"
        LDFLAGS="$LDFLAGS $cli_ldflags"

        dnl
        dnl Check CLI headers
        dnl
        AC_MSG_CHECKING([for Ibmdb2 CLI headers in $ibmdb2_include_dir])

        AC_LANG_PUSH([C])
        AC_COMPILE_IFELSE([
            AC_LANG_PROGRAM([[@%:@include <sqlcli1.h>]],
                [[ ]]
            )],
            [
            IBMDB2_CLI_CPPFLAGS="-I$ibmdb2_include_dir"
            cli_header_found="yes"
            AC_MSG_RESULT([yes])
            ],
            [
            cli_header_found="no"
            AC_MSG_RESULT([not found])
            ]
        )
        AC_LANG_POP([C])

        dnl
        dnl Check CLI libraries
        dnl
        if test "$cli_header_found" = "yes"; then

            AC_MSG_CHECKING([for Ibmdb2 CLI libraries in $ibmdb2_lib_dir])

            AC_LANG_PUSH([C])
            AC_LINK_IFELSE([
                AC_LANG_PROGRAM([[@%:@include <sqlcli.h>]],
                    [[
SQLHANDLE *env;
SQLRETURN cli_rc = SQL_SUCCESS;
cli_rc = SQLAllocHandle(SQL_HANDLE_ENV, SQL_NULL_HANDLE, env);
if (SQL_SUCCESS != cli_rc) return 1; else return 0;
                    ]]
                )],
                [
                IBMDB2_CLI_LDFLAGS="$cli_ldflags"
                cli_lib_found="yes"
                AC_MSG_RESULT([yes])
                ],
                [
                cli_lib_found="no"
                AC_MSG_RESULT([not found])
                ]
            )
            AC_LANG_POP([C])
        fi

        CPPFLAGS="$saved_CPPFLAGS"
        LDFLAGS="$saved_LDFLAGS"
    fi

    AC_SUBST([IBMDB2_CLI_CPPFLAGS])
    AC_SUBST([IBMDB2_CLI_LDFLAGS])
    AC_SUBST([HAVE_IBMDB2])
    if test "$HAVE_IBMDB2" = "yes"
    then
        AC_DEFINE([HAVE_IBMDB2], [1], [Define to 1 if you are using IBM DB2])
    fi
])
