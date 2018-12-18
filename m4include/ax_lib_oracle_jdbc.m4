dnl @synopsis AX_LIB_ORACLE_JDBC
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
dnl This macro provides tests of availability of Oracle JDBC driver.
dnl
dnl Macro supports following options and their values:
dnl
dnl 1) Single-option usage: --with-oracle-jdbc
dnl
dnl This macro calls:
dnl
dnl   AC_SUBST(ORACLE_JDBC_JAR)
dnl

AC_DEFUN([AX_LIB_ORACLE_JDBC],
[
    ORACLE_JDBC_JAR=""
    AC_ARG_WITH([oracle-jdbc],
        AC_HELP_STRING([--with-oracle-jdbc@<:@=FILE@:>@], [specifies JAR file for Oracle Database JDBC driver]), 
           [HAVE_ORACLE_JDBC="maybe"], [HAVE_ORACLE_JDBC="no"]
    )
    if test "$HAVE_ORACLE_JDBC" != "no"
    then
        if test "$withval" != "yes"
        then
            ORACLE_JDBC_JAR=$withval
        fi
        if ! test -f $ORACLE_JDBC_JAR
        then
            AC_MSG_ERROR([cannot find file $ORACLE_JDBC_JAR])
        fi
    fi
    AC_SUBST(ORACLE_JDBC_JAR)
])
