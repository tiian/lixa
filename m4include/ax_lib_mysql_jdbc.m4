dnl @synopsis AX_LIB_MYSQL_JDBC
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
dnl This macro provides tests of availability of MYSQL JDBC driver.
dnl
dnl Macro supports following options and their values:
dnl
dnl 1) Single-option usage: --with-mysql-jdbc
dnl
dnl This macro calls:
dnl
dnl   AC_SUBST(MYSQL_JDBC_JAR)
dnl   AC_SUBST(MYSQL_JDBC_DATASOURCE_CLASS)
dnl

AC_DEFUN([AX_LIB_MYSQL_JDBC],
[
    MYSQL_JDBC_JAR=""
    AC_ARG_WITH([mysql-jdbc],
        AC_HELP_STRING([--with-mysql-jdbc@<:@=FILE@:>@], [specifies JAR file for MySQL/MariaDB JDBC driver]), 
           [HAVE_MYSQL_JDBC="maybe"], [HAVE_MYSQL_JDBC="no"]
    )
    if test "$HAVE_MYSQL_JDBC" != "no"
    then
        if test "$withval" != "yes"
        then
            MYSQL_JDBC_JAR=$withval
        fi
        if ! test -f $MYSQL_JDBC_JAR
        then
            AC_MSG_ERROR([cannot find file $MYSQL_JDBC_JAR])
        fi
    fi
    AC_SUBST(MYSQL_JDBC_JAR)
    # guess the right class to use for MySQL/MariaDB
    TMPFILE=$(mktemp)
    jar -tf $MYSQL_JDBC_JAR | grep DataSource | sed 's/\//./g' | sed 's/\.class$//g' > $TMPFILE
    # check for MysqlXADataSource
    MYSQL_JDBC_DATASOURCE_CLASS=$(grep MysqlXADataSource $TMPFILE)
    if test $? -ne 0
    then
        # check for MariaDbDataSource
        MYSQL_JDBC_DATASOURCE_CLASS=$(grep MariaDbDataSource $TMPFILE)
        if test $? -ne 0
        then
            AC_MSG_ERROR([cannot find a suitable XA Data Source class for MySQL/MariaDB])
        fi
    fi
    AC_SUBST(MYSQL_JDBC_DATASOURCE_CLASS)
    rm $TMPFILE
])
