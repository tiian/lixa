dnl @synopsis AX_DOCBOOK
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
dnl This macro provides tests of availability of xsltproc and some required
dnl XSL files.
dnl
dnl Macro supports following options and their values:
dnl
dnl 1) Single-option usage: --with-docbook
dnl
dnl This macro calls:
dnl
dnl   AC_SUBST(STYLESHEETS_DIR)
dnl

AC_DEFUN([AX_DOCBOOK],
[
    HAVE_DOCBOOK="no"
    STYLESHEETS_DIR=""
    AC_ARG_WITH([docbook],
        AC_HELP_STRING([--with-docbook@<:@=DIR@:>@], [use Docbook, DIR contains the stylesheets]), 
           [HAVE_DOCBOOK="maybe"], [HAVE_DOCBOOK="no"]
    )
    if test "$HAVE_DOCBOOK" != "no"
    then
        AC_CHECK_PROGS(XSLTPROC, [xsltproc], [])
        if test -z $XSLTPROC
        then
            AC_MSG_ERROR([cannot find xsltproc program])
        else
	    HAVE_DOCBOOK="no"
            if test "$withval" != "yes"
            then
                BASEDIR="$withval"
            else
                BASEDIR="/usr/share"
            fi
            AC_MSG_CHECKING([if $BASEDIR contains Docbook stylesheets])
            for DIR in $(find $BASEDIR -type d -name xhtml)
            do
                if test -f "$DIR/docbook.xsl"
                then
                    if test -f "$DIR/chunk.xsl"
                    then
                        HAVE_DOCBOOK="yes"
                        STYLESHEETS_DIR="$DIR"
                        break 1
                    fi
                fi
            done
            AC_MSG_RESULT([$HAVE_DOCBOOK])
        fi
    fi
    AC_SUBST([STYLESHEETS_DIR])
])
