dnl @synopsis AX_LIB_WEBSPHEREMQ
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
dnl This macro provides tests of availability of DB2 CLI API.
dnl This macro checks for DB2 CLI headers and libraries and defines 
dnl compilation flags
dnl
dnl Macro supports following options and their values:
dnl
dnl 1) Single-option usage: --enable-wsmq
dnl    Build switch files for WebSphereMQ (server library) installed at
dnl    default path (/opt/mqm)
dnl
dnl 2) Single-option usage: --enable-wsmq=ETC
dnl    Build switch files for WebSphereMQ (extended transactional client
dnl    library) installed at default path (/opt/mqm)
dnl
dnl 3) Two-options usage: 
dnl    --enable-wsmq
dnl    --with-wsmq=/path/to/wsmq
dnl    Build switch files for WebSphereMQ (server library) installed at
dnl    path /path/to/wsmq
dnl
dnl 4) Two-options usage: 
dnl    --enable-wsmq=ETC
dnl    --with-wsmq=/path/to/wsmq
dnl    Build switch files for WebSphereMQ (extended transactional client
dnl    library) installed at path /path/to/wsmq
dnl
dnl This macro calls:
dnl
dnl   AC_SUBST(WEBSPHEREMQ_CFLAGS)
dnl   AC_SUBST(WEBSPHEREMQ_LDFLAGS)
dnl   AC_SUBST(WEBSPHEREMQ_SWITCH_TYPE)
dnl
dnl And sets:
dnl
dnl   HAVE_WEBSPHEREMQ
dnl

AC_DEFUN([AX_LIB_WEBSPHEREMQ],
[
    HAVE_WEBSPHEREMQ="no"
    AC_ARG_WITH([wsmq],
        AC_HELP_STRING([--with-wsmq=DIR],
            [use WebSphereMQ from given path]
        ),
        [wsmq_path="$withval";HAVE_WEBSPHEREMQ="yes"],
        [wsmq_path=""]
    )
    AC_ARG_ENABLE([wsmq],
        AC_HELP_STRING([--enable-wsmq@<:@=ETC@:>@],
            [use WebSphereMQ Extended Transactional Client]
        ),
        [wsmq_type="$enableval";HAVE_WEBSPHEREMQ="yes"],
        [wsmq_type=""]
    )
    WEBSPHEREMQ_CPPFLAGS=""
    WEBSPHEREMQ_LDFLAGS=""


    if test "$HAVE_WEBSPHEREMQ" = "yes"
    then
        if test -z "$wsmq_path"
        then
            AC_MSG_ERROR([WebSphere MQ support is requested but no WebSphere MQ path have been provided. Please locate WebSphere MQ directory using --with-wsmq option.])
        fi
        dnl
        dnl host dependent values
        dnl
        case $host in
          i?86-*-*-*) 
            WSMQ_LIB_NAME="lib"
            ;;
          x86_64-*-*-*)
            WSMQ_LIB_NAME="lib64"
            ;;
          *) AC_MSG_ERROR([LIXA is not yet ported on this architecture for WebSphere MQ])
            ;;
        esac
        dnl
        dnl compute long int size for this architecture
        dnl
        AC_COMPUTE_INT([long_size], [sizeof(long int)])
        dnl 
        dnl prepare intermediate variables
        dnl
        wsmq_include_dir=$wsmq_path/inc
        wsmq_include_dir2=$wsmq_path/samp/xatm
        wsmq_lib_dir=$wsmq_path/$WSMQ_LIB_NAME
        if test "$wsmq_type" = "ETC"
        then
            WEBSPHEREMQ_SWITCH_TYPE="$wsmq_type"
            if test "$long_size" = "4"
            then
                wsmq_lib="mqcxa_r"
            else
                wsmq_lib="mqcxa64_r"
            fi
        else
            WEBSPHEREMQ_SWITCH_TYPE="SRV"
            if test "$long_size" = "4"
            then
                wsmq_lib="mqmxa_r"
            else
                wsmq_lib="mqmxa64_r"
            fi
        fi
        if test ! -d $wsmq_include_dir
        then
            AC_MSG_ERROR([$wsmq_include_dir is not a valid dir])
        fi
        if test ! -d $wsmq_include_dir2
        then
            AC_MSG_ERROR([$wsmq_include_dir2 is not a valid dir])
        fi
        if test ! -d $wsmq_lib_dir
        then
            AC_MSG_ERROR([$wsmq_lib_dir is not a valid dir])
        fi
        dnl
        dnl Check WebSphereMQ files
        dnl
        saved_CPPFLAGS="$CPPFLAGS"
        CPPFLAGS="$CPPFLAGS -I$wsmq_include_dir -I$wsmq_include_dir2"

        saved_LDFLAGS="$LDFLAGS"
        wsmq_ldflags="-Wl,-rpath=$wsmq_lib_dir -L$wsmq_lib_dir -l$wsmq_lib"
        LDFLAGS="$LDFLAGS $wsmq_ldflags"

        dnl
        dnl Check WebSphere MQ (xa.h and cmqc.h) headers
        dnl
        AC_MSG_CHECKING([for WebSphereMQ headers in $wsmq_include_dir])

        AC_LANG_PUSH([C])
        AC_COMPILE_IFELSE([
            AC_LANG_PROGRAM([[@%:@include <xa.h>]],
                [[ ]]
            )],
            [
            WEBSPHEREMQ_CPPFLAGS="-I$wsmq_include_dir"
            wsmq_header_found="yes"
            AC_MSG_RESULT([yes])
            ],
            [
            wsmq_header_found="no"
            AC_MSG_RESULT([not found])
            ]
        )
        AC_LANG_POP([C])

        dnl
        dnl Check WebSphere MQ libraries for XA switch file
        dnl
        if test "$wsmq_header_found" = "yes"; then

            AC_MSG_CHECKING([for WebSphereMQ libraries in $wsmq_lib_dir])

            AC_LANG_PUSH([C])
            AC_LINK_IFELSE([
                AC_LANG_PROGRAM(
                    [[
@%:@include <xa.h>
extern struct xa_switch_t MQRMIXASwitchDynamic;
extern struct xa_switch_t MQRMIXASwitch;
                    ]],
                    [[
struct xa_switch_t dyn = MQRMIXASwitchDynamic;
struct xa_switch_t sta = MQRMIXASwitch;
                    ]]
                )],
                [
                WEBSPHEREMQ_LDFLAGS="$wsmq_ldflags"
                wsmq_lib_found="yes"
                AC_MSG_RESULT([yes])
                ],
                [
                wsmq_lib_found="no"
                AC_MSG_RESULT([not found])
                ]
            )
            AC_LANG_POP([C])
        fi
        CPPFLAGS="$saved_CPPFLAGS"
        LDFLAGS="$saved_LDFLAGS"
    fi

    AC_SUBST([WEBSPHEREMQ_CPPFLAGS])
    AC_SUBST([WEBSPHEREMQ_LDFLAGS])
    AC_SUBST([WEBSPHEREMQ_SWITCH_TYPE])
    AC_SUBST([HAVE_WEBSPHEREMQ])
])
