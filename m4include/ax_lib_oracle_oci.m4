dnl @synopsis AX_LIB_ORACLE_OCI([MINIMUM-VERSION])
dnl
dnl This macro provides tests of availability of Oracle OCI API of
dnl particular version or newer. This macros checks for Oracle OCI
dnl headers and libraries and defines compilation flags
dnl
dnl Macro supports following options and their values:
dnl
dnl 1) Single-option usage: --with-oracle - path to ORACLE_HOME
dnl directory
dnl
dnl 2) Two-options usage (both options are required):
dnl --with-oracle-include - path to directory with OCI headers
dnl --with-oracle-lib - path to directory with OCI libraries
dnl
dnl This macro calls:
dnl
dnl   AC_SUBST(ORACLE_OCI_CFLAGS)
dnl   AC_SUBST(ORACLE_OCI_LDFLAGS)
dnl   AC_SUBST(ORACLE_OCI_LIBS)
dnl   AC_SUBST(ORACLE_OCI_VERSION)
dnl
dnl And sets:
dnl
dnl   HAVE_ORACLE
dnl
dnl @category InstalledPackages
dnl @category Cxx
dnl @author Mateusz Loskot <mateusz@loskot.net>
dnl @version 2006-07-17
dnl @license AllPermissive

AC_DEFUN([AX_LIB_ORACLE_OCI],
[
    HAVE_ORACLE="no"
    TEST_ORACLE="no"
    AC_ARG_WITH([oracle],
        AC_HELP_STRING([--with-oracle=@<:@DIR@:>@],
            [use Oracle OCI API from given path to Oracle home directory]
        ),
        [oracle_home_dir="$withval";HAVE_ORACLE="yes"],
        [oracle_home_dir=""]
    )

    AC_ARG_WITH([oracle-include],
        AC_HELP_STRING([--with-oracle-include=@<:@DIR@:>@],
            [use Oracle OCI API headers from given path]
        ),
        [oracle_home_include_dir="$withval";HAVE_ORACLE="yes"],
        [oracle_home_include_dir=""]
    )
    AC_ARG_WITH([oracle-lib],
        AC_HELP_STRING([--with-oracle-lib=@<:@DIR@:>@],
            [use Oracle OCI API libraries from given path]
        ),
        [oracle_home_lib_dir="$withval";HAVE_ORACLE="yes"],
        [oracle_home_lib_dir=""]
    )

    ORACLE_OCI_CFLAGS=""
    ORACLE_OCI_LDFLAGS=""
    ORACLE_OCI_LIBS=""
    ORACLE_OCI_VERSION=""
    ORACLE_ENV_SH="/dev/null"

    dnl
    dnl Collect include/lib paths
    dnl
    want_oracle_but_no_path="no"

    if test -n "$oracle_home_dir"; then

        if test "$oracle_home_dir" != "no" -a "$oracle_home_dir" != "yes"; then
            dnl ORACLE_HOME path provided
            oracle_include_dir="$oracle_home_dir/rdbms/public"
            oracle_lib_dir="$oracle_home_dir/lib"
        elif test "$oracle_home_dir" = "yes"; then
            want_oracle_but_no_path="yes"
        fi

    elif test -n "$oracle_home_include_dir" -o -n "$oracle_home_lib_dir"; then

        if test "$oracle_home_include_dir" != "no" -a "$oracle_home_include_dir" != "yes"; then
            oracle_include_dir="$oracle_home_include_dir"
        elif test "$oracle_home_include_dir" = "yes"; then
            want_oracle_but_no_path="yes"
        fi

        if test "$oracle_home_lib_dir" != "no" -a "$oracle_home_lib_dir" != "yes"; then
            oracle_lib_dir="$oracle_home_lib_dir"
        elif test "$oracle_home_lib_dir" = "yes"; then
            want_oracle_but_no_path="yes"
        fi
    fi

    if test "$want_oracle_but_no_path" = "yes"; then
        AC_MSG_WARN([Oracle support is requested but no Oracle paths have been provided. \
Please, locate Oracle directories using --with-oracle or \
--with-oracle-include and --with-oracle-lib options.])
    fi

    dnl
    dnl Check OCI files
    dnl
    if test -n "$oracle_include_dir" -a -n "$oracle_lib_dir"; then

        saved_CPPFLAGS="$CPPFLAGS"
        CPPFLAGS="$CPPFLAGS -I$oracle_include_dir"

	dnl establish as libnnzXX is named for this Oracle version 
	tmp0=$(find $oracle_lib_dir -name 'libnnz*')
	tmp1=$(basename $tmp0)
	tmp2=${tmp1#lib}
	ORACLE_OCI_NNZ=${tmp2%.*}
        saved_LDFLAGS="$LDFLAGS"
        oci_ldflags="-Wl,--no-as-needed -Wl,-rpath -Wl,$oracle_lib_dir -L$oracle_lib_dir"
        LDFLAGS="$LDFLAGS $oci_ldflags"
	saved_LIBS="$LIBS"
        oci_libs="-lclntsh -l$ORACLE_OCI_NNZ"
	LIBS="$LIBS $oci_libs"

        dnl
        dnl Check OCI headers
        dnl
        AC_MSG_CHECKING([for Oracle OCI headers in $oracle_include_dir])

        AC_LANG_PUSH([C])
        AC_COMPILE_IFELSE([
            AC_LANG_PROGRAM([[@%:@include <oci.h>]],
                [[
#if defined(OCI_MAJOR_VERSION) && defined(OCI_MINOR_VERSION)
// Everything is okay
#else
#  error Oracle oci.h header not found
#endif
                ]]
            )],
            [
            ORACLE_OCI_CFLAGS="-I$oracle_include_dir"
            oci_header_found="yes"
            AC_MSG_RESULT([yes])
            ],
            [
            oci_header_found="no"
            AC_MSG_RESULT([not found])
            ]
        )
        AC_LANG_POP([C])

        dnl
        dnl Check OCI libraries
        dnl
        if test "$oci_header_found" = "yes"; then

            AC_MSG_CHECKING([for Oracle OCI libraries in $oracle_lib_dir])

            AC_LANG_PUSH([C])
            AC_LINK_IFELSE([
                AC_LANG_PROGRAM([[@%:@include <oci.h>]],
                    [[
OCIEnv* envh = 0;
OCIEnvCreate(&envh, OCI_DEFAULT, 0, 0, 0, 0, 0, 0);
if (envh) OCIHandleFree(envh, OCI_HTYPE_ENV);
                    ]]
                )],
                [
                ORACLE_OCI_LDFLAGS="$oci_ldflags"
		ORACLE_OCI_LIBS="$oci_libs"
                oci_lib_found="yes"
                AC_MSG_RESULT([yes])
                ],
                [
                oci_lib_found="no"
                AC_MSG_RESULT([not found])
                ]
            )
            AC_LANG_POP([C])
        fi

        CPPFLAGS="$saved_CPPFLAGS"
        LDFLAGS="$saved_LDFLAGS"
	LIBS="$saved_LIBS"
    fi

    dnl
    dnl Check required version of Oracle is available
    dnl

    oracle_version_req=ifelse([$1], [], [], [$1])

    if test "$oci_header_found" = "yes" -a \
            "$oci_lib_found" = "yes" -a \
            -n "$oracle_version_req"; then

        AC_MSG_CHECKING([if Oracle OCI version is >= $oracle_version_req])

        dnl Decompose required version string of Oracle
        dnl and calculate its number representation
        oracle_version_req_major=`expr $oracle_version_req : '\([[0-9]]*\)'`
        oracle_version_req_minor=`expr $oracle_version_req : '[[0-9]]*\.\([[0-9]]*\)'`

        oracle_version_req_number=`expr $oracle_version_req_major \* 1000000 \
                                   \+ $oracle_version_req_minor \* 1000`

        dnl Decompose version string of installed Oracle
        dnl and calculate its number representation
        oracle_version_major=`cat $oracle_include_dir/oci.h \
                             | grep '#define.*OCI_MAJOR_VERSION.*' \
                             | sed -e 's/#define OCI_MAJOR_VERSION  *//' \
                             | sed -e 's/  *\/\*.*\*\///'`

        oracle_version_minor=`cat $oracle_include_dir/oci.h \
                             | grep '#define.*OCI_MINOR_VERSION.*' \
                             | sed -e 's/#define OCI_MINOR_VERSION  *//' \
                             | sed -e 's/  *\/\*.*\*\///'`

        oracle_version_number=`expr $oracle_version_major \* 1000000 \
                              \+ $oracle_version_minor \* 1000`

        oracle_version_check=`expr $oracle_version_number \>\= $oracle_version_req_number`
        if test "$oracle_version_check" = "1"; then
            AC_MSG_RESULT([yes])
        else
            AC_MSG_RESULT([no])
        fi

        ORACLE_OCI_VERSION="$oracle_version_major.$oracle_version_minor"
    fi

    dnl set oracle_env.sh absolute position
    if test -f $oracle_home_dir/bin/oracle_env.sh
    then
	ORACLE_ENV_SH=$oracle_home_dir/bin/oracle_env.sh
        TEST_ORACLE="yes"
    fi

    dnl search for Pro*C and Pro*COBOL precompiler in current path
    if test "$HAVE_ORACLE" = "yes"
    then
	dnl Pro*C precompiler
	AC_CHECK_PROGS(PROC, [proc], [false])
	if test "$PROC" != "false"
	then
	    AC_CHECK_PROG(HAVE_PROC, [$PROC], [yes], [no])
	else
            AC_MSG_NOTICE([Pro*C precompiler [proc] not found, maybe not in PATH or not installed])
	fi
	dnl Pro*COBOL precompiler
	AC_CHECK_PROGS(PROCOB, [procob], [false])
	if test "$PROCOB" != "false"
	then
	    AC_CHECK_PROG(HAVE_PROCOB, [$PROCOB], [yes], [no])
	else
            AC_MSG_NOTICE([Pro*COBOL precompiler [procob] not found, maybe not in PATH or not installed])
	fi
    fi

    AM_CONDITIONAL([COND_PROC], [test "$HAVE_PROC" = "YES"])
    AM_CONDITIONAL([COND_PROCOB], [test "$HAVE_PROC" = "YES"])
    AC_SUBST([ORACLE_OCI_VERSION])
    AC_SUBST([ORACLE_OCI_CFLAGS])
    AC_SUBST([ORACLE_OCI_LDFLAGS])
    AC_SUBST([ORACLE_OCI_LIBS])
    AC_SUBST([ORACLE_OCI_NNZ])
    AC_SUBST([ORACLE_ENV_SH])
    AC_SUBST([HAVE_ORACLE])
    AC_SUBST([TEST_ORACLE])
    if test "$HAVE_ORACLE" = "yes"
    then
        AC_DEFINE([HAVE_ORACLE], [1], [Define to 1 if you are using Oracle])
    fi
])
