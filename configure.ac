AC_INIT([lixa], [0.5.11], [tiian at users dot sourceforge dot net], [], [http://sourceforge.net/projects/lixa/])
AM_INIT_AUTOMAKE([-Wall -Werror])

LT_INIT
AC_PROG_LIBTOOL
AM_PROG_LIBTOOL

AC_PROG_CC
AC_CONFIG_HEADER([config.h])
AC_CONFIG_FILES([
  Makefile
  doc/Makefile
  etc/lixac_conf.xml
  etc/lixad_conf.xml
  etc/Makefile
  src/Makefile
  src/common/Makefile
  src/common/lixa_inst_conf.c
  src/client/Makefile
  src/client/switch/Makefile
  src/client/switch/lixa/Makefile
  src/client/switch/ibmdb2/Makefile
  src/client/switch/oracle/Makefile
  src/server/Makefile
  src/sample/Makefile
  src/sample/ibmdb2/Makefile
  src/sample/oracle/Makefile
  tests/Makefile
  tests/atlocal
  tests/etc/lixac_conf.xml
  tests/etc/lixad_conf.xml
  tests/etc/Makefile
  tests/src/Makefile
  var/Makefile
  doxygen.conf
])
AC_CONFIG_TESTDIR([tests])

# Installation PATH
test "x$prefix" = "xNONE" && prefix="/opt/lixa"
test "x$exec_prefix" = "xNONE" -a "x$prefix" != "xNONE" && exec_prefix=$prefix

# Include Oracle test
m4_include(m4include/ax_lib_oracle_oci.m4)
AX_LIB_ORACLE_OCI
AM_CONDITIONAL([COMPILE_ORACLE_STUFF], [test -n "$oracle_home_dir"])
m4_include(m4include/ax_lib_ibmdb2_cli.m4)
AX_LIB_IBMDB2_CLI
AM_CONDITIONAL([COMPILE_IBMDB2_STUFF], [test -n "$ibmdb2_home_dir"])

# LT Version numbers, remember to change them just *before* a release.
#   (Interfaces removed:    CURRENT++, AGE=0, REVISION=0)
#   (Interfaces added:      CURRENT++, AGE++, REVISION=0)
#   (No interfaces changed:                   REVISION++)
#
# liblixac and liblixacmt libraries version
LT_LIXAC_CURRENT=0
LT_LIXAC_AGE=0
LT_LIXAC_REVISION=0
# liblixab and liblixbcmt libraries version
LT_LIXAB_CURRENT=0
LT_LIXAB_AGE=0
LT_LIXAB_REVISION=0

# Expanded variables can be used in C code and XML files
EXPANDED_SYSCONFDIR=$(eval echo $sysconfdir)
EXPANDED_LOCALSTATEDIR=$(eval echo $localstatedir)
EXPANDED_LIBDIR=$(eval echo $libdir)

AC_PRESERVE_HELP_ORDER
AC_ARG_ENABLE([debug],
        AS_HELP_STRING([--enable-debug],
                [enable debug code (assert statements)]),
        [LIXA_DEBUG="yes" CPPFLAGS="$CPPFLAGS -DLIXA_DEBUG"; CFLAGS="-g" ], \
	[LIXA_DEBUG="no"; CFLAGS="-g -O2" ] )
AC_ARG_ENABLE([crash],
	AS_HELP_STRING([--enable-crash],
		[enable crash simulation code]),
	[LIXA_CRASH="yes"; CPPFLAGS="$CPPFLAGS -D_CRASH"], 
	[LIXA_CRASH="no"] )
AC_ARG_ENABLE([trace],
        AS_HELP_STRING([--disable-trace],
                [disable trace feature (not suggested)]),
        [_TRACE="no"], [_TRACE="yes"; CPPFLAGS="$CPPFLAGS -D_TRACE"] )

AM_PROG_CC_C_O

# Checking pkg-config is available
AC_CHECK_PROGS(PKG_CONFIG, [pkg-config], [])
if test -z $PKG_CONFIG 
then
	AC_MSG_ERROR([cannot find pkg-config program])
fi
# Checking libxml2 is configured with pkg-config
AC_MSG_CHECKING([if libxml2 is pkg-config configured])
$PKG_CONFIG libxml-2.0
if test $? -ne 0
then
	AC_MSG_ERROR([it seems libxml-2.0 is not configured with pkg-config])
else
	CPPFLAGS="$CPPFLAGS $($PKG_CONFIG --cflags libxml-2.0)"
	LDFLAGS_XML2="$LDFLAGS $($PKG_CONFIG --libs libxml-2.0)"
	AC_MSG_RESULT([yes])
fi
# Checking glib2 is configured with pkg-config
AC_MSG_CHECKING([if glib2 is pkg-config configured])
$PKG_CONFIG glib-2.0
if test $? -ne 0
then
	AC_MSG_ERROR([it seems glib-2.0 is not configured with pkg-config])
else
	CPPFLAGS="$CPPFLAGS $($PKG_CONFIG --cflags glib-2.0) $($PKG_CONFIG --cflags gmodule-no-export-2.0)"
	LDFLAGS_GLIB="$($PKG_CONFIG --libs glib-2.0)"
	LDFLAGS_GMODULE="$($PKG_CONFIG --libs gmodule-no-export-2.0)"
	LDFLAGS_GTHREAD="$($PKG_CONFIG --libs gthread-2.0)"
	AC_MSG_RESULT([yes])
fi

AC_CHECK_HEADERS(arpa/inet.h assert.h)
AC_CHECK_HEADERS(errno.h)
AC_CHECK_HEADERS(fcntl.h)
AC_CHECK_HEADERS(glib.h gmodule.h)
AC_CHECK_HEADERS(libgen.h)
AC_CHECK_HEADERS(libxml/xmlversion.h libxml/tree.h libxml/parser.h)
AC_CHECK_HEADERS(netdb.h netinet/in.h)
AC_CHECK_HEADERS(poll.h pthread.h)
AC_CHECK_HEADERS(signal.h stdarg.h stdint.h stdio.h stdlib.h string.h syslog.h)
AC_CHECK_HEADERS(sys/mman.h sys/socket.h sys/stat.h)
AC_CHECK_HEADERS(sys/time.h sys/types.h time.h)
AC_CHECK_HEADERS(unistd.h uuid/uuid.h)


AC_CHECK_TYPES([clock_t], [], [], [[#include <time.h>]])
AC_CHECK_TYPES([in_port_t], [], [], [[#include <netinet/in.h>]])
AC_CHECK_TYPES([nfds_t], [], [], [[#include <poll.h>]])
AC_CHECK_TYPES(off_t)
AC_CHECK_TYPES(pid_t)
AC_CHECK_TYPES(pthread_t)
AC_CHECK_TYPES(uint8_t)
AC_CHECK_TYPES(uint32_t)
AC_CHECK_TYPES(uintptr_t)
AC_CHECK_TYPES([uuid_t], [], \
	[AC_MSG_ERROR([cannot go on without uuid_t type])], \
	[[#include <uuid/uuid.h>]])
AC_CHECK_TYPES([in_addr_t], [], [], [[#include <netinet/in.h>]])
AC_CHECK_TYPES([in_port_t], [], [], [[#include <netinet/in.h>]])

AC_CHECK_SIZEOF([short int])
AC_CHECK_SIZEOF([int])
AC_CHECK_SIZEOF([long int])
AC_CHECK_SIZEOF([long long int])
AC_CHECK_SIZEOF([clock_t], [], [[#include <time.h>]])
AC_CHECK_SIZEOF([in_port_t], [], [[#include <netinet/in.h>]])
AC_CHECK_SIZEOF([off_t])
AC_CHECK_SIZEOF([nfds_t], [], [[#include <poll.h>]])
AC_CHECK_SIZEOF([pid_t], [], [[#include <sys/types.h>]])
AC_CHECK_SIZEOF([pthread_t], [], [[#include <pthread.h>]])
AC_CHECK_SIZEOF([size_t])
AC_CHECK_SIZEOF([uintptr_t])
AC_CHECK_SIZEOF([uint32_t])
AC_CHECK_SIZEOF([uuid_t], [], [[#include <uuid/uuid.h>]])

AC_CHECK_FUNCS(vfprintf)

AC_SUBST(PACKAGE_BUGREPORT)
AC_SUBST(PACKAGE_NAME)
AC_SUBST(PACKAGE_VERSION)
AC_SUBST(LT_LIXAC_CURRENT)
AC_SUBST(LT_LIXAC_REVISION)
AC_SUBST(LT_LIXAC_AGE)
AC_SUBST(LT_LIXAB_CURRENT)
AC_SUBST(LT_LIXAB_REVISION)
AC_SUBST(LT_LIXAB_AGE)
AC_SUBST(LDFLAGS_GLIB)
AC_SUBST(LDFLAGS_GMODULE)
AC_SUBST(LDFLAGS_GTHREAD)
AC_SUBST(LDFLAGS_XML2)
AC_SUBST(LTDLINCL)
AC_SUBST(LIBLTDL)
AC_SUBST(EXPANDED_SYSCONFDIR)
AC_SUBST(EXPANDED_LOCALSTATEDIR)
AC_SUBST(EXPANDED_LIBDIR)
AC_SUBST(LIXA_CRASH)

AC_OUTPUT

