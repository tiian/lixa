AC_INIT([lixa], [1.1.0-dev], [tiian at users dot sourceforge dot net], [], [http://sourceforge.net/projects/lixa/])
AM_INIT_AUTOMAKE([-Wall -Werror])
m4_ifdef([AM_PROG_AR], [AM_PROG_AR])

AC_PROG_LIBTOOL
AM_PROG_LIBTOOL

AC_PROG_CC
AC_CONFIG_HEADER([config.h])
AC_CONFIG_FILES([
  Makefile
  doc/Makefile
  doc/examples/Makefile
  doc/examples/cobol/Makefile
  doc/images/Makefile
  doc/manuals/LIXA.xml
  doc/manuals/Makefile
  etc/lixac_conf.xml
  etc/lixad_conf.xml
  etc/Makefile
  ext/Makefile
  ext/doc/Makefile
  ext/doc/manuals/LIXA_EXT.xml
  ext/doc/manuals/Makefile
  ext/php/Makefile
  ext/php/examples/Makefile
  ext/php/tests/Makefile
  src/Makefile
  src/common/Makefile
  src/common/lixa_inst_conf.c
  src/client/lixa-config.c
  src/client/Makefile
  src/client/switch/Makefile
  src/client/switch/lixa/Makefile
  src/client/switch/ibmdb2/Makefile
  src/client/switch/oracle/Makefile
  src/client/switch/postgresql/Makefile
  src/client/switch/mysql/Makefile
  src/client/switch/webspheremq/Makefile
  src/server/Makefile
  tests/Makefile
  tests/atlocal
  tests/lixa-config.at
  tests/etc/lixac_conf.xml
  tests/etc/lixad_conf.xml
  tests/etc/lixad_conf2.xml
  tests/etc/Makefile
  tests/scripts/Makefile
  tests/src/Makefile
  tests/src/cobol/Makefile
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
AM_CONDITIONAL([COMPILE_ORACLE_STUFF], [test "$HAVE_ORACLE" = "yes"])
# Include IBM DB2 test
m4_include(m4include/ax_lib_ibmdb2_cli.m4)
AX_LIB_IBMDB2_CLI
AM_CONDITIONAL([COMPILE_IBMDB2_STUFF], [test -n "$ibmdb2_home_dir"])
# Include PostgreSQL test
m4_include(m4include/ax_lib_postgresql.m4)
AX_LIB_POSTGRESQL
AM_CONDITIONAL([COMPILE_POSTGRESQL_STUFF], [test "$HAVE_POSTGRESQL" = "yes"])
# Include MySQL test
m4_include(m4include/ax_lib_mysql.m4)
AX_LIB_MYSQL
AM_CONDITIONAL([COMPILE_MYSQL_STUFF], [test "$HAVE_MYSQL" = "yes"])
AM_CONDITIONAL([COMPILE_MYSQL_POSTGRESQL_STUFF], [test "$HAVE_MYSQL" = "yes" -a "$HAVE_POSTGRESQL" = "yes"])
# Include WebSphereMQ test
m4_include(m4include/ax_lib_webspheremq.m4)
AX_LIB_WEBSPHEREMQ
AM_CONDITIONAL([COMPILE_WEBSPHEREMQ_STUFF], [test "$HAVE_WEBSPHEREMQ" = "yes"])
# Include Docbook test
m4_include(m4include/ax_docbook.m4)
AX_DOCBOOK
AM_CONDITIONAL([COMPILE_DOCBOOK_STUFF], [test "$HAVE_DOCBOOK" = "yes"])

#
# From libtool manual:
#
# Here are a set of rules to help you update your library version information:
# 1. Start with version information of ‘0:0:0’ for each libtool library.
# 2. Update the version information only immediately before a public release 
#    of your software. More frequent updates are unnecessary, and only 
#    guarantee that the current interface number gets larger faster.
# 3. If the library source code has changed at all since the last update, then
#    increment revision (‘c :r :a ’ becomes ‘c :r + 1:a ’).
# 4. If any interfaces have been added, removed, or changed since the last
#    update, increment current, and set revision to 0.
# 5. If any interfaces have been added since the last public release, then
#    increment age.
# 6. If any interfaces have been removed since the last public release, then
#    set age to 0.
#
# liblixac and liblixacmt libraries version
LT_LIXAC_CURRENT=1
LT_LIXAC_REVISION=0
LT_LIXAC_AGE=1
# liblixab and liblixbcmt libraries version
LT_LIXAB_CURRENT=0
LT_LIXAB_REVISION=1
LT_LIXAB_AGE=0

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
#	CPPFLAGS="$CPPFLAGS $($PKG_CONFIG --cflags glib-2.0) $($PKG_CONFIG --cflags gmodule-2.0)"
	LDFLAGS_GLIB="$($PKG_CONFIG --libs glib-2.0)"
	LDFLAGS_GMODULE="$($PKG_CONFIG --libs gmodule-no-export-2.0)"
#	LDFLAGS_GMODULE="$($PKG_CONFIG --libs gmodule-2.0)"
	LDFLAGS_GTHREAD="$($PKG_CONFIG --libs gthread-2.0)"
	AC_MSG_RESULT([yes])
fi

# Checking cobc (GNU COBOL compiler) is available
AC_CHECK_PROGS(COBC, [cobc], [true])
AC_CHECK_PROG(HAVE_COBOL, [cobc], [yes], [no])
# Build COBOL TX interface?
AC_ARG_ENABLE([cobol],
	AS_HELP_STRING([--disable-cobol],
		[do not create COBOL TX API]),
	[COBOL="no"], [COBOL="yes"])
# COBOL stuff is conditional
if test "$HAVE_COBOL" = "yes" -a "$COBOL" = "yes"
then
	HAVE_COBOLAPI="yes"
else
	HAVE_COBOLAPI="no"
fi
AM_CONDITIONAL([COND_COBOLAPI], [test "$HAVE_COBOLAPI" = "yes"])
if test "$HAVE_COBOLAPI" = "yes"
then
        AC_DEFINE([HAVE_GNU_COBOL], [1], [Define to 1 if you are using GNU COBOL])
fi
AC_SUBST([HAVE_COBOLAPI])


# Vars useful for lixa-config utility program
eval "LIXA_CONFIG_INCLUDEDIR=$includedir"
eval "LIXA_CONFIG_LIBDIR=$libdir"
eval "LIXA_CONFIG_PREFIX=$prefix"
eval "LIXA_CONFIG_LOCALSTATEDIR=$localstatedir"
eval "LIXA_CONFIG_SYSCONFDIR=$sysconfdir"

AC_CHECK_PROGS(GDB, [gdb], [])
AC_CHECK_PROGS(VALGRIND, [valgrind], [])
AC_CHECK_PROGS(XSLTPROC, [xsltproc], [])
AC_CHECK_PROGS(HAVE_DOT, [dot], [NO], [])
AC_CHECK_PROGS(HAVE_NETSTAT, [netstat], [NO], [])
if test "$HAVE_NETSTAT" = "NO"
then
	AC_MSG_WARN([netstat was not found, make check can not be executed])
fi

AC_CHECK_HEADERS(arpa/inet.h)
AC_CHECK_HEADERS(assert.h)
AC_CHECK_HEADERS(errno.h)
AC_CHECK_HEADERS(fcntl.h)
AC_CHECK_HEADERS(glib.h)
AC_CHECK_HEADERS(gmodule.h)
AC_CHECK_HEADERS(libgen.h)
AC_CHECK_HEADERS(libxml/xmlversion.h)
AC_CHECK_HEADERS(libxml/tree.h)
AC_CHECK_HEADERS(libxml/parser.h)
AC_CHECK_HEADERS(math.h)
AC_CHECK_HEADERS(netdb.h)
AC_CHECK_HEADERS(netinet/in.h)
AC_CHECK_HEADERS(netinet/tcp.h)
AC_CHECK_HEADERS(poll.h)
AC_CHECK_HEADERS(pthread.h)
AC_CHECK_HEADERS(regex.h)
AC_CHECK_HEADERS(signal.h)
AC_CHECK_HEADERS(stdarg.h)
AC_CHECK_HEADERS(stdint.h)
AC_CHECK_HEADERS(stdio.h)
AC_CHECK_HEADERS(stdlib.h)
AC_CHECK_HEADERS(string.h)
AC_CHECK_HEADERS(syslog.h)
AC_CHECK_HEADERS(sys/mman.h)
AC_CHECK_HEADERS(sys/select.h)
AC_CHECK_HEADERS(sys/socket.h)
AC_CHECK_HEADERS(sys/stat.h)
AC_CHECK_HEADERS(sys/time.h)
AC_CHECK_HEADERS(sys/types.h)
AC_CHECK_HEADERS(time.h)
AC_CHECK_HEADERS(unistd.h)
AC_CHECK_HEADERS(uuid/uuid.h)

AC_SEARCH_LIBS([sqrt], [m])

AC_CHECK_TYPES([uuid_t], [], \
	[AC_MSG_ERROR([cannot go on without uuid_t type])], 
	[[#include <uuid/uuid.h>]])
AC_CHECK_TYPES([clock_t], [], [], [[#include <time.h>]])
AC_CHECK_TYPES([in_port_t], [], [], [[#include <netinet/in.h>]])
AC_CHECK_TYPES([nfds_t], [], [], [[#include <poll.h>]])
AC_CHECK_TYPES(off_t)
AC_CHECK_TYPES(pid_t)
AC_CHECK_TYPES(pthread_t)
AC_CHECK_TYPES(uint8_t)
AC_CHECK_TYPES(uint32_t)
AC_CHECK_TYPES(uintptr_t)
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
AC_SUBST(XSLTPROC)
AC_SUBST(HAVE_DOT)
AC_SUBST(LIXA_CONFIG_INCLUDEDIR)
AC_SUBST(LIXA_CONFIG_LIBDIR)
AC_SUBST(LIXA_CONFIG_PREFIX)
AC_SUBST(LIXA_CONFIG_LOCALSTATEDIR)
AC_SUBST(LIXA_CONFIG_SYSCONFDIR)

AC_OUTPUT
