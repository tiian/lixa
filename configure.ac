AC_INIT([lixa], [0.1.1], [tiian@users.sourceforge.net])
AM_INIT_AUTOMAKE([-Wall -Werror])
AC_PROG_CC
AC_CONFIG_HEADER([config.h])
AC_CONFIG_FILES([
  Makefile
  src/Makefile
  src/common/Makefile
  src/client/Makefile
  src/server/Makefile
  src/sample/Makefile
])

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

AC_PRESERVE_HELP_ORDER
AC_ARG_ENABLE([debug],
        AS_HELP_STRING([--enable-debug],
                [enable debug code (assert statements)]),
        [NDEBUG="no"], [NDEBUG="yes"; CPPFLAGS="$CPPFLAGS -DNDEBUG"] )
AC_ARG_ENABLE([trace],
        AS_HELP_STRING([--disable-trace],
                [disable trace feature (not suggested)]),
        [_TRACE="no"], [_TRACE="yes"; CPPFLAGS="$CPPFLAGS -D_TRACE"] )

AM_PROG_CC_C_O
AC_PROG_RANLIB
AC_PROG_LIBTOOL

AC_CHECK_HEADERS(arpa/inet.h assert.h)
AC_CHECK_HEADERS(errno.h)
AC_CHECK_HEADERS(fcntl.h)
AC_CHECK_HEADERS(libxml/xmlversion.h libxml/tree.h libxml/parser.h)
AC_CHECK_HEADERS(netdb.h netinet/in.h)
AC_CHECK_HEADERS(poll.h pthread.h)
AC_CHECK_HEADERS(signal.h stdarg.h stdio.h stdlib.h string.h syslog.h)
AC_CHECK_HEADERS(sys/mman.h sys/socket.h sys/stat.h)
AC_CHECK_HEADERS(sys/time.h sys/types.h)
AC_CHECK_HEADERS(unistd.h)


AC_CHECK_TYPES([nfds_t], [], [], [[#include <poll.h>]])
AC_CHECK_TYPES(pthread_t)
AC_CHECK_TYPES(uint8_t)
AC_CHECK_TYPES(uint32_t)
AC_CHECK_TYPES([in_addr_t], [], [], [[#include <netinet/in.h>]])
AC_CHECK_TYPES([in_port_t], [], [], [[#include <netinet/in.h>]])

AC_CHECK_FUNCS(vfprintf)

AC_SUBST(LT_LIXAC_CURRENT)
AC_SUBST(LT_LIXAC_REVISION)
AC_SUBST(LT_LIXAC_AGE)
AC_SUBST(LT_LIXAB_CURRENT)
AC_SUBST(LT_LIXAB_REVISION)
AC_SUBST(LT_LIXAB_AGE)

AC_OUTPUT

