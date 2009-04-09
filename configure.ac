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
])

AC_PRESERVE_HELP_ORDER
AC_ARG_ENABLE([debug],
        AS_HELP_STRING([--enable-debug],
                [enable debug code normally skipped]),
        [_DEBUG="yes"; CPPFLAGS="$CPPFLAGS -D_DEBUG"], [_DEBUG="no"] )
AC_ARG_ENABLE([trace],
        AS_HELP_STRING([--disable-trace],
                [disable trace feature (not suggested)]),
        [_TRACE="no"], [_TRACE="yes"; CPPFLAGS="$CPPFLAGS -D_TRACE"] )

AM_PROG_CC_C_O
AC_PROG_RANLIB

AC_CHECK_HEADERS(errno.h fcntl.h pthread.h)
AC_CHECK_HEADERS(signal.h stdarg.h stdio.h stdlib.h string.h syslog.h)
AC_CHECK_HEADERS(sys/socket.h sys/stat.h)
AC_CHECK_HEADERS(sys/time.h sys/types.h)
AC_CHECK_HEADERS(unistd.h)
AC_CHECK_HEADERS(libxml/xmlversion.h libxml/tree.h libxml/parser.h)

AC_CHECK_TYPES(uint8_t)
AC_CHECK_TYPES(uint32_t)

AC_CHECK_FUNCS(vfprintf)

AC_OUTPUT

