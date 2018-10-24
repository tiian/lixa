AM_CFLAGS =
AM_CPPFLAGS = @PHP_INCLUDES@ -I ..
#AM_LDFLAGS = -module -Wl,--no-as-needed -Wl,-lglib-2.0 -Wl,--as-needed
dist_noinst_DATA = xta.i
# Unsure if it's better to use flom_php_module_la_LIBADD or 
# flom_php_module_la_LDADD
xta_php_module_la_LIBADD = @GLIB2_LIBS@ ../cpp/libxtapp.la
# PHP version to use
if COND_PHP7
 PHP_VERSION=-php7
else
 PHP_VERSION=-php
endif
xta_wrap.cpp: xta.i
	$(SWIG) $(PHP_VERSION) -I.. -o $@ $<

nodist_xta_php_module_la_SOURCES = xta_wrap.cpp

lib_LTLIBRARIES = xta-php-module.la
phpdir = $(datadir)/$(PACKAGE)/php
php_DATA = xta.php
EXTRA_DIST = xta.php php_xta.h

install-exec-hook:
	ln -sf $(DESTDIR)$(libdir)/xta-php-module.so \
	@PHP_INSTALL_PATH@/xta.so || echo "WARNING: module xta.so not installed in PHP default path @PHP_INSTALL_PATH@"

uninstall-hook:
	rm -f @PHP_INSTALL_PATH@/xta.so
	if test -d ${pkgdatadir}/php; then rmdir ${pkgdatadir}/php; fi

