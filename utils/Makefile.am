scriptdir = $(bindir)
dist_noinst_DATA = Makefile.am lixa_env.sh.in
dist_script_DATA = lixa_env.sh

lixa_env.sh: lixa_env.sh.in
	sed \
	-e 's|@LIXA_CONFIG_BINDIR[@]|$(LIXA_CONFIG_BINDIR)|g' \
	-e 's|@LIXA_CONFIG_LIBDIR[@]|$(LIXA_CONFIG_LIBDIR)|g' \
	-e 's|@LIXA_CONFIG_PREFIX[@]|$(LIXA_CONFIG_PREFIX)|g' \
	$< >$@
