SUBDIRS = lixavsr
etcdir = $(sysconfdir)
dist_etc_DATA = lixac_conf.xml lixac1_conf.xml lixad_conf.xml lixad1_conf.xml
dist_noinst_DATA = lixac_conf.xml.in lixad_conf.xml.in lixad1_conf.xml.in

lixac_conf.xml:	lixac_conf.xml.in
	sed \
	-e 's|@EXPANDED_LIBDIR[@]|$(EXPANDED_LIBDIR)|g' \
	-e 's|@ORACLE_SQL_NET[@]|$(ORACLE_SQL_NET)|g' \
	-e 's|@ORACLE_RM_TYPE[@]|$(ORACLE_RM_TYPE)|g' \
	$< >$@

lixad_conf.xml:	lixad_conf.xml.in
	sed \
	-e 's|@EXPANDED_LOCALSTATEDIR[@]|$(EXPANDED_LOCALSTATEDIR)|g' \
	$< >$@

lixad1_conf.xml:	lixad1_conf.xml.in
	sed \
	-e 's|@EXPANDED_LOCALSTATEDIR[@]|$(EXPANDED_LOCALSTATEDIR)|g' \
	$< >$@

