SUBDIRS = etc src scripts

# Interrupt the test at first failure
TESTSUITEFLAGS = "-e"

# Always generate package.m4 into the source directory, not into the
# build directory, since it must be distributed, along with testsuite,
# configure, etc.
$(srcdir)/package.m4: $(top_srcdir)/configure.ac
	:;{ \
	  echo '# Signature of the current package.' && \
	  echo 'm4_define([AT_PACKAGE_NAME],      [$(PACKAGE_NAME)])' && \
	  echo 'm4_define([AT_PACKAGE_TARNAME],   [$(PACKAGE_TARNAME)])' && \
	  echo 'm4_define([AT_PACKAGE_VERSION],   [$(PACKAGE_VERSION)])' && \
	  echo 'm4_define([AT_PACKAGE_STRING],    [$(PACKAGE_STRING)])' && \
	  echo 'm4_define([AT_PACKAGE_BUGREPORT], [$(PACKAGE_BUGREPORT)])'; \
	  echo 'm4_define([AT_PACKAGE_URL],       [$(PACKAGE_URL)])'; \
	} > $@-t
	mv $@-t $@

EXTRA_DIST = bug.at bug_2944441.at bug_2983464.at \
	generic.at generic_client.at generic_heur.at generic_norm.at \
	testsuite.at nonapi.at \
	tx.at tx_3_2.at tx_3_3.at tx_3_3_1.at tx_3_4.at tx_3_5.at \
	tx_3_6.at tx_3_8.at tx_4_2.at tx_5_19.at \
	tx_5_20.at tx_5_22.at tx_5_23.at \
	tx_5_25.at tx_5_26.at tx_5_27.at tx_7_52.at tx_b_2.at tx_b_4.at \
	tx_b_5.at tx_b_5_0.at tx_b_5_1.at tx_b_5_2.at tx_b_5_3.at \
	tx_b_5_4.at tx_b_5_5.at tx_b_5_6.at tx_b_5_7.at tx_b_5_8.at \
	tx_b_5_9.at tx_b_5_10.at \
	tx_cobol.at \
	xa.at xa_5_26.at xa_5_30.at xa_5_32.at xa_5_37.at xa_5_40.at \
	xa_5_42.at xa_5_44.at xa_5_47.at xa_5_49.at xa_5_52.at \
	xa_5_37_suspend.at xa_5_37_suspended.at xa_5_37_resumed.at \
	xa_6_62.at xa_5_52_resume.at \
	mt.at mt_1_0.at mt_2_0.at \
	ct.at ct_0_0.at ct_0_1.at ct.at ct_0_2.at ct_0_3.at ct_0_4.at \
	ct_0_5.at ct_0_6.at ct_0_7.at ct_0_8.at \
	ct_1_0.at ct_1_1.at ct_1_2.at ct_1_3.at ct_1_4.at ct_1_5.at \
	ct_1_6.at ct_1_7.at ct_1_8.at ct_1_9.at ct_1_10.at ct_1_11.at \
	ct_1_12.at ct_1_13.at \
	memory.at oracle.at.in ibmdb2.at postgresql.at mysql.at xid.at \
	package.m4 mysql_postgresql.at webspheremq.at lixa-config.at.in \
	lixavsr.at.in \
	xta.at xta_1.at xta_2.at xta_3.at xta_4.at \
	xta_5_0.at xta_5_5.at xta_6.at xta_7.at xta_8.at \
	lixa_test_group.sh lixa_test_exec.sh lixa_test_functions.sh \
	db_consist_check.sh \
	lixac.supp lixad.supp \
	atlocal.in $(TESTSUITE)
TESTSUITE = $(srcdir)/testsuite

lixavsr.at: lixavsr.at.in
	sed \
	-e 's|@abs_top_srcdir[@]|$(abs_top_srcdir)|g' \
	$< >$@
	
oracle.at: oracle.at.in
	sed \
	-e 's|@ORACLE_SQL_NET[@]|@$(ORACLE_SQL_NET)|g' \
	$< >$@

check-local: atconfig atlocal $(TESTSUITE)
	$(SHELL) '$(TESTSUITE)' $(TESTSUITEFLAGS)
installcheck-local: atconfig atlocal $(TESTSUITE)
	$(SHELL) '$(TESTSUITE)' $(TESTSUITEFLAGS)
clean-local:
	test ! -f '$(TESTSUITE)' || \
	  $(SHELL) '$(TESTSUITE)' --clean

AUTOM4TE = autom4te
AUTOTEST = $(AUTOM4TE) --language=autotest

# Always generate testsuite into the source directory, not into the
# build directory, since it must be distributed, along with
# package.m4, configure, etc.
$(TESTSUITE): $(srcdir)/testsuite.at \
		$(srcdir)/bug.at \
		$(srcdir)/bug_2944441.at \
		$(srcdir)/bug_2983464.at \
		$(srcdir)/generic.at $(srcdir)/nonapi.at \
		$(srcdir)/generic_client.at \
		$(srcdir)/generic_heur.at $(srcdir)/generic_norm.at \
		$(srcdir)/tx.at \
		$(srcdir)/tx_3_2.at $(srcdir)/tx_3_3.at \
		$(srcdir)/tx_3_3_1.at \
		$(srcdir)/tx_3_4.at $(srcdir)/tx_3_5.at \
		$(srcdir)/tx_3_6.at $(srcdir)/tx_3_8.at \
		$(srcdir)/tx_4_2.at \
		$(srcdir)/tx_5_19.at $(srcdir)/tx_5_20.at \
		$(srcdir)/tx_5_22.at $(srcdir)/tx_5_23.at \
		$(srcdir)/tx_5_25.at $(srcdir)/tx_5_26.at \
		$(srcdir)/tx_5_27.at $(srcdir)/tx_7_52.at \
		$(srcdir)/tx_b_2.at $(srcdir)/tx_b_4.at \
		$(srcdir)/tx_b_5.at $(srcdir)/tx_b_5_0.at \
		$(srcdir)/tx_b_5_1.at $(srcdir)/tx_b_5_2.at \
		$(srcdir)/tx_b_5_3.at $(srcdir)/tx_b_5_4.at \
		$(srcdir)/tx_b_5_5.at $(srcdir)/tx_b_5_6.at \
		$(srcdir)/tx_b_5_7.at $(srcdir)/tx_b_5_8.at \
		$(srcdir)/tx_b_5_9.at $(srcdir)/tx_b_5_10.at \
		$(srcdir)/tx_cobol.at \
		$(srcdir)/xa.at \
		$(srcdir)/xa_5_26.at $(srcdir)/xa_5_30.at \
		$(srcdir)/xa_5_32.at $(srcdir)/xa_5_37.at \
		$(srcdir)/xa_5_37_suspend.at \
		$(srcdir)/xa_5_37_suspended.at $(srcdir)/xa_5_37_resumed.at \
		$(srcdir)/xa_5_40.at $(srcdir)/xa_5_42.at \
		$(srcdir)/xa_5_44.at $(srcdir)/xa_5_47.at \
		$(srcdir)/xa_5_49.at $(srcdir)/xa_5_52.at \
		$(srcdir)/xa_5_52_resume.at \
		$(srcdir)/xa_6_62.at \
		$(srcdir)/mt.at \
		$(srcdir)/mt_1_0.at $(srcdir)/mt_2_0.at \
		$(srcdir)/memory.at $(srcdir)/postgresql.at \
		$(srcdir)/ibmdb2.at $(srcdir)/oracle.at \
		$(srcdir)/mysql.at $(srcdir)/xid.at \
		$(srcdir)/mysql_postgresql.at $(srcdir)/webspheremq.at \
		$(srcdir)/lixa-config.at $(srcdir)/lixavsr.at \
		$(srcdir)/ct.at $(srcdir)/ct_0_0.at \
		$(srcdir)/ct_0_1.at $(srcdir)/ct_0_2.at \
		$(srcdir)/ct_0_3.at $(srcdir)/ct_0_4.at \
		$(srcdir)/ct_0_5.at $(srcdir)/ct_0_6.at \
		$(srcdir)/ct_0_7.at $(srcdir)/ct_0_8.at \
		$(srcdir)/ct_1_0.at $(srcdir)/ct_1_1.at \
		$(srcdir)/ct_1_2.at $(srcdir)/ct_1_3.at \
		$(srcdir)/ct_1_4.at $(srcdir)/ct_1_5.at \
		$(srcdir)/ct_1_6.at $(srcdir)/ct_1_7.at \
		$(srcdir)/ct_1_8.at $(srcdir)/ct_1_9.at \
		$(srcdir)/ct_1_10.at $(srcdir)/ct_1_11.at \
		$(srcdir)/ct_1_12.at $(srcdir)/ct_1_13.at \
		$(srcdir)/xta.at $(srcdir)/xta_1.at $(srcdir)/xta_2.at \
		$(srcdir)/xta_3.at $(srcdir)/xta_4.at \
		$(srcdir)/xta_5_0.at $(srcdir)/xta_5_5.at \
		$(srcdir)/xta_6.at $(srcdir)/xta_7.at $(srcdir)/xta_8.at \
		$(srcdir)/package.m4
	$(AUTOTEST) -I '$(srcdir)' -o $@.tmp $@.at
	mv $@.tmp $@
DISTCLEANFILES = atconfig
