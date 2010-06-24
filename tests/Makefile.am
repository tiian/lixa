SUBDIRS = etc src

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
	generic.at generic_client.at generic_heur.at \
	testsuite.at \
	tx.at tx_3_2.at tx_3_3.at tx_3_3_1.at tx_3_4.at tx_3_5.at \
	tx_3_6.at tx_3_8.at tx_4_2.at tx_5_19.at \
	tx_5_20.at tx_5_22.at tx_5_23.at \
	tx_5_25.at tx_5_26.at tx_5_27.at tx_7_52.at tx_b_2.at tx_b_4.at \
	tx_b_5.at tx_b_5_0.at tx_b_5_1.at tx_b_5_2.at tx_b_5_3.at \
	tx_b_5_4.at tx_b_5_5.at tx_b_5_6.at tx_b_5_7.at tx_b_5_8.at \
	tx_b_5_9.at tx_b_5_10.at \
	xa.at xa_5_26.at \
	package.m4 \
	$(TESTSUITE) atlocal.in lixa_test_exec.sh lixa_test_functions.sh
TESTSUITE = $(srcdir)/testsuite

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
		$(srcdir)/generic.at \
		$(srcdir)/generic_client.at \
		$(srcdir)/generic_heur.at \
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
		$(srcdir)/xa.at \
		$(srcdir)/xa_5_26.at \
		$(srcdir)/package.m4
	$(AUTOTEST) -I '$(srcdir)' -o $@.tmp $@.at
	mv $@.tmp $@
DISTCLEANFILES = atconfig
