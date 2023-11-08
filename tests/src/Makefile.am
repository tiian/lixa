if COMPILE_XTA_CPP
  MAYBE_XTACPP = cpp
endif
if COMPILE_XTA_JAVA
  MAYBEJAVA = java
endif
if COMPILE_XTA_PYTHON
  MAYBE_XTAPYTHON = python
endif
if COND_COBOLAPI
  MAYBE_COBOLAPI = cobol
endif
SUBDIRS = . $(MAYBE_XTACPP) $(MAYBEJAVA) $(MAYBE_XTAPYTHON) $(MAYBE_COBOLAPI)
noinst_HEADERS = case_test_functions.h

#AM_CPPFLAGS = -DLIXA_MULTITHREAD=0 -I../../src/client -I../../src/common \
#	-I../../src/client/switch/lixa
AM_CPPFLAGS = -I../../src/client -I../../src/common \
	-I../../src/client/switch/lixa
AM_CFLAGS = -Wall
#AM_LDFLAGS = -lpthread @LIBLTDL@ @LDFLAGS_GMODULE@
LDADD = ../../src/common/liblixab.la ../../src/client/liblixac.la \
	../../src/client/switch/lixa/liblixamonkey.la
if COMPILE_MYSQL_STUFF
  MAYBE_MYSQL = case0040 case0041 case0045 case0047
endif
if COMPILE_POSTGRESQL_STUFF
  MAYBE_POSTGRESQL = case0037
  LDADD_POSTGRESQL = ../../src/client/switch/postgresql/liblixapq.la
endif
if COMPILE_MYSQL_POSTGRESQL_STUFF
  MAYBE_MYSQL_POSTGRESQL = case0042
  LDADD_MYSQL = ../../src/client/switch/mysql/liblixamy.la
endif
if COMPILE_ORACLE_STUFF
  MAYBE_ORACLE = case0038
  LDADD_ORACLE = -lclntsh
endif
if COMPILE_IBMDB2_STUFF
  MAYBE_IBMDB2 = case0039
endif
if COMPILE_WEBSPHEREMQ_STUFF
  MAYBE_WEBSPHEREMQ = case0044
endif
if COMPILE_XTA
  MAYBE_XTA = case0100 case0101 case0102 case0103 case0104 case0105 case0106 \
	case0107 case0108 case0109 case0110 case0111 case0112
endif
check_PROGRAMS = case0000 case0001 case0002 case0003 case0004 case0005 \
	case0006 case0007 case0008 case0009 case0010 case0011 case0012 \
	case0013 case0014 case0015 case0016 case0017 case0018 case0019 \
	case0020 case0021 case0022 case0023 case0024 case0025 case0026 \
	case0027 case0028 case0029 case0030 case0031 case0032 case0033 \
	case0034 case0035 case0036 case0043 case0046 case0048 case0049 \
	case0050 case0051 case0052 case0053 case0054 \
	$(MAYBE_POSTGRESQL) $(MAYBE_ORACLE) \
	$(MAYBE_IBMDB2) $(MAYBE_MYSQL) $(MAYBE_MYSQL_POSTGRESQL) \
	$(MAYBE_WEBSPHEREMQ) $(MAYBE_XTA)
case0000_SOURCES = case0000.c
case0001_SOURCES = case0001.c
case0002_SOURCES = case0002.c
case0003_SOURCES = case0003.c
case0004_SOURCES = case0004.c
case0005_SOURCES = case0005.c
case0006_SOURCES = case0006.c
case0007_SOURCES = case0007.c
case0008_SOURCES = case0008.c
case0009_SOURCES = case0009.c
case0010_SOURCES = case0010.c
case0011_SOURCES = case0011.c
case0012_SOURCES = case0012.c
case0013_SOURCES = case0013.c
case0014_SOURCES = case0014.c
case0015_SOURCES = case0015.c
case0016_SOURCES = case0016.c
case0017_SOURCES = case0017.c
case0018_SOURCES = case0018.c
case0019_SOURCES = case0019.c
case0020_SOURCES = case0020.c
case0021_SOURCES = case0021.c
case0022_SOURCES = case0022.c
case0023_SOURCES = case0023.c
case0024_SOURCES = case0024.c
case0025_SOURCES = case0025.c
case0026_SOURCES = case0026.c
case0027_SOURCES = case0027.c
case0028_SOURCES = case0028.c
case0029_SOURCES = case0029.c
case0030_SOURCES = case0030.c
case0031_SOURCES = case0031.c
case0032_SOURCES = case0032.c
case0032_LDFLAGS = -lpthread @LDFLAGS_GLIB@ @LDFLAGS_GMODULE@ @LDFLAGS_XML2@
case0033_SOURCES = case0033.c
case0033_LDFLAGS = -lpthread @LDFLAGS_XML2@
case0034_SOURCES = case0034.c
case0034_LDADD = @LDFLAGS_GLIB@ ../../src/common/liblixab.la @LDFLAGS_XML2@ ../../src/client/switch/lixa/liblixamonkey.la ../../src/client/liblixac.la
case0035_SOURCES = case0035.c
case0036_SOURCES = case0036.c
case0036_LDADD = ../../src/common/liblixab.la @LDFLAGS_GLIB@ @LDFLAGS_GTHREAD@ @LDFLAGS_XML2@
case0037_SOURCES = case0037.c
case0037_CPPFLAGS = @POSTGRESQL_CPPFLAGS@ \
	-I../../src/client -I../../src/common \
	-I../../src/client/switch/postgresql
case0037_LDADD = ../../src/common/liblixab.la ../../src/client/liblixac.la \
	../../src/client/switch/postgresql/liblixapq.la
case0037_LDFLAGS = @POSTGRESQL_LDFLAGS@
case0038_SOURCES = case0038.c
case0038_CPPFLAGS = @ORACLE_OCI_CFLAGS@ -I../../src/client
case0038_LDFLAGS = @ORACLE_OCI_LDFLAGS@
case0038_LDADD = ../../src/common/liblixab.la ../../src/client/liblixac.la \
	-lclntsh
case0039_SOURCES = case0039.c
case0039_CPPFLAGS = @IBMDB2_CLI_CPPFLAGS@ -I../../src/client
case0039_LDFLAGS = @IBMDB2_CLI_LDFLAGS@
case0039_LDADD = ../../src/client/liblixac.la
case0040_SOURCES = case0040.c
case0041_SOURCES = case0041.c
case0041_CPPFLAGS = @MYSQL_CPPFLAGS@ \
	-I../../src/client -I../../src/common \
	-I../../src/client/switch/mysql
case0041_LDADD = ../../src/common/liblixab.la ../../src/client/liblixac.la \
	../../src/client/switch/mysql/liblixamy.la
case0041_LDFLAGS = @MYSQL_LDFLAGS@
case0042_CPPFLAGS = @MYSQL_CPPFLAGS@ @POSTGRESQL_CPPFLAGS@ \
	-I../../src/client -I../../src/common \
	-I../../src/client/switch/mysql \
	-I../../src/client/switch/postgresql
case0042_LDADD = ../../src/common/liblixab.la ../../src/client/liblixac.la \
	../../src/common/liblixab.la ../../src/client/liblixac.la \
	../../src/client/switch/mysql/liblixamy.la \
	../../src/client/switch/postgresql/liblixapq.la
case0042_LDFLAGS = @MYSQL_LDFLAGS@ @POSTGRESQL_LDFLAGS@
case0043_SOURCES = case0043.c
case0043_LDADD = ../../src/common/liblixab.la @LDFLAGS_GLIB@ @LDFLAGS_GTHREAD@ @LDFLAGS_XML2@
case0044_SOURCES = case0044.c
case0044_CPPFLAGS = @WEBSPHEREMQ_CPPFLAGS@ -I../../src/client
case0044_LDFLAGS = @WEBSPHEREMQ_LDFLAGS@ -l@WEBSPHEREMQ_PGMLIB@
case0044_LDADD = ../../src/client/liblixac.la
case0045_SOURCES = case0045.c
case0045_CPPFLAGS = @MYSQL_CPPFLAGS@ \
	-I../../src/client -I../../src/common \
	-I../../src/client/switch/mysql
case0045_LDADD = ../../src/common/liblixab.la ../../src/client/liblixac.la \
	../../src/client/switch/mysql/liblixamy.la
case0045_LDFLAGS = @MYSQL_LDFLAGS@
case0046_SOURCES = case0046.c
case0047_SOURCES = case0047.c
case0047_CPPFLAGS = @MYSQL_CPPFLAGS@ \
	-I../../src/client -I../../src/common \
	-I../../src/client/switch/mysql
case0047_LDADD = ../../src/common/liblixab.la @LDFLAGS_XML2@ \
	../../src/client/liblixac.la \
	../../src/client/switch/mysql/liblixamy.la
case0047_LDFLAGS = @LDFLAGS_GLIB@ @MYSQL_LDFLAGS@ @LDFLAGS_XML2@
case0048_SOURCES = case0048.c
case0048_CPPFLAGS = -I../../src/client -I../../src/common
case0048_LDADD = ../../src/common/liblixab.la @LDFLAGS_XML2@ \
    ../../src/client/liblixac.la
case0048_LDFLAGS = @LDFLAGS_GLIB@ @LDFLAGS_XML2@
case0049_SOURCES = case0049.c
case0050_SOURCES = case0050.c
case0051_SOURCES = case0051.c
case0052_SOURCES = case0052.c
case0053_SOURCES = case0053.c
case0054_SOURCES = case0054.c
case0100_CPPFLAGS = -I../../src/client -I../../src/common -I../../src \
	@ORACLE_OCI_CFLAGS@ @MYSQL_CPPFLAGS@ @POSTGRESQL_CPPFLAGS@
case0100_LDADD = ../../src/common/liblixab.la ../../src/client/liblixac.la \
	../../src/xta/liblixta.la $(LDADD_MYSQL) \
	$(LDADD_POSTGRESQL) $(LDADD_ORACLE)
case0100_SOURCES = case0100.c
case0100_LDFLAGS = @ORACLE_OCI_LDFLAGS@ @MYSQL_LDFLAGS@ @POSTGRESQL_LDFLAGS@
case0101_CPPFLAGS = -I../../src/client -I../../src/common -I../../src \
	@ORACLE_OCI_CFLAGS@ @MYSQL_CPPFLAGS@ @POSTGRESQL_CPPFLAGS@
case0101_LDADD = ../../src/common/liblixab.la ../../src/client/liblixac.la \
	../../src/xta/liblixta.la $(LDADD_MYSQL) \
	$(LDADD_POSTGRESQL) $(LDADD_ORACLE)
case0101_SOURCES = case0101.c
case0101_LDFLAGS = @ORACLE_OCI_LDFLAGS@ @MYSQL_LDFLAGS@ @POSTGRESQL_LDFLAGS@
case0102_CPPFLAGS = -I../../src/client -I../../src/common -I../../src \
	@ORACLE_OCI_CFLAGS@ @MYSQL_CPPFLAGS@ @POSTGRESQL_CPPFLAGS@
case0102_LDADD = ../../src/common/liblixab.la ../../src/client/liblixac.la \
	../../src/xta/liblixta.la $(LDADD_MYSQL) \
	$(LDADD_POSTGRESQL) $(LDADD_ORACLE)
case0102_SOURCES = case0102.c
case0102_LDFLAGS = @ORACLE_OCI_LDFLAGS@ @MYSQL_LDFLAGS@ @POSTGRESQL_LDFLAGS@
case0103_CPPFLAGS = -I../../src/client -I../../src/common -I../../src \
	@MYSQL_CPPFLAGS@ @POSTGRESQL_CPPFLAGS@
case0103_LDADD = ../../src/common/liblixab.la ../../src/client/liblixac.la \
	../../src/xta/liblixta.la $(LDADD_MYSQL) \
	$(LDADD_POSTGRESQL) $(LDADD_ORACLE)
case0103_SOURCES = case0103.c
case0103_LDFLAGS = @ORACLE_OCI_LDFLAGS@ @MYSQL_LDFLAGS@ @POSTGRESQL_LDFLAGS@
case0104_CPPFLAGS = -I../../src/client -I../../src/common -I../../src \
	@ORACLE_OCI_CFLAGS@ @MYSQL_CPPFLAGS@ @POSTGRESQL_CPPFLAGS@
case0104_LDADD = ../../src/common/liblixab.la ../../src/client/liblixac.la \
	../../src/xta/liblixta.la $(LDADD_MYSQL) \
	$(LDADD_POSTGRESQL) $(LDADD_ORACLE)
case0104_SOURCES = case0104.c case_test_functions.c
case0104_LDFLAGS = @ORACLE_OCI_LDFLAGS@ @MYSQL_LDFLAGS@ @POSTGRESQL_LDFLAGS@
case0105_CPPFLAGS = -I../../src/client -I../../src/common -I../../src \
	@ORACLE_OCI_CFLAGS@ @MYSQL_CPPFLAGS@ @POSTGRESQL_CPPFLAGS@
case0105_LDADD = ../../src/common/liblixab.la ../../src/client/liblixac.la \
	../../src/xta/liblixta.la $(LDADD_MYSQL) \
	$(LDADD_POSTGRESQL) $(LDADD_ORACLE)
case0105_SOURCES = case0105.c
case0105_LDFLAGS = @ORACLE_OCI_LDFLAGS@ @MYSQL_LDFLAGS@ @POSTGRESQL_LDFLAGS@
case0106_CPPFLAGS = -I../../src/client -I../../src/common -I../../src \
	@ORACLE_OCI_CFLAGS@ @MYSQL_CPPFLAGS@ @POSTGRESQL_CPPFLAGS@
case0106_LDADD = ../../src/common/liblixab.la ../../src/client/liblixac.la \
	../../src/xta/liblixta.la $(LDADD_MYSQL) \
	$(LDADD_POSTGRESQL) $(LDADD_ORACLE)
case0106_SOURCES = case0106.c case_test_functions.c
case0106_LDFLAGS = @ORACLE_OCI_LDFLAGS@ @MYSQL_LDFLAGS@ @POSTGRESQL_LDFLAGS@
case0107_CPPFLAGS = -I../../src/client -I../../src/common -I../../src \
	@MYSQL_CPPFLAGS@ @POSTGRESQL_CPPFLAGS@
case0107_LDADD = ../../src/common/liblixab.la ../../src/client/liblixac.la \
	../../src/xta/liblixta.la $(LDADD_MYSQL) $(LDADD_POSTGRESQL)
case0107_SOURCES = case0107.c case_test_functions.c
case0107_LDFLAGS = @MYSQL_LDFLAGS@ @POSTGRESQL_LDFLAGS@
case0108_CPPFLAGS = -I../../src/client -I../../src/common -I../../src \
	@ORACLE_OCI_CFLAGS@ @MYSQL_CPPFLAGS@ @POSTGRESQL_CPPFLAGS@
case0108_LDADD = ../../src/common/liblixab.la ../../src/client/liblixac.la \
	../../src/xta/liblixta.la $(LDADD_MYSQL) \
	$(LDADD_POSTGRESQL) $(LDADD_ORACLE)
case0108_SOURCES = case0108.c case_test_functions.c
case0108_LDFLAGS = @ORACLE_OCI_LDFLAGS@ @MYSQL_LDFLAGS@ @POSTGRESQL_LDFLAGS@
case0109_CPPFLAGS = -I../../src/client -I../../src/common -I../../src \
	@MYSQL_CPPFLAGS@ @POSTGRESQL_CPPFLAGS@
case0109_LDADD = ../../src/common/liblixab.la ../../src/client/liblixac.la \
	../../src/xta/liblixta.la
case0109_SOURCES = case0109.c
case0110_CPPFLAGS = -I../../src/client -I../../src/common -I../../src \
	@MYSQL_CPPFLAGS@ @POSTGRESQL_CPPFLAGS@
case0110_LDADD = ../../src/common/liblixab.la ../../src/client/liblixac.la \
	../../src/xta/liblixta.la
case0110_SOURCES = case0110.c
case0111_CPPFLAGS = -I../../src/client -I../../src/common -I../../src \
	@ORACLE_OCI_CFLAGS@ @MYSQL_CPPFLAGS@ @POSTGRESQL_CPPFLAGS@
case0111_LDADD = ../../src/common/liblixab.la ../../src/client/liblixac.la \
	../../src/xta/liblixta.la $(LDADD_MYSQL) \
	$(LDADD_POSTGRESQL) $(LDADD_ORACLE)
case0111_SOURCES = case0111.c
case0111_LDFLAGS = @ORACLE_OCI_LDFLAGS@ @MYSQL_LDFLAGS@ @POSTGRESQL_LDFLAGS@
case0112_CPPFLAGS = -I../../src/client -I../../src/common -I../../src \
	@ORACLE_OCI_CFLAGS@ @MYSQL_CPPFLAGS@ @POSTGRESQL_CPPFLAGS@
case0112_LDADD = ../../src/common/liblixab.la ../../src/client/liblixac.la \
	../../src/xta/liblixta.la $(LDADD_MYSQL) \
	$(LDADD_POSTGRESQL) $(LDADD_ORACLE)
case0112_SOURCES = case0112.c
case0112_LDFLAGS = @ORACLE_OCI_LDFLAGS@ @MYSQL_LDFLAGS@ @POSTGRESQL_LDFLAGS@

