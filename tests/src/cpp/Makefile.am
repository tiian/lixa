if COMPILE_XTA_CPP
if COMPILE_MYSQL_STUFF
if COMPILE_POSTGRESQL_STUFF
  MAYBE_XTA_SRC1 = case2100 case2104 case2105 case2106 case2107
endif
endif
if COMPILE_ORACLE_STUFF
  MAYBE_ORACLE = case2102
  LDADD_ORACLE = -lclntsh
endif
endif

check_PROGRAMS = $(MAYBE_XTA_SRC1) $(MAYBE_ORACLE)
case2100_SOURCES = case2100.cpp
case2100_CPPFLAGS = -I../../../src/client -I../../../src/common \
	@MYSQL_CPPFLAGS@ @POSTGRESQL_CPPFLAGS@
case2100_LDFLAGS = @MYSQL_LDFLAGS@ @POSTGRESQL_LDFLAGS@
case2100_LDADD = ../../../src/xta/cpp/liblixtapp.la

case2102_SOURCES = case2102.cpp
case2102_CPPFLAGS = @ORACLE_OCI_CFLAGS@ \
	@MYSQL_CPPFLAGS@ @POSTGRESQL_CPPFLAGS@ \
	-I../../../src/client -I../../../src/common
case2102_LDFLAGS = @ORACLE_OCI_LDFLAGS@
case2102_LDADD = ../../../src/xta/cpp/liblixtapp.la \
        -lclntsh

case2104_SOURCES = case2104.cpp
case2104_CPPFLAGS = -I../../../src/client -I../../../src/common \
	@MYSQL_CPPFLAGS@ @POSTGRESQL_CPPFLAGS@
case2104_LDFLAGS = @MYSQL_LDFLAGS@ @POSTGRESQL_LDFLAGS@
case2104_LDADD = ../../../src/xta/cpp/liblixtapp.la

case2105_SOURCES = case2105.cpp
case2105_CPPFLAGS = -I../../../src/client -I../../../src/common \
	@MYSQL_CPPFLAGS@ @POSTGRESQL_CPPFLAGS@
case2105_LDFLAGS = @MYSQL_LDFLAGS@ @POSTGRESQL_LDFLAGS@
case2105_LDADD = ../../../src/xta/cpp/liblixtapp.la

case2106_SOURCES = case2106.cpp
case2106_CPPFLAGS = -I../../../src/client -I../../../src/common \
	@MYSQL_CPPFLAGS@ @POSTGRESQL_CPPFLAGS@
case2106_LDFLAGS = @MYSQL_LDFLAGS@ @POSTGRESQL_LDFLAGS@
case2106_LDADD = ../../../src/xta/cpp/liblixtapp.la

case2107_SOURCES = case2107.cpp
case2107_CPPFLAGS = -I../../../src/client -I../../../src/common \
	@MYSQL_CPPFLAGS@ @POSTGRESQL_CPPFLAGS@
case2107_LDFLAGS = @MYSQL_LDFLAGS@ @POSTGRESQL_LDFLAGS@
case2107_LDADD = ../../../src/xta/cpp/liblixtapp.la


