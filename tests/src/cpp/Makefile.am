if COMPILE_XTA_CPP
if COMPILE_MYSQL_STUFF
if COMPILE_POSTGRESQL_STUFF
  MAYBE_XTA_SRC1 = case2100
endif
endif
endif

check_PROGRAMS = $(MAYBE_XTA_SRC1)
case2100_SOURCES = case2100.cpp
case2100_CPPFLAGS = -I../../../src/client -I../../../src/common \
	@MYSQL_CPPFLAGS@ @POSTGRESQL_CPPFLAGS@
case2100_LDFLAGS = @MYSQL_LDFLAGS@ @POSTGRESQL_LDFLAGS@
case2100_LDADD = ../../../src/xta/cpp/liblixtapp.la


