if COMPILE_POSTGRESQL_STUFF
  MAYBE_POSTGRESQL = CASE1037
endif
check_PROGRAMS = CASE1000 $(MAYBE_POSTGRESQL)
# COBOL program are not recognized as sources and are distributed as generic
# files
EXTRA_DIST = CASE1000.cob CASE1037.cob
# This rows are boring but they works around implicit autotools rules
nodist_CASE1000_SOURCES = CASE1000.c
nodist_CASE1037_SOURCES = CASE1037.c

AM_LDFLAGS = -Wl,--no-as-needed -lcob
COBCOPTS = -I ../../../src/client

SUFFIXES = .cob
.cob.o:
	$(COBC) $(COBCOPTS) -c -x $< 

LDADD = ../../../src/common/liblixab.la \
	../../../src/client/liblixac.la
CASE1037_LDADD = ../../../src/common/liblixab.la \
	../../../src/client/liblixac.la \
	../../../src/client/switch/postgresql/liblixapq.la

