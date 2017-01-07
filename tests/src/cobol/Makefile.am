if COMPILE_POSTGRESQL_STUFF
  MAYBE_POSTGRESQL = CASE1037
endif
check_PROGRAMS = CASE1000 $(MAYBE_POSTGRESQL)

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

