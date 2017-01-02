check_PROGRAMS = CASE1000.xx

CASE1000_SOURCE = CASE1000.c
COBOL_CPPFLAGS = -I../../../src/client
.cbl.c:
	$(COBC) $(COBOL_CPPFLAGS) -C $< 

#% : %.cbl
#	$(COBC) -x $(lixa-config -f -l) $<


