AM_CPPFLAGS = -DLIXA_MULTITHREAD=0 -I../../src/client -I../../src/common \
	-I../../src/client/switch/lixa
AM_CFLAGS = -Wall
#AM_LDFLAGS = -lpthread @LIBLTDL@ @LDFLAGS_GMODULE@
LDADD = ../../src/common/liblixab.la ../../src/client/liblixac.la \
	../../src/client/switch/lixa/liblixamonkey.la
noinst_PROGRAMS = case0000 case0001 case0002 case0003 case0004 case0005 \
	case0006 case0007 case0008 case0009 case0010
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
