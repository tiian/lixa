#AM_CPPFLAGS = -DLIXA_MULTITHREAD=0 -I../../src/client -I../../src/common \
#	-I../../src/client/switch/lixa
AM_CPPFLAGS = -I../../src/client -I../../src/common \
	-I../../src/client/switch/lixa
AM_CFLAGS = -Wall
#AM_LDFLAGS = -lpthread @LIBLTDL@ @LDFLAGS_GMODULE@
LDADD = ../../src/common/liblixab.la ../../src/client/liblixac.la \
	../../src/client/switch/lixa/liblixamonkey.la
if COMPILE_POSTGRESQL_STUFF
  MAYBE_POSTGRESQL = case0037
endif
noinst_PROGRAMS = case0000 case0001 case0002 case0003 case0004 case0005 \
	case0006 case0007 case0008 case0009 case0010 case0011 case0012 \
	case0013 case0014 case0015 case0016 case0017 case0018 case0019 \
	case0020 case0021 case0022 case0023 case0024 case0025 case0026 \
	case0027 case0028 case0029 case0030 case0031 case0032 case0033 \
	case0034 case0035 case0036 $(MAYBE_POSTGRESQL)
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
case0033_SOURCES = case0033.c
case0034_SOURCES = case0034.c
case0035_SOURCES = case0035.c
case0036_SOURCES = case0036.c
case0037_SOURCES = case0037.c
case0037_CPPFLAGS = @POSTGRESQL_CPPFLAGS@ \
	-I../../src/client -I../../src/common \
	-I../../src/client/switch/postgresql
case0037_LDADD = ../../src/common/liblixab.la ../../src/client/liblixac.la \
        ../../src/client/switch/postgresql/liblixapq.la
