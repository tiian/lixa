if COMPILE_MYSQL_STUFF
  MAYBE_MYSQL_SOURCES = MysqlXaResource.cpp
  MAYBE_MYSQL_CPPFLAGS = @MYSQL_CPPFLAGS@ \
	-I$(top_srcdir)/src/client/switch/mysql
  MAYBE_MYSQL_LIBADD = $(top_srcdir)/src/client/switch/mysql/liblixamy.la
endif
if COMPILE_POSTGRESQL_STUFF
  MAYBE_POSTGRESQL_SOURCES = PostgresqlXaResource.cpp
  MAYBE_POSTGRESQL_CPPFLAGS = @POSTGRESQL_CPPFLAGS@ \
	-I$(top_srcdir)/src/client/switch/postgresql
  MAYBE_POSTGRESQL_LIBADD = $(top_srcdir)/src/client/switch/postgresql/liblixapq.la
endif

dist_doc_DATA = README.md

includedir = $(prefix)/include/xta/cpp
include_HEADERS = Xta.hpp AcquiredXaResource.hpp Config.hpp Exception.hpp \
	MysqlXaResource.hpp NativeXaResource.hpp \
	PostgresqlXaResource.hpp Transaction.hpp TransactionManager.hpp \
	XaResource.hpp Xid.hpp
lib_LTLIBRARIES = liblixtapp.la
liblixtapp_la_SOURCES = Config.cpp Transaction.cpp TransactionManager.cpp \
	Xta.cpp NativeXaResource.cpp Xid.cpp \
	$(MAYBE_MYSQL_SOURCES) $(MAYBE_POSTGRESQL_SOURCES)
AM_CXXFLAGS = -Wall -fno-strict-aliasing
liblixtapp_la_CPPFLAGS = -I$(top_srcdir)/src/common -I$(top_srcdir)/src/client \
	-I$(top_srcdir)/src/xta \
	$(MAYBE_MYSQL_CPPFLAGS) $(MAYBE_POSTGRESQL_CPPFLAGS)
liblixtapp_la_LIBADD = $(top_srcdir)/src/xta/liblixta.la \
	$(MAYBE_MYSQL_LIBADD) $(MAYBE_POSTGRESQL_LIBADD) \
	@LDFLAGS_GLIB@ @LDFLAGS_XML2@
liblixtapp_la_LDFLAGS = -version-info @LT_LIXTA_CURRENT@:@LT_LIXTA_REVISION@:@LT_LIXTA_AGE@
