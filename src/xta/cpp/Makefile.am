if COMPILE_MYSQL_STUFF
  MAYBE_MYSQL_SOURCES = MysqlXaResource.cpp
  MAYBE_MYSQL_CPPFLAGS = @MYSQL_CPPFLAGS@ \
	-I$(top_srcdir)/src/client/switch/mysql
endif
if COMPILE_POSTGRESQL_STUFF
  MAYBE_POSTGRESQL_SOURCES = PostgresqlXaResource.cpp
  MAYBE_POSTGRESQL_CPPFLAGS = @POSTGRESQL_CPPFLAGS@ \
	-I$(top_srcdir)/src/client/switch/postgresql
endif

dist_doc_DATA = README.md

includedir = $(prefix)/include/xta/cpp
include_HEADERS = Xta.hpp AcquiredXaResource.hpp MysqlXaResource.hpp \
	NativeXaResource.hpp \
	PostgresqlXaResource.hpp Transaction.hpp TransactionManager.hpp \
	XaResource.hpp Xid.hpp
lib_LTLIBRARIES = liblixtapp.la
liblixtapp_la_SOURCES = Transaction.cpp TransactionManager.cpp Xta.cpp \
	$(MAYBE_MYSQL_SOURCES) $(MAYBE_POSTGRESQL_SOURCES)
AM_CXXFLAGS = -Wall -fno-strict-aliasing
liblixtapp_la_CPPFLAGS = -I$(top_srcdir)/src/common -I$(top_srcdir)/src/client \
	-I$(top_srcdir)/src/xta \
	$(MAYBE_MYSQL_CPPFLAGS) $(MAYBE_POSTGRESQL_CPPFLAGS)
liblixtapp_la_LIBADD = $(top_srcdir)/src/xta/liblixta.la \
	$(MAYBE_MYSQL_LIBADD) $(MAYBE_POSTGRESQL_LIBADD) \
	@LDFLAGS_GLIB@ @LDFLAGS_XML2@
liblixtapp_la_LDFLAGS = -version-info @LT_LIXTA_CURRENT@:@LT_LIXTA_REVISION@:@LT_LIXTA_AGE@
