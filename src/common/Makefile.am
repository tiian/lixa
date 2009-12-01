include_HEADERS = xa.h
noinst_HEADERS = lixa_config.h lixa_defines.h lixa_errors.h lixa_trace.h lixa_xml_msg.h
lib_LTLIBRARIES = liblixab.la liblixabmt.la
liblixab_la_SOURCES = lixa_config.c lixa_errors.c lixa_trace.c lixa_xml_msg.c
liblixabmt_la_SOURCES = lixa_config.c lixa_errors.c lixa_trace.c lixa_xml_msg.c
AM_CFLAGS = -Wall
liblixab_la_LDFLAGS = -version-info @LT_LIXAB_CURRENT@:@LT_LIXAB_REVISION@:@LT_LIXAB_AGE@
liblixabmt_la_LDFLAGS = -lpthread -version-info @LT_LIXAB_CURRENT@:@LT_LIXAB_REVISION@:@LT_LIXAB_AGE@
