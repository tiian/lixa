noinst_HEADERS = lixa_common_status.h lixa_config.h lixa_defines.h \
		lixa_errors.h lixa_inst_conf.h lixa_syslog.h \
		lixa_trace.h lixa_utils.h lixa_xml_msg.h \
		lixa_xml_msg_deserialize.h lixa_xml_msg_serialize.h \
		lixa_xml_msg_trace.h xa.h
lib_LTLIBRARIES = liblixab.la liblixabmt.la
liblixab_la_SOURCES = lixa_common_status.c lixa_config.c lixa_errors.c \
		lixa_inst_conf.c lixa_utils.c \
		lixa_trace.c lixa_xml_msg.c lixa_xml_msg_deserialize.c \
		lixa_xml_msg_serialize.c lixa_xml_msg_trace.c
liblixabmt_la_SOURCES = lixa_common_status.c lixa_config.c lixa_errors.c \
		lixa_inst_conf.c lixa_utils.c \
		lixa_trace.c lixa_xml_msg.c lixa_xml_msg_deserialize.c \
		lixa_xml_msg_serialize.c lixa_xml_msg_trace.c
AM_CFLAGS = -Wall
liblixab_la_LDFLAGS = -luuid -version-info @LT_LIXAB_CURRENT@:@LT_LIXAB_REVISION@:@LT_LIXAB_AGE@
liblixabmt_la_LDFLAGS = -lpthread -luuid -version-info @LT_LIXAB_CURRENT@:@LT_LIXAB_REVISION@:@LT_LIXAB_AGE@
