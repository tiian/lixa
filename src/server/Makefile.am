include_HEADERS = ../common/lixa_trace.h server_config.h server_listener.h server_manager.h server_status.h
AM_CPPFLAGS = -I$(srcdir)/../common
AM_CFLAGS = -Wall
AM_LDFLAGS = -lxml2
LDADD = ../common/liblixacommon.a
bin_PROGRAMS = lixad
lixad_SOURCES = main.c server_config.c server_listener.c server_manager.c
