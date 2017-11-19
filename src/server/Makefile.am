noinst_HEADERS = ../common/lixa_trace.h server_config.h server_listener.h \
	server_manager.h server_messages.h server_recovery.h \
	server_reply.h server_thread_status.h server_status.h server_xa.h \
	server_xa_branch.h srvr_rcvr_tbl.h server_tpm.h server_trans_tbl.h
AM_CPPFLAGS = -DLIXA_MULTITHREAD=1 -I$(srcdir)/../common
AM_CFLAGS = -Wall -Wno-unused-but-set-variable
AM_LDFLAGS = @LDFLAGS_XML2@ @LDFLAGS_GLIB@ @LDFLAGS_GTHREAD@ -lpthread
sbin_PROGRAMS = lixad
lixad_LDADD = ../common/liblixab.la
lixad_SOURCES = lixad.c server_config.c server_listener.c server_manager.c \
	server_recovery.c server_reply.c server_thread_status.c \
	server_status.c server_xa.c server_xa_branch.c srvr_rcvr_tbl.c \
	server_tpm.c server_trans_tbl.c
