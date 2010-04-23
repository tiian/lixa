lib_LTLIBRARIES = liblixamonkey.la \
	switch_lixa_monkeyrm_dynreg.la \
	switch_lixa_monkeyrm_stareg.la \
	switch_lixa_dummyrm.la \
	switch_lixa_randomrm.la 
liblixamonkey_la_SOURCES = liblixamonkey.c
switch_lixa_monkeyrm_dynreg_la_SOURCES = lixa_monkeyrm_dynreg.c
switch_lixa_monkeyrm_stareg_la_SOURCES = lixa_monkeyrm_stareg.c
switch_lixa_dummyrm_la_SOURCES = lixa_dummyrm.c
switch_lixa_randomrm_la_SOURCES = lixa_randomrm.c
AM_CPPFLAGS = -I$(srcdir)/../../../common
AM_CFLAGS = -Wall
AM_LDFLAGS = -module
liblixamonkey_la_LDFLAGS =
switch_lixa_monkeyrm_dynreg_la_LIBADD = liblixamonkey.la
switch_lixa_monkeyrm_stareg_la_LIBADD = liblixamonkey.la
