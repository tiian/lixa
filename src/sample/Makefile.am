if COMPILE_ORACLE_STUFF
  MAYBE_ORACLE = oracle
endif
SUBDIRS = $(MAYBE_ORACLE)

AM_CPPFLAGS = -I$(srcdir)/../client
AM_CFLAGS = -Wall
AM_LDFLAGS = -lpthread
LDADD = ../client/liblixacmt.la
hello_SOURCES = hello.c
noinst_PROGRAMS = hello
