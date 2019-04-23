if COMPILE_XTA_JAVA
  MAYBE_XTA = case4100.class case4103.class case4105.class case4110.class
if COMPILE_MYSQL_STUFF
if COMPILE_POSTGRESQL_STUFF
  MAYBE_XTA_MYPOST_SQL = case4101.class case4107.class
if COMPILE_ORACLE_STUFF
  MAYBE_XTA_MYPOSTORACLE = case4104.class case4106.class case4108.class
endif
endif
endif
if COMPILE_ORACLE_STUFF
  MAYBE_XTA_ORACLE = case4102.class
endif
endif

dist_noinst_DATA = case4100.java case4101.java case4102.java case4103.java \
	case4104.java case4105.java case4106.java case4107.java \
	case4108.java case4110.java

check_DATA = $(MAYBE_XTA) $(MAYBE_XTA_MYPOST_SQL) $(MAYBE_XTA_ORACLE) \
	$(MAYBE_XTA_MYPOSTORACLE)

SUFFIXES: .java .class

.java.class:
	$(JAVAC) -cp $(top_srcdir)/src/xta/java/xta.jar:@MYSQL_JDBC_JAR@:@POSTGRESQL_JDBC_JAR@:@ORACLE_JDBC_JAR@ $<
