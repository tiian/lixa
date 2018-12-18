dist_noinst_DATA = case4100.java case4101.java

noinst_DATA = case4100.class case4101.class

SUFFIXES: .java .class

.java.class:
	$(JAVAC) -cp $(top_srcdir)/src/xta/java/xta.jar:@MYSQL_JDBC_JAR@:@POSTGRESQL_JDBC_JAR@ $<
