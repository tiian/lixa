dist_noinst_DATA = case4100.java case4101.java case4102.java case4103.java \
	case4104.java case4105.java case4106.java case4107.java \
	case4108.java

noinst_DATA = case4100.class case4101.class case4102.class case4103.class \
	case4104.class case4105.class case4106.class case4107.class \
	case4108.class

SUFFIXES: .java .class

.java.class:
	$(JAVAC) -cp $(top_srcdir)/src/xta/java/xta.jar:@MYSQL_JDBC_JAR@:@POSTGRESQL_JDBC_JAR@:@ORACLE_JDBC_JAR@ $<
