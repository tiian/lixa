javac -cp /opt/lixa/share/lixa/java/xta.jar:/usr/share/java/mysql.jar:/usr/share/java/postgresql.jar ExampleXtaSA31.java

java -Djava.library.path=/opt/lixa/lib -cp /opt/lixa/share/lixa/java/xta.jar:/usr/share/java/mysql.jar:/usr/share/java/postgresql.jar:. ExampleXtaSA31 1 1

# Without UCP
javac -cp /opt/lixa/share/lixa/java/xta.jar:/opt/oracle/OJDBC-Full/ojdbc7.jar ExampleXtaMACC31.java
# if rngd is not running use this
java -Djava.library.path=/opt/lixa/lib -Djava.security.egd=file:/dev/./urandom -cp /opt/lixa/share/lixa/java/xta.jar:/opt/oracle/OJDBC-Full/ojdbc7.jar:. ExampleXtaMACC31 1 1 1 xid.fifo
java -Djava.library.path=/opt/lixa/lib -Djava.security.egd=file:/dev/./urandom -cp /opt/lixa/share/lixa/java/xta.jar:/opt/oracle/OJDBC-Full/ojdbc7.jar:. ExampleXtaMACC31 1 1 0 xid.fifo
# if rngd is RUNNING use this
java -Djava.library.path=/opt/lixa/lib -cp /opt/lixa/share/lixa/java/xta.jar:/opt/oracle/OJDBC-Full/ojdbc7.jar:. ExampleXtaMACC31 1 1 1 xid.fifo
java -Djava.library.path=/opt/lixa/lib -cp /opt/lixa/share/lixa/java/xta.jar:/opt/oracle/OJDBC-Full/ojdbc7.jar:. ExampleXtaMACC31 1 1 0 xid.fifo


javac -cp /opt/lixa/share/lixa/java/xta.jar:/usr/share/java/mysql.jar ExampleXtaMACBPS31.java
javac -cp /opt/lixa/share/lixa/java/xta.jar:/usr/share/java/postgresql.jar ExampleXtaMACBPS32.java

java -Djava.library.path=/opt/lixa/lib -cp /opt/lixa/share/lixa/java/xta.jar:/usr/share/java/mysql.jar:. ExampleXtaMACBPS31 1 1 sup2sub sub2sup
java -Djava.library.path=/opt/lixa/lib -cp /opt/lixa/share/lixa/java/xta.jar:/usr/share/java/postgresql.jar:. ExampleXtaMACBPS32 1 1 sup2sub sub2sup


javac -cp /opt/lixa/share/lixa/java/xta.jar:/usr/share/java/mysql.jar ExampleXtaMACBPA31.java
javac -cp /opt/lixa/share/lixa/java/xta.jar:/usr/share/java/postgresql.jar ExampleXtaMACBPA32.java

java -Djava.library.path=/opt/lixa/lib -cp /opt/lixa/share/lixa/java/xta.jar:/usr/share/java/mysql.jar:. ExampleXtaMACBPA31 1 1 sup2sub sub2sup
java -Djava.library.path=/opt/lixa/lib -cp /opt/lixa/share/lixa/java/xta.jar:/usr/share/java/postgresql.jar:. ExampleXtaMACBPA32 1 1 sup2sub sub2sup

https://docs.oracle.com/cd/E11882_01/java.112/e12265/connect.htm#CHDDCICA

https://docs.oracle.com/database/121/JJDBC/xadistra.htm#JJDBC28878

https://docs.oracle.com/cd/A97335_02/apps.102/a83724/xadistr4.htm


