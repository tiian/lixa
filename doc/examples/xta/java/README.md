# XTA example Application Programs for Java language #

This directory contains examples related to the usage of XTA: XA Transactional
API.

All the examples require the availability of LIXA state server: a lixad daemon
must be up and running.

See [LIXA Reference Guide Manual](http://www.tiian.org/lixa/manuals/html/index.html), chapter "Developing Application Programs using XTA (XA Transaction API) interface", for detailed instructions.

## "Single Application" Pattern ##

Program *ExampleXtaSA11.java* requires MySQL (or MariaDB) and PostgreSQL to
work.
After proper installation of LIXA, you can compile the program with the
following command (jar classpath must be changed as necessary):

`javac -cp /opt/lixa/share/lixa/java/xta.jar:/usr/share/java/mysql.jar:/opt/postgresql/postgresql.jar ExampleXtaSA31.java`

and execute it with this command:

`java -Djava.library.path=/opt/lixa/lib -cp /opt/lixa/share/lixa/java/xta.jar:/usr/share/java/mysql.jar:/opt/postgresql/postgresql.jar:. ExampleXtaSA31 1 1`

## "Multiple Applications, Consecutive Calls" Pattern ##

Program *ExampleXtaMACC11.java* requires Oracle Client to work.
After proper installation of LIXA, you can compile the program with the
following command (jar classpath must be changed as necessary):

`javac -cp /opt/lixa/share/lixa/java/xta.jar:/opt/oracle/OJDBC-Full/ojdbc7.jar ExampleXtaMACC31.java`

Oracle JDBC thin client requires random number generator, depending on rngd
availability, execute the above command with these commands:

# if rngd is not running use this
`java -Djava.library.path=/opt/lixa/lib -Djava.security.egd=file:/dev/./urandom -cp /opt/lixa/share/lixa/java/xta.jar:/opt/oracle/OJDBC-Full/ojdbc7.jar:. ExampleXtaMACC31 1 1 1 xid.fifo`
`java -Djava.library.path=/opt/lixa/lib -Djava.security.egd=file:/dev/./urandom -cp /opt/lixa/share/lixa/java/xta.jar:/opt/oracle/OJDBC-Full/ojdbc7.jar:. ExampleXtaMACC31 1 1 0 xid.fifo`
# if rngd is RUNNING use this
`java -Djava.library.path=/opt/lixa/lib -cp /opt/lixa/share/lixa/java/xta.jar:/opt/oracle/OJDBC-Full/ojdbc7.jar:. ExampleXtaMACC31 1 1 1 xid.fifo`
`java -Djava.library.path=/opt/lixa/lib -cp /opt/lixa/share/lixa/java/xta.jar:/opt/oracle/OJDBC-Full/ojdbc7.jar:. ExampleXtaMACC31 1 1 0 xid.fifo`

## "Multiple Applications, Concurrent Branches/Pseudo Synchronous" Pattern ##

Program *ExampleXtaMACBPS11.java* requires MySQL (or MariaDB) to work.
Program *ExampleXtaMACBPS12.java* requires PostgreSQL to work.
After proper installation of LIXA, you can compile the programs with the
following commands:

`javac -cp /opt/lixa/share/lixa/java/xta.jar:/usr/share/java/mysql.jar ExampleXtaMACBPS31.java`
`javac -cp /opt/lixa/share/lixa/java/xta.jar:/usr/share/java/postgresql.jar ExampleXtaMACBPS32.java`

and execute them with these commands:

`java -Djava.library.path=/opt/lixa/lib -cp /opt/lixa/share/lixa/java/xta.jar:/usr/share/java/mysql.jar:. ExampleXtaMACBPS31 1 1 sup2sub sub2sup`
`java -Djava.library.path=/opt/lixa/lib -cp /opt/lixa/share/lixa/java/xta.jar:/usr/share/java/postgresql.jar:. ExampleXtaMACBPS32 1 1 sup2sub sub2sup`

## "Multiple Applications, Concurrent Branches/Pseudo Asynchronous" Pattern ##

Program *ExampleXtaMACBPA11.java* requires MySQL (or MariaDB) to work.
Program *ExampleXtaMACBPA12.java* requires PostgreSQL to work.
After proper installation of LIXA, you can compile the programs with the
following commands:

`javac -cp /opt/lixa/share/lixa/java/xta.jar:/usr/share/java/mysql.jar ExampleXtaMACBPA31.java`
`javac -cp /opt/lixa/share/lixa/java/xta.jar:/usr/share/java/postgresql.jar ExampleXtaMACBPA32.java`

and execute them with these commands:

`java -Djava.library.path=/opt/lixa/lib -cp /opt/lixa/share/lixa/java/xta.jar:/usr/share/java/mysql.jar:. ExampleXtaMACBPA31 1 1 sup2sub sub2sup`
`java -Djava.library.path=/opt/lixa/lib -cp /opt/lixa/share/lixa/java/xta.jar:/usr/share/java/postgresql.jar:. ExampleXtaMACBPA32 1 1 sup2sub sub2sup`

### The following links are useful to debug Oracle JDBC thin client behavior ###

https://docs.oracle.com/cd/E11882_01/java.112/e12265/connect.htm#CHDDCICA

https://docs.oracle.com/database/121/JJDBC/xadistra.htm#JJDBC28878

Here is a sample file that can be used for logging:

.level=SEVERE
oracle.jdbc.level=ALL
oracle.jdbc.handlers=java.util.logging.FileHandler
java.util.logging.FileHandler.level=ALL
java.util.logging.FileHandler.pattern=jdbc.log
java.util.logging.FileHandler.count=1
java.util.logging.FileHandler.formatter=java.util.logging.SimpleFormatter
