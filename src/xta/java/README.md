This directory contains the source code necessary to build XTA for Java

To generate the C function prototypes related to class Transaction, use:

javah -o jni_wrapper.h -classpath xta.jar org.tiian.lixa.xta.Transaction

or 

javac -h . org/tiian/lixa/xta/Transaction.java

depending on Java version.


To retrieve the signature of Java methods, as required from JNI for class
XtaException, use:

javap -s org.tiian.lixa.xta.XtaException

Best practices for using the Java Native Interface: https://www.ibm.com/developerworks/library/j-jni/index.html

https://docs.oracle.com/javase/7/docs/technotes/guides/jni/spec/functions.html

https://www.ibm.com/support/knowledgecenter/en/SSYKE2_7.1.0/com.ibm.java.aix.71.doc/user/packed_jni_interface.html

https://docs.oracle.com/javase/8/docs/technotes/guides/jni/spec/invocation.html

https://docs.oracle.com/javase/9/docs/specs/jni/functions.html#version-information

A code example that shows XA Java usage: https://www.programcreek.com/java-api-examples/?code=bragex/the-vigilantes/the-vigilantes-master/Is-202/Sprint_1/Last%20ned%20denne/mysql-connector-java-5.1.44/src/testsuite/regression/ConnectionRegressionTest.java

