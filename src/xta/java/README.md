This directory contains the source code necessary to build XTA for Java

To generate the C function prototypes related to class Transaction, use:

javah -o jni_wrapper.h -classpath xta.jar org.tiian.lixa.xta.Transaction

or 

javac -h . org/tiian/lixa/xta/Transaction.java

depending on Java version.


To retrieve the signature of Java methods, as required from JNI for class
XtaException, use:

javap -s org.tiian.lixa.xta.XtaException
