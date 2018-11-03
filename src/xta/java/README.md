This directory contains the source code necessary to build XTA for Java

To generate the C function prototypes, use:

javah -o jni_wrapper.h -classpath xta.jar org.tiian.lixa.xta.Transaction

or 

javac -h . org/tiian/lixa/xta/Transaction.java

depending on Java version
