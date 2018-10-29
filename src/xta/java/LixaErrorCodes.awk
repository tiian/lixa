BEGIN { 
	print "package  org.tiian.lixa.xta;"
	print "/**"
	print " * This class contains the constants necessary to map the"
	print " * codes returned by the C native functions wrapped by the JNI"
	print " * methods."
	print " */"
	print "public class ErrorCodes {" 
}
/#define.*LIXA_/	{ print "\tpublic final static int " $2 " = " $3 ";" }
END   {
	print "\t/**"
	print "\t * Retrieve the text associated to a LIXA code"
	print "\t * @param code is the code returned by the C native functions"
	print "\t * @return a string with a human readable description"
	print "\t */"
	print "\tpublic native static String getText(int code);"
	print "}"
}
