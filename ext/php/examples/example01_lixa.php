<?php	
	/* set PHPRC to /home/tiian/src/swig to pick-up php.ini when using
	   /opt/php/bin/php to run this script; it's not necessary if using
	   /opt/php/bin/php-cgi */
	/* set LIXA_PROFILE=MYS_STA_PQL_STA */
	/* run me using /opt/php/bin/php -c php.ini foo.php */
	/*
	include("lixa.php");
	*/
	print "Hello world!\n";
	$rc=tx_open();
	print "tx_open() --> $rc\n"; 
	$rc=tx_begin();
	print "tx_begin() --> $rc\n"; 
	$rc=tx_commit();
	print "tx_commit() --> $rc\n"; 
	$rc=tx_close();
	print "tx_close() --> $rc\n"; 
	$rc=TX_OK;
	print "TX_OK=$rc\n";
?>
