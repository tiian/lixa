--TEST--
LIXA T.M. availability - basic test: the T.M. is up and running
--SKIPIF--
<?php 
if (!stristr(PHP_OS, "Linux")) die("skip this test is Linux platforms only");
?>
--FILE--
<?php
	$rc = tx_open();
	print "tx_open(): $rc\n";
	tx_close();
	print "tx_close(): $rc\n";
?>
--EXPECT--
tx_open(): 0
tx_close(): 0
