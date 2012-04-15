--TEST--
LIXA T.M. trivial commit - basic test: a trivial commit transaction
--SKIPIF--
<?php 
if (!stristr(PHP_OS, "Linux")) die("skip this test is Linux platforms only");
?>
--FILE--
<?php
	$rc = tx_open();
	print "tx_open(): $rc\n";
	$rc = tx_begin();
	print "tx_begin(): $rc\n";
	$rc = tx_commit();
	print "tx_commit(): $rc\n";
	tx_close();
	print "tx_close(): $rc\n";
?>
--EXPECT--
tx_open(): 0
tx_begin(): 0
tx_commit(): 0
tx_close(): 0
