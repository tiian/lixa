<?php	
	print "Trying to open all the Resource Managers with tx_open()...\n";
	$rc=tx_open();
	if (TX_OK != $rc) 
		exit("tx_open returned rc=$rc, can't going on!\n");
	
	print "Trying to start a new transaction with tx_begin()...\n";	
	$rc=tx_begin();
	if (TX_OK != $rc) 
		exit("tx_begin returned rc=$rc, can't going on!\n");

	print "Trying to commit the transaction with tx_commit()...\n";	
	$rc=tx_commit();
	if (TX_OK != $rc) 
		exit("tx_commit returned rc=$rc, can't going on!\n");

	print "Trying to close all the Resource Managers with tx_close()...\n";
	$rc=tx_close();
	if (TX_OK != $rc) 
		exit("tx_close returned rc=$rc, can't going on!\n");

	print "Execution completed!\n";
?>
