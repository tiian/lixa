<?php	
/*
 * Copyright (c) 2009-2012, Christian Ferrari <tiian@users.sourceforge.net>
 * All rights reserved.
 *
 * This file is part of LIXA.
 *
 * LIXA is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License version 2 as published
 * by the Free Software Foundation.
 *
 * LIXA is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with LIXA.  If not, see <http://www.gnu.org/licenses/>.
 */

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
