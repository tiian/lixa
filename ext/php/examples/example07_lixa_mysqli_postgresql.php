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

	/* open all the Resource Managers (MySQL & PostgreSQL instances 
	   in this example) */
	$rc=tx_open();
	print "tx_open(): $rc\n"; 

	/* retrieve database connections from LIXA Transaction Manager instead
	   of MySQL/PostgreSQL directly: lixa// means:
	   database name=lixa
	   chooser type (pos/rmid)=none
	   id (0, 1, 2, ...)=none
	   and can be read as "retrieve the first available MySQL/PostgreSQL
	   connection established by the Transaction Manager
	   using the current LIXA profile" */
	// retrieve first available MySQL connection
	$mysqli = new mysqli("localhost", "lixa", "", "lixa//", 3306);
	if ($mysqli->connect_errno)
		echo "Failed to connect to MySQL: (" . $mysqli->connect_errno .
			") " . $mysqli->connect_error . "\n";
	// retrieve first available PostgreSQL connection
	$dbconn = pg_connect("dbname=lixa//") 
		or die("Could not connect: " . pg_last_error() . "\n");

	// start a new transaction coordinated by LIXA
	$rc=tx_begin();
	print "tx_begin(): $rc\n"; 

	// remove all the rows from MySQL table
	print "DELETE FROM authors;\n";
	if (!$mysqli->query("DELETE FROM authors;"))
		echo "DELETE failed: (" . $mysqli->errno . ") " . 
			$mysqli->error;
	// remove all the rows from PostgreSQL table 
	$query = 'DELETE FROM authors';
	$result = pg_query($query) 
		or die("DELETE failed: " . pg_last_error() . "\n");

	// check the table content (MySQL)
	$mysqli->real_query("SELECT id,last_name,first_name FROM authors ORDER BY id DESC;");
	$res = $mysqli->use_result();
	echo "Result set (MySQL):\n";
	while ($row = $res->fetch_assoc())
		echo "id=" . $row['id'] . ", last_name=" . $row['last_name'] .
		", first_name=" . $row['first_name'] . "\n";
	// check the table content (PostgreSQL)
	$query = 'SELECT * FROM authors';
	$result = pg_query($query) 
		or die("Query failed: " . pg_last_error(). "\n");
	echo "Result set (PostgreSQL):\n";
	while ($row = pg_fetch_array($result, null, PGSQL_ASSOC))
		echo "id=" . $row['id'] . ", last_name=" . $row['last_name'] . 
			", first_name=" . $row['first_name'] . "\n";

	// commit the transaction (DELETE FROM ...)
	$rc=tx_commit();
	print "tx_commit(): $rc\n"; 

	// start a new transaction coordinated by LIXA
	$rc=tx_begin();
	print "tx_begin(): $rc\n"; 

	// check the table content (MySQL)
	$mysqli->real_query("SELECT id,last_name,first_name FROM authors ORDER BY id DESC;");
	$res = $mysqli->use_result();
	echo "Result set (MySQL):\n";
	while ($row = $res->fetch_assoc())
		echo "id=" . $row['id'] . ", last_name=" . $row['last_name'] . ", first_name=" . $row['first_name'] . "\n";
	// check the table content (PostgreSQL)
	$query = 'SELECT * FROM authors';
	$result = pg_query($query) 
		or die("Query failed: " . pg_last_error(). "\n");
	echo "Result set (PostgreSQL):\n";
	while ($row = pg_fetch_array($result, null, PGSQL_ASSOC))
		echo "id=" . $row['id'] . ", last_name=" . $row['last_name'] . 
			", first_name=" . $row['first_name'] . "\n";

	// populate the table (MySQL)
	print "INSERT INTO authors...\n";
	if (!$mysqli->query("INSERT INTO authors VALUES(969,'Ferrari','Christian');"))
		echo "INSERT failed: (" . $mysqli->errno . ") " . 
			$mysqli->error;
	// populate the table (PostgreSQL)
	$query = "INSERT INTO authors VALUES(999, 'Ferrari', 'Christian')";
	$result = pg_query($query) 
		or die("Query failed: " . pg_last_error() . "\n");

	// check the table content (MySQL)
	$mysqli->real_query("SELECT id,last_name,first_name FROM authors ORDER BY id DESC;");
	$res = $mysqli->use_result();
	echo "Result set (MySQL):\n";
	while ($row = $res->fetch_assoc())
		echo "id=" . $row['id'] . ", last_name=" . $row['last_name'] .
			", first_name=" . $row['first_name'] . "\n";
	// check the table content (PostgreSQL)
	$query = 'SELECT * FROM authors';
	$result = pg_query($query) 
		or die("Query failed: " . pg_last_error(). "\n");
	echo "Result set (PostgreSQL):\n";
	while ($row = pg_fetch_array($result, null, PGSQL_ASSOC))
		echo "id=" . $row['id'] . ", last_name=" . $row['last_name'] . 
			", first_name=" . $row['first_name'] . "\n";


	// rollback the transaction (INSERT INTO ...)
	$rc=tx_rollback();
	print "tx_rollback(): $rc\n"; 

	// check the table content (MySQL)
	$mysqli->real_query("SELECT id,last_name,first_name FROM authors ORDER BY id DESC;");
	$res = $mysqli->use_result();
	echo "Result set (MySQL):\n";
	while ($row = $res->fetch_assoc())
		echo "id=" . $row['id'] . ", last_name=" . $row['last_name'] .
			", first_name=" . $row['first_name'] . "\n";
	// check the table content (PostgreSQL)
	$query = 'SELECT * FROM authors';
	$result = pg_query($query) 
		or die("Query failed: " . pg_last_error(). "\n");
	echo "Result set (PostgreSQL):\n";
	while ($row = pg_fetch_array($result, null, PGSQL_ASSOC))
		echo "id=" . $row['id'] . ", last_name=" . $row['last_name'] . 
			", first_name=" . $row['first_name'] . "\n";

	// start a new transaction coordinated by LIXA
	$rc=tx_begin();
	print "tx_begin(): $rc\n"; 

	// populate the table (MySQL)
	print "INSERT INTO authors...\n";
	if (!$mysqli->query("INSERT INTO authors VALUES(969,'Ferrari','Christian');"))
		echo "INSERT failed: (" . $mysqli->errno . ") " .
			$mysqli->error;
	// populate the table (PostgreSQL)
	$query = "INSERT INTO authors VALUES(999, 'Ferrari', 'Christian')";
	$result = pg_query($query) 
		or die("Query failed: " . pg_last_error() . "\n");
	
	// check the table content (MySQL)
	$mysqli->real_query("SELECT id,last_name,first_name FROM authors ORDER BY id DESC;");
	$res = $mysqli->use_result();
	echo "Result set (MySQL):\n";
	while ($row = $res->fetch_assoc())
		echo "id=" . $row['id'] . ", last_name=" . $row['last_name'] .
			", first_name=" . $row['first_name'] . "\n";
	// check the table content (PostgreSQL)
	$query = 'SELECT * FROM authors';
	$result = pg_query($query) 
		or die("Query failed: " . pg_last_error(). "\n");
	echo "Result set (PostgreSQL):\n";
	while ($row = pg_fetch_array($result, null, PGSQL_ASSOC))
		echo "id=" . $row['id'] . ", last_name=" . $row['last_name'] . 
			", first_name=" . $row['first_name'] . "\n";

	// commit the transaction (INSERT INTO ...)
	$rc=tx_commit();
	print "tx_commit(): $rc\n"; 

	// check the table content (MySQL)
	$mysqli->real_query("SELECT id,last_name,first_name FROM authors ORDER BY id DESC;");
	$res = $mysqli->use_result();
	echo "Result set (MySQL):\n";
	while ($row = $res->fetch_assoc())
		echo "id=" . $row['id'] . ", last_name=" . $row['last_name'] .
			", first_name=" . $row['first_name'] . "\n";
	// check the table content (PostgreSQL)
	$query = 'SELECT * FROM authors';
	$result = pg_query($query) 
		or die("Query failed: " . pg_last_error(). "\n");
	echo "Result set (PostgreSQL):\n";
	while ($row = pg_fetch_array($result, null, PGSQL_ASSOC))
		echo "id=" . $row['id'] . ", last_name=" . $row['last_name'] . 
			", first_name=" . $row['first_name'] . "\n";

	// release resultset
	pg_free_result($result);

	/* release PHP handler for dbconn (it does not close database 
	   connection because it was opened by LIXA Transaction Manager */
	pg_close($dbconn);

	// close all the Resource Managers
	$rc=tx_close();
	print "tx_close(): $rc\n"; 
?>
