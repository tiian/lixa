--TEST--
LIXA pgsql integration - a simple transaction using LIXA and PostgreSQL (pgsql driver)
--SKIPIF--
<?php
if (!stristr(PHP_OS, "Linux")) die("skip this test is Linux platforms only");
if (!lixa_config_have_postgresql()) die("skip this test requires LIXA configured for PostgreSQL");
?>
--FILE--
<?php
	/* set LIXA profile environment variable */
	putenv("LIXA_PROFILE=PQL_STA");

	// open all the Resource Managers (PostgreSQL instance in this example)
	$rc = tx_open();
	print "tx_open(): $rc\n";

	/* retrieve PostgreSQL connection from LIXA Transaction Manager instead
	   of PostgreSQL directly: lixa// means:
	   database name=lixa
	   chooser type (pos/rmid)=none
	   id (0, 1, 2, ...)=none
	   and can be read as "retrieve the first available PostgreSQL 
	   connection established by the Transaction Manager using the current
	   LIXA profile" */
	$dbconn = pg_connect("dbname=lixa//") 
		or die("Could not connect: " . pg_last_error() . "\n");

	// start a new transaction coordinated by LIXA Transaction Manager
	$rc=tx_begin();
	print "tx_begin(): $rc\n"; 

	// remove all the rows from the table 
	$query = 'DELETE FROM authors';
	$result = pg_query($query) 
		or die("DELETE failed: " . pg_last_error() . "\n");
	print "DELETE FROM authors: OK\n";

	// check the table content
	$query = 'SELECT * FROM authors';
	$result = pg_query($query) 
		or die("Query failed: " . pg_last_error(). "\n");
	echo "Result set:\n";
	while ($row = pg_fetch_array($result, null, PGSQL_ASSOC))
		echo "id=" . $row['id'] . ", last_name=" . $row['last_name'] . 
			", first_name=" . $row['first_name'] . "\n";

	// commit the transaction (DELETE FROM...)
	$rc=tx_commit();
	print "tx_commit(): $rc\n";

	// start a new transaction coordinated by LIXA Transaction Manager
	$rc=tx_begin();
	print "tx_begin(): $rc\n"; 

	// check the table content
	$query = 'SELECT * FROM authors';
	$result = pg_query($query) 
		or die("Query failed: " . pg_last_error(). "\n");
	echo "Result set:\n";
	while ($row = pg_fetch_array($result, null, PGSQL_ASSOC))
		echo "id=" . $row['id'] . ", last_name=" . $row['last_name'] . 
			", first_name=" . $row['first_name'] . "\n";

	// populate the table 
	$query = "INSERT INTO authors VALUES(999, 'Ferrari', 'Christian')";
	$result = pg_query($query) 
		or die("Query failed: " . pg_last_error() . "\n");
	print "INSERT INTO authors: OK\n";

	// check the table content
	$query = 'SELECT * FROM authors';
	$result = pg_query($query) 
		or die("Query failed: " . pg_last_error(). "\n");
	echo "Result set:\n";
	while ($row = pg_fetch_array($result, null, PGSQL_ASSOC))
		echo "id=" . $row['id'] . ", last_name=" . $row['last_name'] . 
			", first_name=" . $row['first_name'] . "\n";

	// rollback the transaction (INSERT INTO...)
	$rc=tx_rollback();
	print "tx_rollback(): $rc\n";

	// check the table content
	$query = 'SELECT * FROM authors';
	$result = pg_query($query) 
		or die("Query failed: " . pg_last_error(). "\n");
	echo "Result set:\n";
	while ($row = pg_fetch_array($result, null, PGSQL_ASSOC))
		echo "id=" . $row['id'] . ", last_name=" . $row['last_name'] . 
			", first_name=" . $row['first_name'] . "\n";

	// start a new transaction coordinated by LIXA Transaction Manager
	$rc=tx_begin();
	print "tx_begin(): $rc\n"; 

	// populate the table 
	$query = "INSERT INTO authors VALUES(999, 'Ferrari', 'Christian')";
	$result = pg_query($query) 
		or die("Query failed: " . pg_last_error() . "\n");
	print "INSERT INTO authors: OK\n";

	// check the table content
	$query = 'SELECT * FROM authors';
	$result = pg_query($query) 
		or die("Query failed: " . pg_last_error(). "\n");
	echo "Result set:\n";
	while ($row = pg_fetch_array($result, null, PGSQL_ASSOC))
		echo "id=" . $row['id'] . ", last_name=" . $row['last_name'] . 
			", first_name=" . $row['first_name'] . "\n";

	// commit the transaction (DELETE FROM...)
	$rc=tx_commit();
	print "tx_commit(): $rc\n";

	// check the table content
	$query = 'SELECT * FROM authors';
	$result = pg_query($query) 
		or die("Query failed: " . pg_last_error(). "\n");
	echo "Result set:\n";
	while ($row = pg_fetch_array($result, null, PGSQL_ASSOC))
		echo "id=" . $row['id'] . ", last_name=" . $row['last_name'] . 
			", first_name=" . $row['first_name'] . "\n";

	// release resultset
	pg_free_result($result);

	/* release PHP handler for dbconn (it does not close database 
	   connection because it was opened by LIXA Transaction Manager */
	pg_close($dbconn);

	// close al the Resource Managers
	$rc = tx_close();
	print "tx_close(): $rc\n";
?>
--EXPECT--
tx_open(): 0
tx_begin(): 0
DELETE FROM authors: OK
Result set:
tx_commit(): 0
tx_begin(): 0
Result set:
INSERT INTO authors: OK
Result set:
id=999, last_name=Ferrari, first_name=Christian
tx_rollback(): 0
Result set:
tx_begin(): 0
INSERT INTO authors: OK
Result set:
id=999, last_name=Ferrari, first_name=Christian
tx_commit(): 0
Result set:
id=999, last_name=Ferrari, first_name=Christian
tx_close(): 0
