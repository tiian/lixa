--TEST--
LIXA pgsql mysqli integration - a simple transaction using LIXA, PostgreSQL (pgsql driver) and MySQL (mysqli driver)
--SKIPIF--
<?php
if (!stristr(PHP_OS, "Linux")) die("skip this test is Linux platforms only");
if (!lixa_config_have_postgresql()) die("skip this test requires LIXA configured for PostgreSQL");
if (!lixa_config_have_mysql()) die("skip this test requires LIXA configured for MySQL with mysqli driver");
?>
--FILE--
<?php
	/* set LIXA profile environment variable */
	putenv("LIXA_PROFILE=MYS_STA_PQL_STA");

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
	print "MYSQL: DELETE FROM authors: OK\n";
	// remove all the rows from PostgreSQL table 
	$query = 'DELETE FROM authors';
	$result = pg_query($query) 
		or die("DELETE failed: " . pg_last_error() . "\n");
	print "PostgreSQL: DELETE FROM authors: OK\n";

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
	print "MYSQL: INSERT INTO authors: OK\n";
	// populate the table (PostgreSQL)
	$query = "INSERT INTO authors VALUES(999, 'Ferrari', 'Christian')";
	$result = pg_query($query) 
		or die("Query failed: " . pg_last_error() . "\n");
	print "PostgreSQL: INSERT INTO authors: OK\n";

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
	print "MYSQL: INSERT INTO authors: OK\n";
	// populate the table (PostgreSQL)
	$query = "INSERT INTO authors VALUES(999, 'Ferrari', 'Christian')";
	$result = pg_query($query) 
		or die("Query failed: " . pg_last_error() . "\n");
	print "PostgreSQL: INSERT INTO authors: OK\n";
	
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
--EXPECT--
tx_open(): 0
tx_begin(): 0
DELETE FROM authors;
MYSQL: DELETE FROM authors: OK
PostgreSQL: DELETE FROM authors: OK
Result set (MySQL):
Result set (PostgreSQL):
tx_commit(): 0
tx_begin(): 0
Result set (MySQL):
Result set (PostgreSQL):
INSERT INTO authors...
MYSQL: INSERT INTO authors: OK
PostgreSQL: INSERT INTO authors: OK
Result set (MySQL):
id=969, last_name=Ferrari, first_name=Christian
Result set (PostgreSQL):
id=999, last_name=Ferrari, first_name=Christian
tx_rollback(): 0
Result set (MySQL):
Result set (PostgreSQL):
tx_begin(): 0
INSERT INTO authors...
MYSQL: INSERT INTO authors: OK
PostgreSQL: INSERT INTO authors: OK
Result set (MySQL):
id=969, last_name=Ferrari, first_name=Christian
Result set (PostgreSQL):
id=999, last_name=Ferrari, first_name=Christian
tx_commit(): 0
Result set (MySQL):
id=969, last_name=Ferrari, first_name=Christian
Result set (PostgreSQL):
id=999, last_name=Ferrari, first_name=Christian
tx_close(): 0

