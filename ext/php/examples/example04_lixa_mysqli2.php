<?php	
	/* open all the Resource Managers (MySQL instance in this example) */
	/* export LIXA_PROFILE=MYS_STA_MYS_STA */
	$rc=tx_open();
	print "tx_open(): $rc\n"; 

	/* retrieve MySQL connections from LIXA Transaction Manager instead
	   of MySQL directly: lixa// means:
	   database name=lixa
	   chooser type (pos/rmid)=none
	   id (0, 1, 2, ...)=none
	   and can be read as "retrieve the first available MySQL connection
	   established by the Transaction Manager using the current
	   LIXA profile */
	$mysqli = new mysqli("localhost", "lixa", "", "lixa/pos/2", 3306);
	if ($mysqli->connect_errno)
		echo "Failed to connect to MySQL: (" . $mysqli->connect_errno . ") " . $mysqli->connect_error . "\n";
	$mysqli2 = new mysqli("localhost", "lixa", "", "lixa/pos/1", 3306);
	if ($mysqli2->connect_errno)
		echo "Failed to connect to MySQL2: (" . $mysqli2->connect_errno . ") " . $mysqli2->connect_error . "\n";

	/* start a new transaction coordinated by LIXA */
	$rc=tx_begin();
	print "tx_begin(): $rc\n"; 

	/* remove all the rows from the tables */
	print "DELETE FROM authors;\n";
	if (!$mysqli->query("DELETE FROM authors;"))
		echo "DELETE failed: (" . $mysqli->errno . ") " . $mysqli->error;
	print "DELETE FROM authors; (2)\n";
	if (!$mysqli2->query("DELETE FROM authors;"))
		echo "DELETE (2) failed: (" . $mysqli2->errno . ") " . $mysqli2->error;

	/* check the content of the tables */	
	$mysqli->real_query("SELECT id,last_name,first_name FROM authors ORDER BY id DESC;");
	$res = $mysqli->use_result();
	echo "Result set:\n";
	while ($row = $res->fetch_assoc())
		echo "id=" . $row['id'] . ", last_name=" . $row['last_name'] . ", first_name=" . $row['first_name'] . "\n";
	$mysqli2->real_query("SELECT id,last_name,first_name FROM authors ORDER BY id DESC;");
	$res = $mysqli2->use_result();
	echo "Result set (2):\n";
	while ($row = $res->fetch_assoc())
		echo "id=" . $row['id'] . ", last_name=" . $row['last_name'] . ", first_name=" . $row['first_name'] . "\n";

	/* commit the transaction (DELETE FROM ...) */
	$rc=tx_commit();
	print "tx_commit(): $rc\n"; 

	/* start a new transaction coordinated by LIXA */
	$rc=tx_begin();
	print "tx_begin(): $rc\n"; 

	/* check the content of the tables */	
	$mysqli->real_query("SELECT id,last_name,first_name FROM authors ORDER BY id DESC;");
	$res = $mysqli->use_result();
	echo "Result set:\n";
	while ($row = $res->fetch_assoc())
		echo "id=" . $row['id'] . ", last_name=" . $row['last_name'] . ", first_name=" . $row['first_name'] . "\n";
	$mysqli2->real_query("SELECT id,last_name,first_name FROM authors ORDER BY id DESC;");
	$res = $mysqli2->use_result();
	echo "Result set (2):\n";
	while ($row = $res->fetch_assoc())
		echo "id=" . $row['id'] . ", last_name=" . $row['last_name'] . ", first_name=" . $row['first_name'] . "\n";

	/* populate the tables */
	print "INSERT INTO authors...\n";
	if (!$mysqli->query("INSERT INTO authors VALUES(969,'Ferrari','Christian');"))
		echo "INSERT failed: (" . $mysqli->errno . ") " . $mysqli->error;
	print "INSERT INTO authors (2)...\n";
	if (!$mysqli2->query("INSERT INTO authors VALUES(969,'Ferrari','Christian');"))
		echo "INSERT (2) failed: (" . $mysqli->errno . ") " . $mysqli->error;

	/* check the content of the tables */
	$mysqli->real_query("SELECT id,last_name,first_name FROM authors ORDER BY id DESC;");
	$res = $mysqli->use_result();
	echo "Result set:\n";
	while ($row = $res->fetch_assoc())
		echo "id=" . $row['id'] . ", last_name=" . $row['last_name'] . ", first_name=" . $row['first_name'] . "\n";
	$mysqli2->real_query("SELECT id,last_name,first_name FROM authors ORDER BY id DESC;");
	$res = $mysqli2->use_result();
	echo "Result set (2):\n";
	while ($row = $res->fetch_assoc())
		echo "id=" . $row['id'] . ", last_name=" . $row['last_name'] . ", first_name=" . $row['first_name'] . "\n";

	/* rollback the transaction (INSERT INTO ...) */
	$rc=tx_rollback();
	print "tx_rollback(): $rc\n"; 

	/* check the content of the tables */
	$mysqli->real_query("SELECT id,last_name,first_name FROM authors ORDER BY id DESC;");
	$res = $mysqli->use_result();
	echo "Result set:\n";
	while ($row = $res->fetch_assoc())
		echo "id=" . $row['id'] . ", last_name=" . $row['last_name'] . ", first_name=" . $row['first_name'] . "\n";
	$mysqli2->real_query("SELECT id,last_name,first_name FROM authors ORDER BY id DESC;");
	$res = $mysqli2->use_result();
	echo "Result set (2):\n";
	while ($row = $res->fetch_assoc())
		echo "id=" . $row['id'] . ", last_name=" . $row['last_name'] . ", first_name=" . $row['first_name'] . "\n";

	/* start a new transaction coordinated by LIXA */
	$rc=tx_begin();
	print "tx_begin(): $rc\n"; 

	/* populate the tables */
	print "INSERT INTO authors...\n";
	if (!$mysqli->query("INSERT INTO authors VALUES(969,'Ferrari','Christian');"))
		echo "INSERT failed: (" . $mysqli->errno . ") " . $mysqli->error;
	print "INSERT INTO authors (2)...\n";
	if (!$mysqli2->query("INSERT INTO authors VALUES(969,'Ferrari','Christian');"))
		echo "INSERT (2) failed: (" . $mysqli->errno . ") " . $mysqli->error;
	
	/* check the content of the tables */
	$mysqli->real_query("SELECT id,last_name,first_name FROM authors ORDER BY id DESC;");
	$res = $mysqli->use_result();
	echo "Result set:\n";
	while ($row = $res->fetch_assoc())
		echo "id=" . $row['id'] . ", last_name=" . $row['last_name'] . ", first_name=" . $row['first_name'] . "\n";
	$mysqli2->real_query("SELECT id,last_name,first_name FROM authors ORDER BY id DESC;");
	$res = $mysqli2->use_result();
	echo "Result set (2):\n";
	while ($row = $res->fetch_assoc())
		echo "id=" . $row['id'] . ", last_name=" . $row['last_name'] . ", first_name=" . $row['first_name'] . "\n";

	/* commit the transaction (INSERT INTO ...) */
	$rc=tx_commit();
	print "tx_commit(): $rc\n"; 

	/* check the content of the tables */
	$mysqli->real_query("SELECT id,last_name,first_name FROM authors ORDER BY id DESC;");
	$res = $mysqli->use_result();
	echo "Result set:\n";
	while ($row = $res->fetch_assoc())
		echo "id=" . $row['id'] . ", last_name=" . $row['last_name'] . ", first_name=" . $row['first_name'] . "\n";
	$mysqli2->real_query("SELECT id,last_name,first_name FROM authors ORDER BY id DESC;");
	$res = $mysqli2->use_result();
	echo "Result set (2):\n";
	while ($row = $res->fetch_assoc())
		echo "id=" . $row['id'] . ", last_name=" . $row['last_name'] . ", first_name=" . $row['first_name'] . "\n";

	/* close all the Resource Managers */
	$rc=tx_close();
	print "tx_close(): $rc\n"; 
?>

