<?php	
	/* open all the Resource Managers (MySQL instance in this example) */
	$rc=tx_open();
	print "tx_open(): $rc\n"; 

	/* retrieve MySQL connection from LIXA Transaction Manager instead
	   of MySQL directly: lixa// means:
	   database name=lixa
	   chooser type (pos/rmid)=none
	   id (0, 1, 2, ...)=none
	   and can be read as "retrieve the first available MySQL connection
	   established by the Transaction Manager using the current
	   LIXA profile" */
	$mysqli = new mysqli("localhost", "lixa", "", "lixa//", 3306);
	if ($mysqli->connect_errno)
		echo "Failed to connect to MySQL: (" . $mysqli->connect_errno . ") " . $mysqli->connect_error . "\n";

	/* start a new transaction coordinated by LIXA */
	$rc=tx_begin();
	print "tx_begin(): $rc\n"; 

	/* remove all the rows from the table */
	print "DELETE FROM authors;\n";
	if (!$mysqli->query("DELETE FROM authors;"))
		echo "DELETE failed: (" . $mysqli->errno . ") " . $mysqli->error;

	/* check the content of the table */	
	$mysqli->real_query("SELECT id,last_name,first_name FROM authors ORDER BY id DESC;");
	$res = $mysqli->use_result();
	echo "Result set:\n";
	while ($row = $res->fetch_assoc())
		echo "id=" . $row['id'] . ", last_name=" . $row['last_name'] . ", first_name=" . $row['first_name'] . "\n";

	/* commit the transaction (DELETE FROM ...) */
	$rc=tx_commit();
	print "tx_commit(): $rc\n"; 

	/* start a new transaction coordinated by LIXA */
	$rc=tx_begin();
	print "tx_begin(): $rc\n"; 

	/* check the content of the table */	
	$mysqli->real_query("SELECT id,last_name,first_name FROM authors ORDER BY id DESC;");
	$res = $mysqli->use_result();
	echo "Result set:\n";
	while ($row = $res->fetch_assoc())
		echo "id=" . $row['id'] . ", last_name=" . $row['last_name'] . ", first_name=" . $row['first_name'] . "\n";

	/* populate the table */
	print "INSERT INTO authors...\n";
	if (!$mysqli->query("INSERT INTO authors VALUES(969,'Ferrari','Christian');"))
		echo "INSERT failed: (" . $mysqli->errno . ") " . $mysqli->error;
	
	/* check the content of the table */
	$mysqli->real_query("SELECT id,last_name,first_name FROM authors ORDER BY id DESC;");
	$res = $mysqli->use_result();
	echo "Result set:\n";
	while ($row = $res->fetch_assoc())
		echo "id=" . $row['id'] . ", last_name=" . $row['last_name'] . ", first_name=" . $row['first_name'] . "\n";

	/* rollback the transaction (INSERT INTO ...) */
	$rc=tx_rollback();
	print "tx_rollback(): $rc\n"; 

	/* check the content of the table */
	$mysqli->real_query("SELECT id,last_name,first_name FROM authors ORDER BY id DESC;");
	$res = $mysqli->use_result();
	echo "Result set:\n";
	while ($row = $res->fetch_assoc())
		echo "id=" . $row['id'] . ", last_name=" . $row['last_name'] . ", first_name=" . $row['first_name'] . "\n";

	/* start a new transaction coordinated by LIXA */
	$rc=tx_begin();
	print "tx_begin(): $rc\n"; 

	/* populate the table */
	print "INSERT INTO authors...\n";
	if (!$mysqli->query("INSERT INTO authors VALUES(969,'Ferrari','Christian');"))
		echo "INSERT failed: (" . $mysqli->errno . ") " . $mysqli->error;
	
	/* check the content of the table */
	$mysqli->real_query("SELECT id,last_name,first_name FROM authors ORDER BY id DESC;");
	$res = $mysqli->use_result();
	echo "Result set:\n";
	while ($row = $res->fetch_assoc())
		echo "id=" . $row['id'] . ", last_name=" . $row['last_name'] . ", first_name=" . $row['first_name'] . "\n";

	/* commit the transaction (INSERT INTO ...) */
	$rc=tx_commit();
	print "tx_commit(): $rc\n"; 

	/* check the content of the table */
	$mysqli->real_query("SELECT id,last_name,first_name FROM authors ORDER BY id DESC;");
	$res = $mysqli->use_result();
	echo "Result set:\n";
	while ($row = $res->fetch_assoc())

	/* close all the Resource Managers */
	$rc=tx_close();
	print "tx_close(): $rc\n"; 
?>

