<?php	
	/* set PHPRC to /home/tiian/src/swig to pick-up php.ini when using
	   /opt/php/bin/php to run this script; it's not necessary if using
	   /opt/php/bin/php-cgi */
	/* set LIXA_PROFILE=MYS_STA_PQL_STA */
	/* run me using /opt/php/bin/php -c php.ini foo.php */
	/*
	include("lixa.php");
	*/
	$rc=tx_open();
	print "tx_open() --> $rc\n"; 
	
	$mysqli = new mysqli("localhost", "lixa", "", "lixa//", 3306);
	if ($mysqli->connect_errno)
		echo "Failed to connect to MySQL: (" . $mysqli->connect_errno . ") " . $mysqli->connect_error . "\n";

	$rc=tx_begin();
	print "tx_begin() --> $rc\n"; 

	print "DELETE FROM authors;\n";
	if (!$mysqli->query("DELETE FROM authors;"))
		echo "DELETE failed: (" . $mysqli->errno . ") " . $mysqli->error;
	$mysqli->real_query("SELECT id,last_name,first_name FROM authors ORDER BY id DESC;");
	$res = $mysqli->use_result();
	echo "Result set order...\n";
	while ($row = $res->fetch_assoc())
		echo "id=" . $row['id'] . ", last_name=" . $row['last_name'] . ", first_name=" . $row['first_name'] . "\n";

	$rc=tx_commit();
	print "tx_commit() --> $rc\n"; 

	$rc=tx_begin();
	print "tx_begin() --> $rc\n"; 

	$mysqli->real_query("SELECT id,last_name,first_name FROM authors ORDER BY id DESC;");
	$res = $mysqli->use_result();
	echo "Result set order...\n";
	while ($row = $res->fetch_assoc())
		echo "id=" . $row['id'] . ", last_name=" . $row['last_name'] . ", first_name=" . $row['first_name'] . "\n";

	print "INSERT INTO authors...\n";
	if (!$mysqli->query("INSERT INTO authors VALUES(999,'surname','name');"))
		echo "INSERT failed: (" . $mysqli->errno . ") " . $mysqli->error;
	print "INSERT INTO authors...\n";
	if (!$mysqli->query("INSERT INTO authors VALUES(969,'Ferrari','Christian');"))
		echo "INSERT failed: (" . $mysqli->errno . ") " . $mysqli->error;
	print "INSERT INTO authors...\n";
	if (!$mysqli->query("INSERT INTO authors VALUES(333,'Ferrari','C');"))
		echo "INSERT failed: (" . $mysqli->errno . ") " . $mysqli->error;
	$mysqli->real_query("SELECT id,last_name,first_name FROM authors ORDER BY id DESC;");
	$res = $mysqli->use_result();
	echo "Result set order...\n";
	while ($row = $res->fetch_assoc())
		echo "id=" . $row['id'] . ", last_name=" . $row['last_name'] . ", first_name=" . $row['first_name'] . "\n";

	$rc=tx_rollback();
	print "tx_rollback() --> $rc\n"; 

	$mysqli->real_query("SELECT id,last_name,first_name FROM authors ORDER BY id DESC;");
	$res = $mysqli->use_result();
	echo "Result set order...\n";
	while ($row = $res->fetch_assoc())
		echo "id=" . $row['id'] . ", last_name=" . $row['last_name'] . ", first_name=" . $row['first_name'] . "\n";

	$rc=tx_begin();
	print "tx_begin() --> $rc\n"; 

	print "INSERT INTO authors...\n";
	if (!$mysqli->query("INSERT INTO authors VALUES(999,'surname','name');"))
		echo "INSERT failed: (" . $mysqli->errno . ") " . $mysqli->error;
	print "INSERT INTO authors...\n";
	if (!$mysqli->query("INSERT INTO authors VALUES(969,'Ferrari','Christian');"))
		echo "INSERT failed: (" . $mysqli->errno . ") " . $mysqli->error;
	print "INSERT INTO authors...\n";
	if (!$mysqli->query("INSERT INTO authors VALUES(333,'Ferrari','C');"))
		echo "INSERT failed: (" . $mysqli->errno . ") " . $mysqli->error;
	
	$mysqli->real_query("SELECT id,last_name,first_name FROM authors ORDER BY id DESC;");
	$res = $mysqli->use_result();
	echo "Result set order...\n";
	while ($row = $res->fetch_assoc())
		echo "id=" . $row['id'] . ", last_name=" . $row['last_name'] . ", first_name=" . $row['first_name'] . "\n";

	$rc=tx_commit();
	print "tx_commit() --> $rc\n"; 

	$mysqli->real_query("SELECT id,last_name,first_name FROM authors ORDER BY id DESC;");
	$res = $mysqli->use_result();
	echo "Result set order...\n";
	while ($row = $res->fetch_assoc())

	$rc=tx_close();
	print "tx_close() --> $rc\n"; 
	$rc=TX_OK;
	print "TX_OK=$rc\n";
?>

