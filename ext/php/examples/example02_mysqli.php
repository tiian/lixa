<?php
echo "Using mysqli direct API\n";
$mysqli = new mysqli("localhost", "lixa", "", "lixa", 3306);
if ($mysqli->connect_errno)
	echo "Failed to connect to MySQL: (" . $mysqli->connect_errno . ") " . $mysqli->connect_error;

if (!$mysqli->query("DELETE FROM authors;"))
	echo "DELETE failed: (" . $mysqli->errno . ") " . $mysqli->error;
if (!$mysqli->query("INSERT INTO authors VALUES(999,'surname','name');"))
	echo "INSERT failed: (" . $mysqli->errno . ") " . $mysqli->error;
if (!$mysqli->query("INSERT INTO authors VALUES(969,'Ferrari','Christian');"))
	echo "INSERT failed: (" . $mysqli->errno . ") " . $mysqli->error;
if (!$mysqli->query("INSERT INTO authors VALUES(333,'Ferrari','C');"))
	echo "INSERT failed: (" . $mysqli->errno . ") " . $mysqli->error;

$mysqli->real_query("SELECT id,last_name,first_name FROM authors ORDER BY id DESC;");
$res = $mysqli->use_result();
echo "Result set order...\n";
while ($row = $res->fetch_assoc())
	echo "id=" . $row['id'] . ", last_name=" . $row['last_name'] . ", first_name=" . $row['first_name'] . "\n";
?>

