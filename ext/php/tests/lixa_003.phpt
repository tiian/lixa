--TEST--
LIXA mysqli availability  - basic test: MySQL is reachable with mysqli
--SKIPIF--
<?php 
if (!stristr(PHP_OS, "Linux")) die("skip this test is Linux platforms only");
if (!lixa_config_have_mysql()) die("skip this test requires LIXA configured for MySQL with mysqli driver");
?>
--FILE--
<?php
	/* try connect */
	$mysqli = new mysqli("localhost", "lixa", "", "lixa", 3306);
	if ($mysqli->connect_errno) 
		echo "NOT CONNECTED\n";
	else
		echo "CONNECT OK\n";
	/* try delete */
        if (!$mysqli->query("DELETE FROM authors;"))
                echo "DELETE failed: (" . $mysqli->errno . ") " . 
			$mysqli->error . "\n";
	else
		echo "DELETE OK\n";
	/* try insert */
        if (!$mysqli->query("INSERT INTO authors VALUES(969,'Ferrari','Christian');"))
                echo "INSERT failed: (" . $mysqli->errno . ") " . 
			$mysqli->error . "\n";
	else
		echo "INSERT OK\n";
	/* try select */
        $mysqli->real_query("SELECT id,last_name,first_name FROM authors ORDER BY id DESC;");
        $res = $mysqli->use_result();
        while ($row = $res->fetch_assoc())
                echo "id=" . $row['id'] . ", last_name=" . $row['last_name'] . ", first_name=" . $row['first_name'] . "\n";
?>
--EXPECT--
CONNECT OK
DELETE OK
INSERT OK
id=969, last_name=Ferrari, first_name=Christian
