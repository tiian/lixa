--TEST--
LIXA PostgreSQL availability - basic test: PostgreSQL is reachable
--SKIPIF--
<?php 
if (!stristr(PHP_OS, "Linux")) die("skip this test is Linux platforms only");
if (!lixa_config_have_postgresql()) die("skip this test requires LIXA configured for PostgreSQL");
?>
--FILE--
<?php
/* Connecting to "testdb" local PostgreSQL database */
$dbconn = pg_connect("dbname=testdb")
    or die('Could not connect: ' . pg_last_error());
echo "CONNECT OK\n";

/* Clean "authors" table from any rows */
$query = 'DELETE FROM authors';
$result = pg_query($query) or die('Query failed: ' . pg_last_error() . '\n');
echo "DELETE OK\n";

/* Insert a test row */
$query = "INSERT INTO authors VALUES(999, 'Ferrari', 'Christian')";
$result = pg_query($query) or die('Query failed: ' . pg_last_error() . '\n');
echo "INSERT OK\n";

/* Retrieving table content */
$query = 'SELECT * FROM authors';
$result = pg_query($query) or die('Query failed: ' . pg_last_error(). '\n');

/* Print table content */
while ($line = pg_fetch_array($result, null, PGSQL_ASSOC)) {
    foreach ($line as $col_value) {
        echo "  $col_value";
    }
    echo "\n";
}

/* Free result set */
pg_free_result($result);

/* Close database connection */
pg_close($dbconn);
?>
--EXPECT--
CONNECT OK
DELETE OK
INSERT OK
  999  Ferrari  Christian
