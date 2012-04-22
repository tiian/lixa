<?php
// Connecting, selecting database
$dbconn = pg_connect("dbname=testdb") or die("Could not connect: " . pg_last_error() . "\n");

// Performing SQL query
$query = 'DELETE FROM authors';
$result = pg_query($query) or die("Query failed: " . pg_last_error() . "\n");

// Performing SQL query
$query = "INSERT INTO authors VALUES(999, 'Ferrari', 'Christian')";
$result = pg_query($query) or die("Query failed: " . pg_last_error() . "\n");

// Performing SQL query
$query = 'SELECT * FROM authors';
$result = pg_query($query) or die("Query failed: " . pg_last_error(). "\n");

// Printing results in HTML
while ($line = pg_fetch_array($result, null, PGSQL_ASSOC)) {
    foreach ($line as $col_value) {
        echo "  $col_value";
    }
    echo "\n";
}

// Free resultset
pg_free_result($result);

// Closing connection
pg_close($dbconn);
?>

