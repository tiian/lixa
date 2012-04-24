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

	// Connecting, selecting database
	$dbconn = pg_connect("dbname=testdb") 
		or die("Could not connect: " . pg_last_error() . "\n");

	// Performing SQL query
	$query = 'DELETE FROM authors';
	$result = pg_query($query) 
		or die("Query failed: " . pg_last_error() . "\n");

	// Performing SQL query
	$query = "INSERT INTO authors VALUES(999, 'Ferrari', 'Christian')";
	$result = pg_query($query) 
		or die("Query failed: " . pg_last_error() . "\n");

	// Performing SQL query
	$query = 'SELECT * FROM authors';
	$result = pg_query($query) 
		or die("Query failed: " . pg_last_error(). "\n");

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

