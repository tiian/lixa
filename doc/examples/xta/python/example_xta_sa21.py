#
# Copyright (c) 2009-2018, Christian Ferrari <tiian@users.sourceforge.net>
# All rights reserved.
#
# This file is part of LIXA.
#
# LIXA is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License version 2 as published
# by the Free Software Foundation.
#
# LIXA is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with LIXA.  If not, see <http://www.gnu.org/licenses/>.
#

#
# This program is an example implementation of the
# "Single Application" Pattern
# as documented in LIXA manual:
# http://www.tiian.org/lixa/manuals/html/index.html
#
# This program accepts exactly two parameters on the command line:
# first parameter:  "commit", boolean value (if FALSE, "rollback")
# second parameter: "insert", boolean value (if FALSE, "delete")
#
# Programming Style note:
# the purpose of this small program is not to explain Python development
# techniques or good style, but simply to show XTA for Python using the easiest
# approach.
#

import sys
import psycopg2
import MySQLdb
from xta import *

# Check command line parameters
if len(sys.argv) < 3:
	sys.stderr.write("This program requires two boolean parameters: " +
	"'commit' and 'insert'\n")
	sys.exit(1)

commit = int(sys.argv[1])
insert = int(sys.argv[2])

# Prepare SQL statements in accordance with "insert" command line parameter
if insert:
	postgresql_stmt = "INSERT INTO authors VALUES(1921, 'Rigoni Stern', 'Mario')"
	mysql_stmt = "INSERT INTO authors VALUES(1919, 'Levi', 'Primo')"
else:
	postgresql_stmt = "DELETE FROM authors WHERE id=1921"
	mysql_stmt = "DELETE FROM authors WHERE id=1919"

# initialize XTA environment
Xta_Init()

# create a new PostgreSQL connection
# Note: using PostgreSQL Psycopg2 functions
rm1 = psycopg2.connect("dbname=testdb")

# create a new MySQL connection
# Note: using MySQLdb functions
rm2 = MySQLdb.connect("localhost", "lixa", "", "lixa")

# create a new XTA Transaction Manager object
tm = TransactionManager()

# create an XA resource for PostgreSQL
# second parameter "PostgreSQL" is descriptive
# third parameter "dbname=testdb" identifies the specific database
#
# how to retrieve PGconn * from rm1?!
xar1 = PostgresqlXaResource(rm1.conn, "PostgreSQL", "dbname=testdb")

# Execute PostgreSQL statement
sys.stdout.write("PostgreSQL, executing >" + postgresql_stmt + "<\n")
cur1 = rm1.cursor()
cur1.execute(postgresql_stmt)

# Execute MySQL statement
sys.stdout.write("MySQL, executing >" + mysql_stmt + "<\n")
cur2 = rm2.cursor()
cur2.execute(mysql_stmt)

# commit or rollback the transaction
if commit:
	rm1.commit()
	rm2.commit()
else:
	rm1.rollback()
	rm2.rollback()

# Close the PostgreSQL connection
cur1.close()

# Close the MySQL connection
cur2.close()
