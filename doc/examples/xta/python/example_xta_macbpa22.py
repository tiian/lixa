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
# This module is necessary for all the XTA related stuff
from xta import *

# Check command line parameters
if len(sys.argv) < 5:
	sys.stderr.write("This program requires two boolean parameters: " +
			"'commit' and 'insert' and two strings: " +
			"'Superior2SubordinateFIFOname', " +
			"'Subordinate2SuperiorFIFOname'\n")
	sys.exit(1)

commit = int(sys.argv[1])
insert = int(sys.argv[2])
sup2sub_fifoname = sys.argv[3]
sub2sup_fifoname = sys.argv[4]

# Prepare SQL statements in accordance with "insert" command line parameter
if insert:
	postgresql_stmt = "INSERT INTO authors VALUES(1921, 'Rigoni Stern', 'Mario')"
else:
	postgresql_stmt = "DELETE FROM authors WHERE id=1921"

# initialize XTA environment
Xta_init()

# create a new PostgreSQL connection
# Note: using PostgreSQL Psycopg2 functions
rm = psycopg2.connect("dbname=testdb")

# create a new XTA Transaction Manager object
tm = TransactionManager()

# create an XA resource for PostgreSQL
# second parameter "PostgreSQL" is descriptive
# third parameter "dbname=testdb" identifies the specific database
#
# PostgreSQL driver is available here:
# https://github.com/tiian/psycopg2/tree/get-native-connection
# it should be available in Psycopg2 2.8
xar = PostgresqlXaResource(rm.get_native_connection(), "PostgreSQL", "dbname=testdb")

# Create a new XA global transaction and retrieve a reference from
# the TransactionManager object
tx = tm.createTransaction()

# Enlist PostgreSQL resource to transaction
tx.enlistResource(xar)

# *** NOTE: ***
# at this point, subordinate Application Program must wait until
# superior Application Program has started the transaction.
# Here the synchronization is implemented with
# a synchronous message passing using a named pipe (FIFO)

# open the pipe for read operation
sup2subFifo = open(sup2sub_fifoname, 'r')
xidString = sup2subFifo.read()
sys.stdout.write("Subordinate AP has received XID '" + xidString + "' from superior AP\n")
sup2subFifo.close()

# create a new branch in the same global transaction
tx.branch(xidString);

# the branch has the same global identifier, but a different branch id
branchXidString = tx.getXid().toString();
sys.stdout.write("Subordinate AP has created a branch with XID '" + branchXidString + "'\n")

# *** NOTE: ***
# subordinate Application Program (this one) has branched the
# transaction and must send a message to the superior Application
# Program that can proceed with it's own operations

# open the pipe for write operation
sub2supFifo = open(sub2sup_fifoname, 'w')
# write the message
sub2supFifo.write(branchXidString)
sys.stdout.write("Subordinate AP has returned '" + branchXidString + "' to superior AP\n")
sub2supFifo.close()

# *** NOTE: ***
# at this point the subordinate Application Program (this one) can
# go on with its own operations indipendently from the superior AP

# Execute PostgreSQL statement
sys.stdout.write("PostgreSQL, executing >" + postgresql_stmt + "<\n")
cur = rm.cursor()
cur.execute(postgresql_stmt)

# commit or rollback the transaction
if commit:
	tx.commit()
	sys.stdout.write("Subordinate AP has committed its branch\n")
else:
	tx.rollback()
	sys.stdout.write("Subordinate AP has rolled back its branch\n")

# Close the PostgreSQL connection
cur.close()
