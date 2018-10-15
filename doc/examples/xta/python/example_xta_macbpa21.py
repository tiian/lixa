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
import MySQLdb
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
	mysql_stmt = "INSERT INTO authors VALUES(1919, 'Levi', 'Primo')"
else:
	mysql_stmt = "DELETE FROM authors WHERE id=1919"

# initialize XTA environment
Xta_Init()

# create a new MySQL connection
# Note: using MySQLdb functions
rm = MySQLdb.connect(host="localhost", user="lixa", db="lixa")

# create a new XTA Transaction Manager object
tm = TransactionManager()

# create an XA resource for MySQL
# second parameter "MySQL" is descriptive
# third parameter "localhost,0,lixa,,lixa" identifies the specific database
#
# MySQL driver is available here:
# https://github.com/tiian/mysqlclient-python/tree/get_native_connection
# it should be merged in the master tree in the future
xar = MysqlXaResource(rm._get_native_connection(), "MySQL", "localhost,0,lixa,,lixa")

# Create a new XA global transaction and retrieve a reference from
# the TransactionManager object
tx = tm.CreateTransaction()

# Enlist MySQL resource to transaction
tx.EnlistResource(xar)

# Open all resources enlisted by tx Transaction
tx.Open()

# Start a new XA global transaction for multiple branches
# Note: argument ("MultipleBranch") has true value because the
#       transaction will be branched by the subordinate Application
#       Program
tx.Start(True)

# Retrieve the Transaction ID (XID) associated to the transaction
# that has been started in the previous step
xidString = tx.getXid().toString()

# *** NOTE: ***
# a synchronization message must be sent to the subordinate Application
# Program: the global transaction has been started and the subordinate AP
# can branch it. The synchronization is implemented
# with a synchronous message passing using a named pipe (FIFO)
#
# open the pipe for write operation
sup2subFifo = open(sup2sub_fifoname, 'w')
# write the message
sup2subFifo.write(xidString)
sup2subFifo.close()
sys.stdout.write("Superior AP has sent XID '" + xidString + "' to subordinate AP\n")
# open the pipe for read operation
sub2supFifo = open(sub2sup_fifoname, 'r')
reply = sub2supFifo.read()
sub2supFifo.close()
sys.stdout.write("Superior AP has received '" + reply + "' reply from subordinate AP\n")

# *** NOTE: ***
# at this point the subordinate Application Program has branched the
# transaction and this (superior) Application Program can go on with
# the main branch created by tx.Start() indipendently from the subordinate AP

# Execute MySQL statement
sys.stdout.write("MySQL, executing >" + mysql_stmt + "<\n")
cur = rm.cursor()
cur.execute(mysql_stmt)

# commit or rollback the transaction
if commit:
	tx.Commit()
	sys.stdout.write("Superior AP has committed its branch\n")
else:
	tx.Rollback()
	sys.stdout.write("Superior AP has rolled back its branch\n")

# Close all resources enlisted by the Transaction
tx.Close()

# Close the MySQL connection
cur.close()
