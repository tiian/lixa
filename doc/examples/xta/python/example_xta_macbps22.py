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
Xta_Init()

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
# https://github.com/fogzot/psycopg2/tree/feature-expose-pgconn
# it should be available in Psycopg2 2.8
xar = PostgresqlXaResource(rm.get_native_connection(), "PostgreSQL", "dbname=testdb")

# Create a new XA global transaction and retrieve a reference from
# the TransactionManager object
tx = tm.CreateTransaction()

# Enlist PostgreSQL resource to transaction
tx.EnlistResource(xar)

# Open all resources enlisted by tx Transaction
tx.Open()

# *** NOTE: ***
# at this point, subordinate Application Program must wait a Remote
# Procedure Call (RPC) or a Web Service (WS) or a REST API invocation from
# superior Application Program. Here the incoming call is emulated with
# a synchronous message passing using a named pipe (FIFO)
#
# open the pipe for read operation
sup2subFifo = open(sup2sub_fifoname, 'r')
xidString = sup2subFifo.read()
sys.stdout.write("Subordinate AP has received XID '" + xidString + "' from superior AP\n")
sup2subFifo.close()

# create a new branch in the same global transaction
tx.Branch(xidString);

# the branch has the same global identifier, but a different branch id
branchXidString = tx.getXid().toString();
sys.stdout.write("Subordinate AP has created a branch with XID '" + branchXidString + "'\n")

# Execute PostgreSQL statement
sys.stdout.write("PostgreSQL, executing >" + postgresql_stmt + "<\n")
cur = rm.cursor()
cur.execute(postgresql_stmt)

# commit or rollback the transaction
if commit:
	# *** NOTE: ***
	# commit MUST be performed in two step:
	# 1. in first step, the branch is only "prepared" (as in XA
	#    specification) and control can be returned to the superior AP
	#    that has to start its commit
	# 2. in the second step, the branch is definitely "committed", but
	#    the operation will block the caller because the subordinate AP
	#    must wait the "prepared" state of the superior AP before
	#    committing
	#
	# commit is performed with "nonBlocking" flag set to TRUE: this is
	# necessary to allow the superior branch to commit */
	tx.Commit(True)
else:
	tx.Rollback()

# *** NOTE: ***
# at this point the subordinate Application Program (this one) has to
# reply to the superior A.P. that's waiting. Here the reply is emulated
# with a synchronous message passing using a named pipe (FIFO)
#
# open the pipe for write operation
sub2supFifo = open(sub2sup_fifoname, 'w')
# prepare the reply message
if commit:
	reply_msg = "PREPARED_for_COMMIT"
else:
	reply_msg = "ROLLBACK"
# write the message
sub2supFifo.write(reply_msg)

sys.stdout.write("Subordinate AP has returned '" + reply_msg + "' to superior AP\n")
sub2supFifo.close()

if commit:
	# Complete the second phase of the commit with "nonBlocking" flag set
	# to FALSE: this is necessary to wait the superior AP prepare phase
	tx.Commit(False)

# Close all resources enlisted by the Transaction
tx.Close()

# Close the PostgreSQL connection
cur.close()
