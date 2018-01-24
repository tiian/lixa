#!/bin/sh
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
# This script check the consistency of different databases.
# The test assures that all the tables "authors" in all the databases
# contains the same number of rows. This is typically correct after a
# global commit or after a global rollback
#

echo "Checking available databases:"
echo "HAVE_MYSQL=$HAVE_MYSQL"
echo "HAVE_ORACLE=$HAVE_ORACLE"
echo "HAVE_POSTGRESQL=$HAVE_POSTGRESQL"

if test "$HAVE_MYSQL" = "yes"
then
	echo "Checking MySQL / MariaDB..."
	TMP_MYSQL=$(mktemp)
	echo "select count(*) as NumberOfRows from authors;" | mysql -h localhost -u lixa lixa > $TMP_MYSQL
	cat $TMP_MYSQL | grep -E -e '^[[:space:]]*[[:digit:]]+$' | tr -d '[:blank:]'
	NOR_MYSQL=$(cat $TMP_MYSQL | grep -E -e '^[[:space:]]*[[:digit:]]+$' | tr -d '[:blank:]')
	rm $TMP_MYSQL
fi

if test "$HAVE_ORACLE" = "yes"
then
	echo "Checking Oracle database..."
	TMP_ORACLE=$(mktemp)
	echo "select count(*) as NumberOfRows from authors;" | sqlplus hr/hr@lixa_ora_db> $TMP_ORACLE
	cat $TMP_ORACLE | grep -E -e '^[[:space:]]*[[:digit:]]+$' | tr -d '[:blank:]'
	NOR_ORACLE=$(cat $TMP_ORACLE | grep -E -e '^[[:space:]]*[[:digit:]]+$' | tr -d '[:blank:]')
	rm $TMP_ORACLE
fi

if test "$HAVE_POSTGRESQL" = "yes"
then
	echo "Checking PostgreSQL..."
	TMP_POSTGRESQL=$(mktemp)
	echo "select count(*) as NumberOfRows from authors;" | psql testdb > $TMP_POSTGRESQL
	cat $TMP_POSTGRESQL | grep -E -e '^[[:space:]]*[[:digit:]]+$' | tr -d '[:blank:]'
	NOR_POSTGRESQL=$(cat $TMP_POSTGRESQL | grep -E -e '^[[:space:]]*[[:digit:]]+$' | tr -d '[:blank:]')
	rm $TMP_POSTGRESQL
fi

echo "NOR_MYSQL=$NOR_MYSQL"
echo "NOR_ORACLE=$NOR_ORACLE"
echo "NOR_POSTGRESQL=$NOR_POSTGRESQL"

NOR=0
NOV=0
if test "$HAVE_MYSQL"
then
	NOR=$((NOR + NOR_MYSQL))
	NOV=$((NOV + 1))
fi
if test "$HAVE_ORACLE"
then
	NOR=$((NOR + NOR_ORACLE))
	NOV=$((NOV + 1))
fi
if test "$HAVE_POSTGRESQL"
then
	NOR=$((NOR + NOR_POSTGRESQL))
	NOV=$((NOV + 1))
fi
if [ "$NOV" -gt 0 ]
then
	ENOR=$((NOR / NOV))
else
	echo "No variable, exiting..."
	exit 0
fi

echo "Total Number Of Rows=$NOR"
echo "Total Number Of Variables=$NOV"
echo "Equal Number Of Rows=$ENOR"

if test "$HAVE_MYSQL"
then
	if [ "$NOR_MYSQL" -ne "$ENOR" ]
	then
		echo "NOR_MYSQL != ENOR: $NOR_MYSQL != $ENOR"
		exit 1
	fi
fi

if test "$HAVE_ORACLE"
then
	if [ "$NOR_ORACLE" -ne "$ENOR" ]
	then
		echo "NOR_ORACLE != ENOR: $NOR_ORACLE != $ENOR"
		exit 1
	fi
fi

if test "$HAVE_POSTGRESQL"
then
	if [ "$NOR_POSTGRESSQL" -ne "$ENOR" ]
	then
		echo "NOR_POSTGRESQL != ENOR: $NOR_POSTGRESQL != $ENOR"
		exit 1
	fi
fi

exit 0
