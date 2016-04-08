#!/bin/sh
#
# Copyright (c) 2009-2016, Christian Ferrari <tiian@users.sourceforge.net>
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

# Use this script to set-up the test environment for PostgreSQL
# NOTE: you must be allowed to use the "sudo" command on behalf of root

# checking for apt-get (Debian derivatives)
type apt-get
if test $? -eq 0
then
	sudo apt-get update
	[ $? -ne 0 ] && echo "apt-get update error" && exit 1
	sudo apt-get install postgresql
	[ $? -ne 0 ] && echo "apt-get install postgresql" && exit 1
	sudo apt-get install libpq-dev
	[ $? -ne 0 ] && echo "apt-get install libpq-dev" && exit 1
fi
# checking for yum (Red Hat derivatives)
type yum
if test $? -eq 0
then
	sudo yum install mysql-server
	RC1=$?
	sudo yum install mariadb-server
	RC2=$?
	[ $RC1 -eq $RC2 ] && echo "yum install mysql-server/mariadb-server" && exit 1
	sudo yum install mysql-devel
	RC1=$?
	sudo yum install mariadb-devel
	RC2=$?
	[ $RC1 -eq $RC2 ] && echo "yum install mysql-devel/mariadb/devel" && exit 1
	sudo service mysqld start
	RC1=$?
	sudo service mariadb start
	RC2=$?
	[ $RC1 -eq $RC2 ] && echo "service mysqld/mariadb start" && exit 1
fi

echo "Creating a role for user $USER"
sudo -u postgres -s createuser --createdb --no-superuser --no-createrole $USER
[ $? -ne 0 ] && echo "Create user error" && exit 1

echo "Creating testdb database"
createdb testdb
[ $? -ne 0 ] && echo "Create database error" && exit 1

psql testdb <<EOF
CREATE TABLE "authors" (
 "id" integer NOT NULL,
 "last_name" text,
 "first_name" text,
 Constraint "authors_pkey" Primary Key ("id"));
SELECT * FROM authors;
\q
EOF
[ $? -ne 0 ] && echo "PostgreSQL create table error" && exit 1

echo "Change the value of parameter max_prepared_transactions inside file"
echo "postgresq.conf to a reasonable value, for example 10 and restart the"
echo "database server"
