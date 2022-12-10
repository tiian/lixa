#!/bin/sh
#
# Copyright (c) 2009-2023, Christian Ferrari <tiian@users.sourceforge.net>
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

# Use this script to set-up the test environment for MySQL
# NOTE: you must be allowed to use the "sudo" command on behalf of root

# checking for apt-get (Debian derivatives)
type apt-get
if test $? -eq 0
then
	sudo apt-get update
	[ $? -ne 0 ] && echo "apt-get update error" && exit 1
	sudo apt-get install mysql-server
	[ $? -ne 0 ] && echo "apt-get install mysql-server" && exit 1
	sudo apt-get install libmysqlclient15-dev
	[ $? -ne 0 ] && echo "apt-get install libmysqlclient-dev" && exit 1
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
	sudo yum install mariadb-devel
	sudo service mysqld start
	sudo service mariadb start
fi

# creating user and database
echo "Creating lixa user and lixa database in MySQL"
mysql -u root -p <<EOF
CREATE USER 'lixa'@'localhost';
GRANT ALL ON lixa.* TO 'lixa'@'localhost';
CREATE DATABASE lixa;
quit
EOF
[ $? -ne 0 ] && echo "MySQL create database error" && exit 1

# creating table
echo "Creating authors table in lixa database"
mysql -h localhost -u lixa lixa <<EOF
SELECT DATABASE();
CREATE TABLE authors (id INTEGER NOT NULL PRIMARY KEY, last_name TEXT, first_name TEXT) ENGINE=InnoDB;
DESCRIBE authors;
SELECT * FROM authors;
quit
EOF
[ $? -ne 0 ] && echo "MySQL create table error" && exit 1

