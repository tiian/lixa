#!/bin/sh
#
# Copyright (c) 2009-2020, Christian Ferrari <tiian@users.sourceforge.net>
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
# This script extracts diff files comparying different version of files in
# parallel trees
#
# It needs two arguments:
#   arg1: base directory (original PHP version)
#   arg2: new directory (patched PHP version)
#
# It produces a diff file named: 
#   lixa-${NEW_DIRECTORY}-patch.diff
#
if [ $# -lt 2 ]
then
	echo "You must specify two arguments"
	exit 1
fi
if [ ! -d $1 ]
then 
	echo "First argument ($1) is not a directory"
	exit 1
fi
OLD=$1
if [ ! -d $2 ]
then 
	echo "First argument ($2) is not a directory"
	exit 1
fi
NEW=$2
#
# Check diff file does not exists; please save it before!
DIFF_FILE="lixa-${NEW}-patch.diff"
if test -f $DIFF_FILE
then
	echo "File $DIFF_FILE already exists; please save it and restart"
	echo "this script"
	exit 1
fi
#
# NOTE:
# Add all the changed and new files to the list
LIST="mysqli_api.c mysqli_nonapi.c php_mysqli_structs.h"
for FILE in $LIST
do
	diff -u -N $OLD/ext/mysqli/$FILE $NEW/ext/mysqli/$FILE >>$DIFF_FILE
done

#
# NOTE:
# Add all the changed and new files to the list
LIST="pgsql.c"
for FILE in $LIST
do
	diff -u -N $OLD/ext/pgsql/$FILE $NEW/ext/pgsql/$FILE >>$DIFF_FILE
done

#
# The patch can be applied against the original tree with
# patch --dry-run -p0 < file.diff
# patch -p0 < file.diff
