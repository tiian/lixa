#!/bin/bash
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

# Options:
# $1 name of the file with the arguments to execute

if test $# -lt 1
then
	echo "At least one parameter must be specified"
	exit 1
fi	

if test -e "$1"
then
	echo "Reading commands from $1"
else
	echo "File $1 does not exist"
	exit 1
fi

PID_LIST=""

# all commands are executed with a reasonable timeout
while IFS='' read -r LINE || [[ -n "$LINE" ]]; do
	echo "Command to execute: $LINE"
	sh -c "$LINE" 2>&1 &
	PID=$!
	echo "Started with pid $PID"
	PID_LIST="$PID_LIST $PID" 
done < "$1"

TOTAL_ERRORS=0
for PID in $PID_LIST
do
	echo -n "Waiting PID $PID ... "
	wait $PID
	RC=$?
	echo " ... PID $PID exited with RC=$RC"
	TOTAL_ERRORS=$(($TOTAL_ERRORS+$RC))
done

echo "TOTAL_ERRORS=$TOTAL_ERRORS"
exit $TOTAL_ERRORS
