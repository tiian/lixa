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

# Options:
# $1 [reset|noreset] to reset the LIXA server status
# $2 [start|stop|recycle|none] to start, stop, recycle or keep running the 
#    server
# $3 name of the case test must be run

. lixa_test_functions.sh

if test $# -lt 3
then
	echo "At least three parameters must be specified"
	exit 1
fi	

if test x"$1" = xreset
then
	reset_server
fi	

START_SERVER="NO"
STOP_SERVER="NO"
case "$2" in
	start) START_SERVER="YES" 
	;;
	stop) STOP_SERVER="YES"
	;;
	recycle) START_SERVER="YES" ; STOP_SERVER="YES"
	;;
	none)
	;;
	*)
	echo "Second argument must be [start|stop|recycle|none]"
	exit 1
	;;
esac

if test -x "$3"
then
	echo "No program to test, exiting with error"
	exit 1
fi

shift 2

if test "$START_SERVER" = "YES"
then
	start_server
fi

if test "x$1" = "xtrue"
then
	echo "true is not a case test, just a dummy step, skipping..."
else
	exec_test $*
	rc=$?
fi

if test "$STOP_SERVER" = "YES"
then
	stop_server
fi

exit $rc
