#!/bin/sh
#
# Copyright (c) 2009-2010, Christian Ferrari <tiian@users.sourceforge.net>
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
# $1 name of the case test must be run
# $2 [reset|noreset] to reset the LIXA server status

. lixa_test_functions.sh

if test -z "$1"
then
	echo "No program to test, exiting with error"
	exit 1
fi

if test x"$2" = xreset
then
	reset_server
fi	

exec_test $1
