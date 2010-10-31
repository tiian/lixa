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


export LIXA_TRACE_MASK=0xffffffff


# This file contains utility functions for test purpouses
reset_server() {
	echo "Resetting LIXA server status"
	rm -rf $TESTS_VAR_DIR
	mkdir $TESTS_VAR_DIR
}

start_server() {
	echo "Starting LIXA server"
	lixad --daemon --config-file=$TESTS_ETC_DIR/lixad_conf.xml --trace-file=$TESTS_TMP_DIR/lixad.trace
	echo "LIXA server is running with PID " $(cat $TESTS_VAR_DIR/run.pid)
}

stop_server() {
	echo "Stopping LIXA server running with PID " $(cat $TESTS_VAR_DIR/run.pid)
	PID="$(cat $TESTS_VAR_DIR/run.pid)"
	kill $PID
	while test $(ps -p $PID | grep lixad | wc -l) -gt 0
	do
		echo "Waiting server (PID=$PID) termination..."
		sleep 1
	done
}

exec_test() {
	echo "Starting case test $1"
	PGM=$1
	shift
	if [ "$MEMCHECK" = "yes" ]
	then
		export G_SLICE=always-malloc
		#libtool --mode=execute valgrind --leak-check=full --show-reachable=yes --num-callers=1000 --gen-suppressions=all $TESTS_SRC_DIR/$PGM $*
		#libtool --mode=execute valgrind --leak-check=full --show-reachable=yes --num-callers=1000 --suppressions=$TESTS_DIR/lixa.supp --gen-suppressions=all $TESTS_SRC_DIR/$PGM $*
		libtool --mode=execute valgrind --leak-check=full --show-reachable=yes --num-callers=1000 --suppressions=$TESTS_DIR/lixa.supp $TESTS_SRC_DIR/$PGM $*
	else
		$PGM $*
	fi
	rc=$?
	return $rc
}
