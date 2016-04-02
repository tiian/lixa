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


export LIXA_TRACE_MASK=0xffffffff


# This file contains utility functions for test purpouses
reset_server() {
	echo "Resetting LIXA server status"
	rm -rf $TESTS_VAR_DIR
	mkdir $TESTS_VAR_DIR
}

start_server() {
	PWD=$(pwd)
	SERVER_ALREADY_ACTIVE="yes"
	for i in 1 2 3 4 5
	do
 		echo "Checking LIXA server is not active; count=$i"
		pgrep lixad
		if [ $? -eq 1 ]
		then
			SERVER_ALREADY_ACTIVE="no"
			break;
		fi
		sleep 1
	done
	if [ "$SERVER_ALREADY_ACTIVE" = "yes" ]
	then
		echo "LIXA server is already running, unable to start it..."
		exit 1
	fi
	echo "Starting LIXA server"

	REAL_CHECK_TYPE="$SERVER_CHECK_TYPE"
	#using gdb/valgrind in crash test simulation is foolish
	if [ "x$LIXA_CRASH_POINT" != "x" ]
	then
		REAL_CHECK_TYPE=""
	fi
	LIXAD_CONF_XML="lixad_conf.xml"
	if [ "x$LIXA_SYNC_NODELAY" != "x" ]
	then
		LIXAD_CONF_XML="lixad_conf2.xml"
	fi
	echo "SERVER/REAL_CHECK_TYPE=$REAL_CHECK_TYPE"
	echo "SERVER/VALGRIND=$VALGRIND"
	echo "SERVER/GDB=$GDB"
	echo "SERVER/LIXA_SYNC_NODELAY=$LIXA_SYNC_NODELAY"
	echo "SERVER/LIXAD_CONF_XML=$LIXAD_CONF_XML"
	if [ "$REAL_CHECK_TYPE" = "memory" ]
	then
		if [ "x$VALGRIND" != "x" ] 
		then
			export G_SLICE=always-malloc
			echo libtool --mode=execute $VALGRIND --leak-check=full --show-reachable=yes --num-callers=50 --suppressions=$TESTS_DIR/lixad.supp --gen-suppressions=all $SERVER_DIR/lixad --daemon --config-file=$TESTS_ETC_DIR/${LIXAD_CONF_XML} --trace-file=$PWD/lixad.trace
			libtool --mode=execute $VALGRIND --leak-check=full --show-reachable=yes --num-callers=50 --suppressions=$TESTS_DIR/lixad.supp --gen-suppressions=all $SERVER_DIR/lixad --daemon --config-file=$TESTS_ETC_DIR/${LIXAD_CONF_XML} --trace-file=$PWD/lixad.trace 2>>$PWD/lixad.trace
		else
			echo $SERVER_DIR/lixad --daemon --config-file=$TESTS_ETC_DIR/${LIXAD_CONF_XML} --trace-file=$PWD/lixad.trace
			$SERVER_DIR/lixad --daemon --config-file=$TESTS_ETC_DIR/${LIXAD_CONF_XML} --trace-file=$PWD/lixad.trace 2>>$PWD/lixad.trace
		fi
	elif [ "$REAL_CHECK_TYPE" = "thread" ]
	then
		if [ "x$VALGRIND" != "x" ] 
		then
			echo libtool --mode=execute $VALGRIND --tool=helgrind --num-callers=50 --suppressions=$TESTS_DIR/lixad.supp --gen-suppressions=all $SERVER_DIR/lixad --daemon --config-file=$TESTS_ETC_DIR/${LIXAD_CONF_XML} --trace-file=$PWD/lixad.trace
			libtool --mode=execute $VALGRIND --tool=helgrind --num-callers=50 --suppressions=$TESTS_DIR/lixad.supp --gen-suppressions=all $SERVER_DIR/lixad --daemon --config-file=$TESTS_ETC_DIR/${LIXAD_CONF_XML} --trace-file=$PWD/lixad.trace 2>>$PWD/lixad.trace
		else
			echo $SERVER_DIR/lixad --daemon --config-file=$TESTS_ETC_DIR/${LIXAD_CONF_XML} --trace-file=$PWD/lixad.trace
			$SERVER_DIR/lixad --daemon --config-file=$TESTS_ETC_DIR/${LIXAD_CONF_XML} --trace-file=$PWD/lixad.trace 2>>$PWD/lixad.trace
		fi
	elif [ "$REAL_CHECK_TYPE" = "debug" ]
	then
		if [ "x$GDB" != "x" ] 
		then
			TMPFILE=/tmp/lixad_$$.gdb
			echo "run --daemon --config-file=$TESTS_ETC_DIR/${LIXAD_CONF_XML} --trace-file=$PWD/lixad.trace" > $TMPFILE
			echo "bt" >> $TMPFILE
			echo libtool --mode=execute $GDB -batch -x $TMPFILE $SERVER_DIR/lixad
			libtool --mode=execute $GDB -batch -x $TMPFILE $SERVER_DIR/lixad 2>>$PWD/lixad.trace
			rm $TMPFILE
		else
			echo $SERVER_DIR/lixad --daemon --config-file=$TESTS_ETC_DIR/${LIXAD_CONF_XML} --trace-file=$PWD/lixad.trace
			$SERVER_DIR/lixad --daemon --config-file=$TESTS_ETC_DIR/${LIXAD_CONF_XML} --trace-file=$PWD/lixad.trace 2>>$PWD/lixad.trace
		fi
	else
		echo $SERVER_DIR/lixad --daemon --config-file=$TESTS_ETC_DIR/${LIXAD_CONF_XML} --trace-file=$PWD/lixad.trace
		$SERVER_DIR/lixad --daemon --config-file=$TESTS_ETC_DIR/${LIXAD_CONF_XML} --trace-file=$PWD/lixad.trace 2>>$PWD/lixad.trace
	fi
	# wait until the server opens the listening socket
	while test $(netstat -nta | grep LISTEN | grep 127.0.0.1:2345 | wc -l) -le 0
	do
		echo "Waiting server socket listening..."
		sleep 1
	done
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
	# check lixar link
	echo "LD_LIBRARY_PATH=$LD_LIBRARY_PATH"
	if [ ! -h $TESTS_SRC_DIR/lixar ]
	then
		ln -s $CLIENT_DIR/lixar $TESTS_SRC_DIR/lixar
	fi
	PGM=$1
	shift
	REAL_CHECK_TYPE="$CLIENT_CHECK_TYPE"
	#using gdb/valgrind in crash test simulation is foolish
	if [ "x$LIXA_CRASH_POINT" != "x" ]
	then
		REAL_CHECK_TYPE=""
	fi
	echo "CLIENT/REAL_CHECK_TYPE=$REAL_CHECK_TYPE"
	echo "CLIENT/VALGRIND=$VALGRIND"
	echo "CLIENT/GDB=$GDB"
	if [ "$REAL_CHECK_TYPE" = "memory" ]
	then
		if [ "x$VALGRIND" != "x" ] 
		then
			export G_SLICE=always-malloc
			echo libtool --mode=execute $VALGRIND --leak-check=full --show-reachable=yes --num-callers=50 --suppressions=$TESTS_DIR/lixac.supp --gen-suppressions=all $TESTS_SRC_DIR/$PGM $*
			libtool --mode=execute $VALGRIND --leak-check=full --show-reachable=yes --num-callers=50 --suppressions=$TESTS_DIR/lixac.supp --gen-suppressions=all $TESTS_SRC_DIR/$PGM $*
		else
			echo $PGM $*
			$PGM $*
		fi
	elif [ "$REAL_CHECK_TYPE" = "thread" ]
	then
		if [ "x$VALGRIND" != "x" ] 
		then
			echo libtool --mode=execute $VALGRIND --tool=helgrind --num-callers=50 --gen-suppressions=all $TESTS_SRC_DIR/$PGM $*
			libtool --mode=execute $VALGRIND --tool=helgrind --num-callers=50 --gen-suppressions=all $TESTS_SRC_DIR/$PGM $*
		else
			echo $PGM $*
			$PGM $*
		fi
	elif [ "$REAL_CHECK_TYPE" = "debug" ]
	then
		if [ "x$GDB" != "x" ] 
		then
			TMPFILE=/tmp/lixac_$$.gdb
			echo "run $*" > $TMPFILE
			echo "bt" >> $TMPFILE
			echo libtool --mode=execute $GDB -batch -x $TMPFILE $TESTS_SRC_DIR/$PGM
			libtool --mode=execute $GDB -batch -x $TMPFILE $TESTS_SRC_DIR/$PGM $*
			rm $TMPFILE
		else
			echo $PGM $*
			$PGM $*
		fi
	else
		$PGM $*
	fi
	rc=$?
	return $rc
}
