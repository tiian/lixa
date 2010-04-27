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
start_server() {
    lixad --daemon --config-file=$TESTS_ETC_DIR/lixad_conf.xml
}

stop_server() {
    kill $(cat $TESTS_VAR_DIR/run.pid)
}


exec_test() {
    echo "Starting LIXA server"
    start_server
    echo "Starting case test $1"
    $1
    rc=$?
    echo "Stopping LIXA server"
    stop_server
    return $rc
}
