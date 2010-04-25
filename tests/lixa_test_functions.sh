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
    echo PATH=$PATH
    lixad --daemon --config-file=$TESTS_ETC_DIR/lixad_conf.xml
    LIXAD_PID=$$
    echo $LIXAD_PID
}

stop_server() {
    kill $LIXAD_PID
}


exec_test() {
    start_server
    stop_server
}
