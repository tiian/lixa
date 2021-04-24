#!/bin/sh
#
# Copyright (c) 2009-2021, Christian Ferrari <tiian@users.sourceforge.net>
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

# Damage the portion of second state table files from byte 100 to byte 200

TMP_FILE=$TESTS_TMP_DIR/first_part
for f in $(ls $TESTS_VAR_DIR/*_[12].table)
do
	echo "Damaging file $f"
	# picking up 100 bytes
	dd if=$f of=$TMP_FILE bs=1 count=100
	# append 100 random bytes
	dd if=/dev/urandom bs=1 count=100 >> $TMP_FILE
	# overwriting first part of the state table file
	dd if=$TMP_FILE of=$f conv=notrunc
done

