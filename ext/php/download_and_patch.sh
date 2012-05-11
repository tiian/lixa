#!/bin/sh
#
# Copyright (c) 2009-2012, Christian Ferrari <tiian@users.sourceforge.net>
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

#
# This script automates PHP download and patching
#

# First argument is optional (PHP version)
# extract PHP version if specified on the command line
if test $# -gt 0
then
	PHP_VERSION="$1"
	echo $PHP_VERSION | grep -E '^[[:digit:]]+\.[[:digit:]]+\.[[:digit:]]$'
	if test $? -ne 0
	then
		echo "Sorry, $PHP_VERSION does not seems a valid X.Y.Z PHP version"
		exit 1
	fi
else
	PHP_VERSION="5.4.3"
fi
echo "PHP $PHP_VERSION will be downloaded..."

# Checking for wget command
echo -n "Checking if wget command is available... "
type wget
if test $? -ne 0
then
	echo "Sorry, this script need wget to retrieve PHP source code"
	exit 1
fi

# Checking for patch command
echo -n "Checking if patch command is available... "
type patch
if test $? -ne 0
then
	echo "Sorry, this script need patch to patch PHP source code"
	exit 1
fi

# Type of archive to download: we are using the correlated tar flag
DEFLATE_FLAG=""

# Cheching for bunzip2 command
echo -n "Checking if bunzip2 is available... "
type bunzip2
if test $? -ne 0
then
	# Cheching for gunzip command
	echo -n "bunzip2 is not available, checking if gunzip is available... "
	type gunzip
	if test $? -ne 0
	then
		echo "Sorry, this script need bunzip2 or gunzip to deflate PHP source code"
		exit 1
	else
		DEFLATE_FLAG="z"
	fi
else
	DEFLATE_FLAG="j"
fi

# Source code archive file name
DIR_NAME="php-${PHP_VERSION}"
ARCHIVE_NAME=""
case "$DEFLATE_FLAG" in
	j) ARCHIVE_NAME="${DIR_NAME}.tar.bz2"
	;;
	z) ARCHIVE_NAME="${DIR_NAME}.tar.gz"
	;;
	*)
	echo "DEFLATE_FLAG=$DEFLATE_FLAG, unable to determine ARCHIVE_NAME"
	exit 1
	;;
esac

# Retrieve the tarball if it's not available
if test -f $ARCHIVE_NAME
then
	echo "File $ARCHIVE_NAME already available, bypassing download."
	echo "NOTE: if you want to download it again, please remove the current tarball ($ARCHIVE_NAME) and restart the script."
else
	wget -O $ARCHIVE_NAME http://docs.php.net/get/$ARCHIVE_NAME/from/docs.php.net/mirror
	if test $? -ne 0
	then
		echo "Error while downloading PHP source code"
		exit 1
	fi
fi

# Extracting source code from tarball if the dir is not available
echo "Deflating PHP source code..."
if test -d $DIR_NAME
then
	echo "Directory $DIR_NAME is already in place, bypassing tarball expansion."
	echo "NOTE: if you want to extract the source code again, please remove the current directory ($DIR_NAME) and restart the script."
else
	tar -x${DEFLATE_FLAG}f $ARCHIVE_NAME
	if test $? -ne 0
	then
		echo "Error while deflating PHP source code"
		exit 1
	fi
fi

# Creating LIXA extension
echo "Creating LIXA extension directory..."
LIXA_DIR=$DIR_NAME/ext/lixa
mkdir -pv $LIXA_DIR
if test $? -ne 0
then
	echo "Error while creating directory $LIXA_DIR"
	exit 1
fi
cp -av config.m4 lixa.c php_lixa.h $LIXA_DIR
if test $? -ne 0
then
	echo "Error while copying LIXA sources inside $LIXA_DIR"
	exit 1
fi

# Create and populate LIXA tests directory
mkdir -pv $LIXA_DIR/tests
if test $? -ne 0
then
	echo "Error while creating directory $LIXA_DIR/tests"
	exit 1
fi
cp -av tests/* $LIXA_DIR/tests

# Dive into PHP source code
cd $DIR_NAME
if test $? -ne 0
then
	echo "Error while diving into directory $DIR_NAME"
	exit 1
fi

# Checking PHP source code patches
PATCH_LIST=$(ls -r ../lixa-php-patch-???.diff)
PATCHABLE="no"
for PATCH in $PATCH_LIST
do
	echo "Trying patch $PATCH"
	patch --dry-run -p1 < $PATCH
	if test $? -eq 0
	then
		echo "Patch $PATCH can be applied to $DIR_NAME, going on..."
		PATCHABLE="yes"
		break
	else
		echo "Patch $PATCH can NOT be applied to $DIR_NAME, trying another one (if available)..."
	fi
done

if test "$PATCHABLE" != "yes"
then
	echo "Version $DIR_NAME is NOT patchable, please open a case on"
	echo "http://lixa.sourceforge.net/"
	exit 1
fi

# Patching PHP source code
echo "Patching PHP source code..."
patch -p1 < $PATCH
if test $? -ne 0
then
	echo "The PHP source code in $DIR_NAME can not be patched..."
	exit 1
else
	echo "PHP source code successfully patched!"
fi

# Coming back to parent dir
cd ..

# Cleaning build environment
echo "Removing $DIR_NAME/configure and some stuff..."
rm -vf $DIR_NAME/configure $DIR_NAME/autom4te.cache/*

# Creating configure again
echo "Creating new configure script..."
cd $DIR_NAME
./buildconf --force
if test $? -ne 0
then
	echo "Error while creating the new configure script"
	exit 1
fi

# Checking the PHP build environment contains LIXA as well
echo "Checking configure recognize LIXA extension..."
./configure --help | grep lixa
if test $? -ne 0
then
	echo "ERROR: PHP build environment did not linked LIXA extension..."
	exit 1
fi
cd ..

# Build environment is now ready
echo
echo "************************************************************************"
echo "  PHP source code in directory $DIR_NAME is ready for build and deploy;"
echo ""
echo "  use --with-lixa=/opt/lixa/bin/lixa-config (or different path if you"
echo "  not using LIXA default path) to enable LIXA extension for PHP"
echo ""
echo "  use --enable-lixa-swig if you want to generate LIXA PHP wrapper from"
echo "  scratch using SWIG software (this should not be necessary most times)"
echo "************************************************************************"
exit 0
