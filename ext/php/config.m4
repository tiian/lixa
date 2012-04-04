dnl config.m4 for extension lixa

PHP_ARG_WITH(lixa, for LIXA support,
[  --with-lixa=FILE        Include LIXA support. File is the path to lixa-config program])
PHP_ARG_ENABLE(lixa-swig, whether LIXA wrapper should be generated from scratch using SWIG,
[  --enable-lixa-swig      Generate LIXA wrapper using SWIG], no, no)

if test "$PHP_LIXA" != "no"; then
	LIXA_CONFIG=$PHP_LIXA
	dnl --with-lixa -> SWIG is a pre-requisite
	AC_CHECK_PROGS(SWIG, [swig], [])
	if test -z $SWIG
	then
		if test "$PHP_LIXA_SWIG" != "no"
		then
			AC_MSG_ERROR([cannot find swig program, cannot generate LIXA wrapper using SWIG])
		fi
	fi
	dnl use lixa-config to determine LIXA include dir
	AC_MSG_CHECKING([for LIXA using $LIXA_CONFIG])
	LIXA_INCLUDE="$($LIXA_CONFIG --include-dir)"
	if test -r "$LIXA_INCLUDE/tx.h"
	then
		AC_MSG_RESULT(found tx.h in $LIXA_INCLUDE)
		LIXA_DIR="$($PHP_LIXA --prefix)"
	fi
  
	if test -z "$LIXA_DIR"
	then
		AC_MSG_RESULT([not found])
		AC_MSG_ERROR([Please specify a valid lixa-config utility program or reinstall the LIXA distribution])
	fi

	dnl # --with-lixa -> add include path
	PHP_ADD_INCLUDE($($LIXA_CONFIG --include-dir))
	AC_MSG_CHECKING([if LIXA is compiled with PostgreSQL support])
	tmp=$($LIXA_CONFIG --include-dir-postgresql 2>/dev/null)
	if test $? -eq 0
	then
		PHP_ADD_INCLUDE($tmp)
		HAVE_LIXA_POSTGRESQL=1
		AC_DEFINE([HAVE_LIXA_POSTGRESQL], [1], [Define to 1 if LIXA is compiled with PostgreSQL support])
		AC_MSG_RESULT([yes])
	else
		HAVE_LIXA_POSTGRESQL=0
		AC_MSG_RESULT([no])
	fi
	AC_MSG_CHECKING([if LIXA is compiled with MySQL support])
	tmp=$($LIXA_CONFIG --include-dir-mysql 2>/dev/null)
	if test $? -eq 0
	then
		PHP_ADD_INCLUDE($tmp)
		HAVE_LIXA_MYSQL=1
		AC_DEFINE([HAVE_LIXA_MYSQL], [1], [Define to 1 if LIXA is compiled with MySQL support])
		AC_MSG_RESULT([yes])
	else
		HAVE_LIXA_MYSQL=0
		AC_MSG_RESULT([no])
	fi

	PHP_NEW_EXTENSION(lixa, lixa.c, $ext_shared)

	dnl # --enable-lixa-swig -> use SWIG to generare the wrapper from 
	dnl #                       scratch
	if test "$PHP_LIXA_SWIG" != "no"
	then
		dnl # --with-lixa -> dynamically generate lixa.i interface 
		dnl #                file for SWIG
		LIXA_INTERFACE="$ext_srcdir/lixa.i"
		echo "%module lixa" > $LIXA_INTERFACE
		echo "%{" >> $LIXA_INTERFACE
		echo "#include \"tx.h\"" >> $LIXA_INTERFACE
		if test $HAVE_LIXA_POSTGRESQL -eq 1
		then
			echo "#include \"lixapq.h\"" >> $LIXA_INTERFACE
		fi	
		if test $HAVE_LIXA_MYSQL -eq 1
		then
			echo "#include \"lixamy.h\"" >> $LIXA_INTERFACE
		fi	
		echo "%}" >> $LIXA_INTERFACE
		echo "%include \"tx.h\"" >> $LIXA_INTERFACE
		if test $HAVE_LIXA_POSTGRESQL -eq 1
		then
			echo "%include \"lixapq.h\"" >> $LIXA_INTERFACE
		fi	
		if test $HAVE_LIXA_MYSQL -eq 1
		then
			echo "%include \"lixamy.h\"" >> $LIXA_INTERFACE
		fi
		dnl # --with-lixa -> building LIXA interface using SWIG
		AC_MSG_CHECKING([if LIXA wrapper can be created with SWIG])
		$SWIG -php -outdir $ext_srcdir -I$LIXA_INCLUDE -o $ext_srcdir/lixa.c $LIXA_INTERFACE
		if test $? -eq 0
		then
			AC_MSG_RESULT([yes])
		else
			AC_MSG_ERROR([unable to create LIXA php wrapper using SWIG])
		fi 
	fi

	dnl # --with-lixa -> dynamically generate lixa_php.h
	LIXA_PHP_H="$ext_srcdir/lixa_php_config.h"
	cat > $LIXA_PHP_H <<EOF
/*
 * Copyright (c) 2009-2012, Christian Ferrari <tiian@users.sourceforge.net>
 * All rights reserved.
 *
 * This file is part of LIXA.
 *
 * LIXA is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License version 2 as published
 * by the Free Software Foundation.
 *
 * LIXA is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with LIXA.  If not, see <http://www.gnu.org/licenses/>.
 */
#ifndef LIXA_PHP_H
# define LIXA_PHP_H
EOF
	if test $HAVE_LIXA_POSTGRESQL -eq 1
	then
		echo "/* LIXA was build with PostgreSQL support */" >> $LIXA_PHP_H
		echo "# define HAVE_LIXA_POSTGRESQL 1" >> $LIXA_PHP_H
	else
		echo "/* LIXA was not build with PostgreSQL support */" >> $LIXA_PHP_H
		echo "# undef HAVE_LIXA_POSTGRESQL" >> $LIXA_PHP_H
	fi	
	if test $HAVE_LIXA_MYSQL -eq 1
	then
		echo "/* LIXA was build with MySQL support */" >> $LIXA_PHP_H
		echo "# define HAVE_LIXA_MYSQL 1" >> $LIXA_PHP_H
	else
		echo "/* LIXA was not build with MySQL support */" >> $LIXA_PHP_H
		echo "# undef HAVE_LIXA_MYSQL" >> $LIXA_PHP_H
	fi	
	echo "#endif /* LIXA_PHP_H */" >> $LIXA_PHP_H
	
	dnl # --with-lixa -> check for lib and symbol presence
	dnl LIXA client default library
	LIBNAME=lixac
	LIBSYMBOL=tx_open
	PHP_CHECK_LIBRARY($LIBNAME,$LIBSYMBOL,
	[
		PHP_ADD_LIBRARY_WITH_PATH($LIBNAME, $($PHP_LIXA --lib-dir), LIXA_SHARED_LIBADD)
		AC_DEFINE(HAVE_LIXALIB,1,[ ])
	],[
		AC_MSG_ERROR([wrong lixac lib version or lib not found])
	],[
		$($PHP_LIXA --libs)
	])
	if test $HAVE_LIXA_POSTGRESQL -eq 1
	then
		dnl LIXA & PostgreSQL specific library
		LIBNAME=lixapq
		LIBSYMBOL=lixa_pq_get_conn
		PHP_CHECK_LIBRARY($LIBNAME,$LIBSYMBOL,
		[
			PHP_ADD_LIBRARY_WITH_PATH($LIBNAME, $($PHP_LIXA --lib-dir), LIXA_SHARED_LIBADD)
			AC_DEFINE(HAVE_LIXALIB,1,[ ])
		],[
			AC_MSG_ERROR([wrong lixac lib version or lib not found])
		],[
			$($PHP_LIXA --libs-postgresql)
		])
	fi	
	if test $HAVE_LIXA_MYSQL -eq 1
	then
		dnl LIXA & MySQL specific library
		LIBNAME=lixamy
		LIBSYMBOL=lixa_my_get_conn
		PHP_CHECK_LIBRARY($LIBNAME,$LIBSYMBOL,
		[
			PHP_ADD_LIBRARY_WITH_PATH($LIBNAME, $($PHP_LIXA --lib-dir), LIXA_SHARED_LIBADD)
			AC_DEFINE(HAVE_LIXALIB,1,[ ])
		],[
			AC_MSG_ERROR([wrong lixac lib version or lib not found])
		],[
			$($PHP_LIXA --libs-mysql)
		])
	fi
	dnl
	AC_DEFINE(HAVE_LIXA, 1, [Whether you have LIXA])
	PHP_SUBST(LIXA_SHARED_LIBADD)
fi
