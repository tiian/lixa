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
#ifndef LIXA_INST_CONF_H
# define LIXA_INST_CONF_H



#include <config.h>
\


/* save old LIXA_TRACE_MODULE and set a new value */
#ifdef LIXA_TRACE_MODULE
# define LIXA_TRACE_MODULE_SAVE LIXA_TRACE_MODULE
# undef LIXA_TRACE_MODULE
#else
# undef LIXA_TRACE_MODULE_SAVE
#endif /* LIXA_TRACE_MODULE */
#define LIXA_TRACE_MODULE      LIXA_TRACE_MOD_NO_TRACE



/**
 * The root of the installation path for Lixa; this is true only if the
 * software has not been manually relocated to a different filesystem position
 */
extern const char *LIXA_INSTALL_BASE_PATH;

/**
 * This is the path of the system server config file to search for (install
 * configuration)
 */
extern const char *LIXA_SERVER_CONFIG_SYSTEM_FILE;

/**
 * This is the path of the system client config file to search for (install
 * configuration)
 */
extern const char *LIXA_CLIENT_CONFIG_SYSTEM_FILE;

/**
 * Name of the package as set inside configure.ac
 */
extern const char *LIXA_PACKAGE_NAME;

/**
 * Version of the package as set inside configure.ac
 */
extern const char *LIXA_PACKAGE_VERSION;

/**
 * E-mail address as set inside configure.ac
 */
extern const char *LIXA_PACKAGE_BUGREPORT;



#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */



#ifdef __cplusplus
}
#endif /* __cplusplus */



/* restore old value of LIXA_TRACE_MODULE */
#ifdef LIXA_TRACE_MODULE_SAVE
# undef LIXA_TRACE_MODULE
# define LIXA_TRACE_MODULE LIXA_TRACE_MODULE_SAVE
# undef LIXA_TRACE_MODULE_SAVE
#endif /* LIXA_TRACE_MODULE_SAVE */



#endif /* LIXA_INST_CONF_H */

