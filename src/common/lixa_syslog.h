/*
 * Copyright (c) 2009-2010, Christian Ferrari <tiian@users.sourceforge.net>
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
#ifndef LIXA_SYSLOG_H
# define LIXA_SYSLOG_H



#include <lixa_defines.h>



/* This file contains only syslog messages: they are specified as macros
   because every message must be used only once inside the source code;
   used prefixes:
   LXC: LiXa Client
   LXD: LiXa Daemon (server)
   used messages code:
   D: LOG_DEBUG
   I: LOG_INFO
   N: LOG_NOTICE
   W: LOG_WARNING
   E: LOG_ERR
   C: LOG_CRIT
*/
#define LIXA_SYSLOG_LXC000I "LXC000I this process is starting a new LIXA " \
    "transaction manager"
#define LIXA_SYSLOG_LXC001E "LXC001E transaction '%s' can NOT be recovered " \
    "because current config digest '%s' does NOT match server stored config " \
    "digest '%s'"
#define LIXA_SYSLOG_LXC002E "LXC002E unable to connect to LIXA server at " \
    "address %s, port " IN_PORT_T_FORMAT
#define LIXA_SYSLOG_LXC003C "LXC003C resource manager '%s' returned an " \
    "error (%d) while committing (xa_commit) during recovery phase for " \
    "transaction '%s'"
#define LIXA_SYSLOG_LXC004C "LXC004C resource manager '%s' returned an " \
    "error (%d) while rolling back (xa_rollback) during recovery phase for " \
    "transaction '%s'"



#define LIXA_SYSLOG_LXD000N "LXD000N this process is starting a new LIXA " \
    "server"
#define LIXA_SYSLOG_LXD001E "LXD001E failed to parse options: %s"
#define LIXA_SYSLOG_LXD002W "LXD002W dump option overrides daemon option"
#define LIXA_SYSLOG_LXD003E "LXD003E configuration error ('%s'), premature " \
    "exit"
#define LIXA_SYSLOG_LXD004E "LXD004E error (%s) while starting manager(s), " \
    "premature exit"
#define LIXA_SYSLOG_LXD005E "LXD005E error (%s) while starting listener(s), " \
    "premature exit"
#define LIXA_SYSLOG_LXD006N "LXD006N this LIXA server is terminating"
#define LIXA_SYSLOG_LXD007W "LXD007W first status file ('%s') did not pass " \
    "integrity check"
#define LIXA_SYSLOG_LXD008W "LXD008W second status file ('%s') did not pass " \
    "integrity check"
#define LIXA_SYSLOG_LXD009E "LXD009E both status files did not pass " \
    "integrity check; the server can not start-up"
#define LIXA_SYSLOG_LXD010C "LXD010C memory mapped status files are " \
    "different after copy. INTERNAL ERROR"
#define LIXA_SYSLOG_LXD011W "LXD011W a client is performing recovery " \
    "but config file changed in the meantime for job '%s' and transaction " \
    "'%s'"


#define LIXA_SYSLOG_LXR000I "LXR000I LIXA recovery process is starting"
#define LIXA_SYSLOG_LXR001E "LXR001E failed to parse options: %s"
#define LIXA_SYSLOG_LXR002I "LXR002I this LIXA recovery process is terminating"



#endif /* LIXA_SYSLOG_H */
