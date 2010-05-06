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
    "transaction manager (%s package version is %s)"
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
#define LIXA_SYSLOG_LXC005W "LXC005W unable to recover transaction id '%s'; " \
    "this transaction must be manually recovered and the correlated " \
    "record(s) must be manually fixed in lixad server status file"
#define LIXA_SYSLOG_LXC006N "LXC006N resource manager '%s' returned " \
    "XA_RDONLY (%d) while committing (xa_commit) during recovery phase for " \
    "transaction '%s'"
#define LIXA_SYSLOG_LXC007N "LXC007N resource manager '%s' returned " \
    "XA_RDONLY (%d) while rolling back (xa_rollback) during recovery phase " \
    "for transaction '%s'"
#define LIXA_SYSLOG_LXC008I "LXC008I resource manager '%s' returned " \
    "XAER_NOTA during recovery commit: the resource manager already " \
    "committed the transaction with xid '%s'"



#define LIXA_SYSLOG_LXD000N "LXD000N this process is starting a new LIXA " \
    "server (%s package version is %s)"
#define LIXA_SYSLOG_LXD001E "LXD001E failed to parse options: %s"
#define LIXA_SYSLOG_LXD002W "LXD002W dump option overrides daemon option"
#define LIXA_SYSLOG_LXD003E "LXD003E configuration error ('%s'), premature " \
    "exit"
#define LIXA_SYSLOG_LXD004E "LXD004E error (%s) while starting manager(s), " \
    "premature exit"
#define LIXA_SYSLOG_LXD005E "LXD005E error (%s) while starting listener(s), " \
    "premature exit"
#define LIXA_SYSLOG_LXD006N "LXD006N this LIXA server ended"
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
#define LIXA_SYSLOG_LXD012W "LXD012W a client notified recovery failed " \
    "condition for the transaction registered in status file %d and block " \
    UINT32_T_FORMAT
#define LIXA_SYSLOG_LXD013W "LXD013W unable to switch client; recovery not " \
    "performed"
#define LIXA_SYSLOG_LXD014N "LXD014N LIXA server entered daemon status"
#define LIXA_SYSLOG_LXD015W "LXD015W unable to open pid file '%s'"
#define LIXA_SYSLOG_LXD016E "LXD016E config file error: unable to find property '%s' in tag '%s'"
#define LIXA_SYSLOG_LXD017E "LXD017E intercommunication pipes initialization error"



#define LIXA_SYSLOG_LXR000I "LXR000I LIXA recovery process is starting " \
    "(%s package version is %s)"
#define LIXA_SYSLOG_LXR001E "LXR001E failed to parse options: %s"
#define LIXA_SYSLOG_LXR002I "LXR002I this LIXA recovery process is terminating"
#define LIXA_SYSLOG_LXR003E "LXR003E unable to perform tx_open() (rc=%d)"
#define LIXA_SYSLOG_LXR004W "LXR004W resource manager returned %d while performing cold commit recovery, xa_forget will be issued to clean-up transaction '%s'"
#define LIXA_SYSLOG_LXR005W "LXR005W resource manager returned %d while performing cold rollback recovery, xa_forget will be issued to clean-up transaction '%s'"



#endif /* LIXA_SYSLOG_H */
