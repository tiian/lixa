/*
 * Copyright (c) 2009-2023, Christian Ferrari <tiian@users.sourceforge.net>
 * All rights reserved.
 *
 * This file is part of LIXA.
 *
 * LIXA is free software: you can redistribute this file and/or modify
 * it under the terms of the GNU Lesser General Public License version 2.1 as
 * published by the Free Software Foundation.
 *
 * LIXA is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with LIXA.  If not, see <http://www.gnu.org/licenses/>.
 */
#include <config.h>



#ifdef HAVE_STDLIB_H
# include <stdlib.h>
#endif
#ifdef HAVE_STRING_H
# include <string.h>
#endif
#ifdef HAVE_GLIB_H
# include <glib.h>
#endif
#ifdef HAVE_GMODULE_H
# include <gmodule.h>
#endif
#ifdef HAVE_PTHREAD_H
# include <pthread.h>
#endif
#ifdef HAVE_REGEX_H
# include <regex.h>
#endif



/* XA standard header */
#include "xa.h"
/* MySQL front-end */
#include <mysql.h>
/* MySQL XA wrapper provided by LIXA (private header) */
#include "liblixamy.h"
/* MySQL XA wrapper provided by LIXA (public header) */
#include "lixamy.h"
/* LIXA standard trace library: it could be removed if "TRACE" calls would
 * be removed from this source */
#include "lixa_trace.h"
/* LIXA convenience macros: it could be removed if "TRY/CATCH" statement
 * would be removed from this source */
#include "lixa_defines.h"
/* LIXA xid: it contains XID serialization/deserialization utilities */
#include "lixa_xid.h"
#include "lixa_syslog.h"



/* set module trace flag */
#ifdef LIXA_TRACE_MODULE
# undef LIXA_TRACE_MODULE
#endif /* LIXA_TRACE_MODULE */
#define LIXA_TRACE_MODULE   LIXA_TRACE_MOD_CLIENT_XA_SWITCH



struct xa_switch_t xamyls = {
    "MySQL[LIXA]",
    TMNOMIGRATE,
    0,
    lixa_my_open,
    lixa_my_close,
    lixa_my_start,
    lixa_my_end,
    lixa_my_rollback,
    lixa_my_prepare,
    lixa_my_commit,
    lixa_my_recover,
    lixa_my_forget,
    lixa_my_complete
};



const char *MYSQL_XA_RECOVER_STMT = "XA RECOVER CONVERT XID";
const char *MARIADB_XA_RECOVER_STMT= "XA RECOVER FORMAT='SQL'";



const gchar *g_module_check_init(GModule *module)
{
    LIXA_TRACE(("lixa_my/g_module_check_init: initializing module\n"));
    return NULL;
}



void g_module_unload(GModule *module)
{
    LIXA_TRACE(("lixa_my/g_module_unload: releasing local storage\n"));
    /* lock the status mutex */
    g_mutex_lock(&lixa_sw_status_mutex);

    if (NULL != lixa_sw_status) {
        g_hash_table_destroy(lixa_sw_status);
        lixa_sw_status = NULL;
    }
    
    /* unlock the status mutex */
    g_mutex_unlock(&lixa_sw_status_mutex);
}



int lixa_my_parse_xa_info(const char *xa_info,
                          struct lixa_mysql_real_connect_s *lmrc)
{
    enum Exception { BUFFER_OVERFLOW
                     , SYNTAX_ERROR1
                     , PARSE_KEY_VALUE1
                     , SYNTAX_ERROR2
                     , INTERNAL_ERROR
                     , PARSE_KEY_VALUE2
                     , NONE } excp;
    int xa_rc = XAER_RMERR;
    size_t i = 0, l;
    char key[MAXINFOSIZE], value[MAXINFOSIZE];
    int k, v;
    enum State { ST_KEY, ST_VALUE } state;
    enum Token { TK_SEPAR, TK_ASSIGN, TK_CHAR } token;
    
    LIXA_TRACE(("lixa_my_parse_xa_info\n"));
    TRY {
        /* check the string is not a buffer overflow attack */
        if (MAXINFOSIZE <= (l = strlen(xa_info))) {
            LIXA_TRACE(("lixa_my_parse_xa_info: xa_info is too long ("
                        SIZE_T_FORMAT ")\n", l));
            THROW(BUFFER_OVERFLOW);
        }
        LIXA_TRACE(("lixa_my_parse_xa_info: parsing '%s'\n", xa_info));
        /* reset the struct */
        memset(lmrc, 0, sizeof(struct lixa_mysql_real_connect_s));
        lmrc->host = lmrc->user = lmrc->passwd = lmrc->db = lmrc->unix_socket =
            NULL;
        state = ST_KEY;
        k = v = 0;
        while (i<l) {
            char current, next;
            int escaping = FALSE;
            current = xa_info[i];
            if (i<l-1)
                next = xa_info[i+1];
            else
                next = '\0';
            /* recognize double separator or double assignator (they are
               escaping sequences) */
            if ((LIXA_MYSQL_XA_INFO_SEPARATOR == current &&
                 LIXA_MYSQL_XA_INFO_SEPARATOR == next) ||
                (LIXA_MYSQL_XA_INFO_ASSIGN == current &&
                 LIXA_MYSQL_XA_INFO_ASSIGN == next))
                escaping = TRUE;
            token = TK_CHAR;
            if (!escaping) {
                if (LIXA_MYSQL_XA_INFO_SEPARATOR == current)
                    token = TK_SEPAR;
                else if (LIXA_MYSQL_XA_INFO_ASSIGN == current)
                    token = TK_ASSIGN;
            }
            switch (token) {
                case TK_SEPAR:
                    if (ST_KEY == state) {
                        LIXA_TRACE(("lixa_my_parse_xa_info: separator '%c' "
                                    "unexpected while parsing a KEY at pos "
                                    SIZE_T_FORMAT " of xa_info '%s'\n",
                                    current, i, xa_info));
                        THROW(SYNTAX_ERROR1);
                    }
                    /* value terminated */
                    value[v] = '\0';
                    state = ST_KEY;
                    k = 0;
                    LIXA_TRACE(("lixa_my_parse_xa_info: (key=value) -> "
                                "(%s=%s)\n", key, value));
                    /* registering key/value in broken down structure */
                    if (XA_OK != (xa_rc = lixa_my_parse_key_value(
                                      lmrc, key, value)))
                        THROW(PARSE_KEY_VALUE1);
                    /* resetting... */
                    key[0] = value[0] = '\0';
                    break;
                case TK_ASSIGN:
                    if (ST_VALUE == state) {
                        LIXA_TRACE(("lixa_my_parse_xa_info: separator '%c' "
                                    "unexpected while parsing a VALUE at pos "
                                    SIZE_T_FORMAT " of xa_info '%s'\n",
                                    current, i, xa_info));
                        THROW(SYNTAX_ERROR2);
                    }
                    /* value terminated */
                    key[k] = '\0';
                    state = ST_VALUE;
                    v = 0;
                    break;
                case TK_CHAR:
                    if (ST_KEY == state) {
                        key[k++] = current;
                        key[k] = '\0'; /* debugging purpose */
                    } else if (ST_VALUE == state) {
                        value[v++] = current;
                        value[v] = '\0'; /* debugging purpose */
                    } else {
                        LIXA_TRACE(("lixa_my_parse_xa_info: unexpected state "
                                    "(%d) while parsing '%c' at pos "
                                    SIZE_T_FORMAT " of xa_info '%s'\n",
                                    state, current, i, xa_info));
                        THROW(INTERNAL_ERROR);
                    }
                default:
                    break;
            } /* switch (token) */
            /* shift one (or two) char(s) */
            if (escaping)
                i = i+2;
            else
                i++;
        }
        LIXA_TRACE(("lixa_my_parse_xa_info: (key=value) -> "
                    "(%s=%s)\n", key, value));
        /* registering key/value in broken down structure */
        if (XA_OK != (xa_rc = lixa_my_parse_key_value(lmrc, key, value)))
            THROW(PARSE_KEY_VALUE2);
        /* display brokendown structure */
        LIXA_TRACE(("lixa_my_parse_xa_info: lmrc[host=%s,user=%s,passwd=%s,"
                    "db=%s,port=%d,unix_socket=%s,client_flag=%ld]\n",
                    lmrc->host != NULL ? lmrc->host : "NULL",
                    lmrc->user != NULL ? lmrc->user : "NULL",
                    lmrc->passwd != NULL ? lmrc->passwd : "NULL",
                    lmrc->db != NULL ? lmrc->db : "NULL",
                    lmrc->port,
                    lmrc->unix_socket != NULL ? lmrc->unix_socket : "NULL",
                    lmrc->client_flag));
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case BUFFER_OVERFLOW:
            case SYNTAX_ERROR1:
            case SYNTAX_ERROR2:
                xa_rc = XAER_INVAL;
                break;
            case PARSE_KEY_VALUE1:
            case PARSE_KEY_VALUE2:
                break;
            case INTERNAL_ERROR:
                xa_rc = XAER_RMERR;
                break;
            case NONE:
                xa_rc = XA_OK;
                break;
            default:
                xa_rc = XAER_RMERR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("lixa_my_parse_xa_info/excp=%d/xa_rc=%d\n", excp, xa_rc));
    return xa_rc;
}



int lixa_my_parse_key_value(struct lixa_mysql_real_connect_s *lmrc,
                            const char *key, const char *value)
{
    enum Exception { KEY_NOT_RECOGNIZED
                     , INTERNAL_ERROR
                     , NONE } excp;
    int xa_rc = XAER_RMERR;
    enum Key { KY_HOST, KY_USER, KY_PASSWD, KY_DB, KY_PORT, KY_UNIX_SOCKET,
               KY_CLIENT_FLAG, KY_NULL } ky;
    
    LIXA_TRACE(("lixa_my_parse_key_value\n"));
    TRY {
        /* recognize key */
        if (!strcmp(key, "host"))
            ky = KY_HOST;
        else if (!strcmp(key, "user"))
            ky = KY_USER;
        else if (!strcmp(key, "passwd"))
            ky = KY_PASSWD;
        else if (!strcmp(key, "db"))
            ky = KY_DB;
        else if (!strcmp(key, "port"))
            ky = KY_PORT;
        else if (!strcmp(key, "unix_socket"))
            ky = KY_UNIX_SOCKET;
        else if (!strcmp(key, "client_flag"))
            ky = KY_CLIENT_FLAG;
        else if (0 == strlen(key))
            ky = KY_NULL;
        else {
            LIXA_TRACE(("lixa_my_parse_key_value: key='%s' is not recognized"
                        "\n", key));
            THROW(KEY_NOT_RECOGNIZED);
        }
        /* move in broken down structure */
        switch (ky) {
            case KY_HOST:
                strcpy(lmrc->host_buffer, value);
                lmrc->host = lmrc->host_buffer;
                break;
            case KY_USER:
                strcpy(lmrc->user_buffer, value);
                lmrc->user = lmrc->user_buffer;
                break;
            case KY_PASSWD:
                strcpy(lmrc->passwd_buffer, value);
                lmrc->passwd = lmrc->passwd_buffer;
                break;
            case KY_DB:
                strcpy(lmrc->db_buffer, value);
                lmrc->db = lmrc->db_buffer;
                break;
            case KY_PORT:
                lmrc->port = strtoul(value, NULL, 0);
                break;
            case KY_UNIX_SOCKET:
                strcpy(lmrc->unix_socket_buffer, value);
                lmrc->unix_socket = lmrc->unix_socket_buffer;
                break;
            case KY_CLIENT_FLAG:
                lmrc->client_flag = strtoul(value, NULL, 0);
                break;
            case KY_NULL:
                LIXA_TRACE(("lixa_my_parse_key_value: WARNING there is a null "
                            "key in xa_open_info string\n"));
                break;
            default:
                LIXA_TRACE(("lixa_my_parse_key_value: ky=%d invalid KY_ value"
                            "\n", ky));
                THROW(INTERNAL_ERROR);
        }
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case KEY_NOT_RECOGNIZED:
                xa_rc = XAER_INVAL;
                break;
            case INTERNAL_ERROR:
                xa_rc = XAER_RMERR;
                break;
            case NONE:
                xa_rc = XA_OK;
                break;
            default:
                xa_rc = XAER_RMERR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("lixa_my_parse_key_value/excp=%d/xa_rc=%d\n", excp, xa_rc));
    return xa_rc;
}


    
int lixa_my_xid_serialize(const XID *xid, lixa_my_ser_xid_t lmsx)
{
    int i = 0, j = 0;
    byte_t *p;
    long len = 2 /* first X' */ +
        xid->gtrid_length*2 /* gtrid */ +
        4 /* ',X' */ +
        xid->bqual_length*2 /* bqual */ +
        2 /* ', */ +
        LIXA_SERIALIZED_LONG_INT /* formatID */ +
        1 /* '\0' terminator */ ;
    /* check the XID can be serialized for MySQL by this routine */
    if (len > sizeof(lixa_my_ser_xid_t)) {
        LIXA_TRACE(("lixa_my_xid_serialize: xid can not be serialized "
                    "because it would need %ld bytes instead of "
                    SIZE_T_FORMAT "\n", len, sizeof(lixa_ser_xid_t)));
        return FALSE;
    }
    /* put the first X-apostrophe */
    lmsx[0] = 'X';
    lmsx[1] = '\'';
    lmsx[2] = '\0';
    /* serialize gtrid */
    j = 2;
    p = (byte_t *)&(xid->data[0]);
    for (i=0; i<xid->gtrid_length; ++i) {
        sprintf(lmsx+j, "%2.2x", p[i]);
        j += 2;
    }
    /* put the second apostrophe */
    *(lmsx+j) = '\'';
    j += 1;
    /* put a comma */
    *(lmsx+j) = ',';
    j += 1;
    /* put the second X */
    *(lmsx+j) = 'X';
    j += 1;
    /* put the third apostrophe */
    *(lmsx+j) = '\'';
    j += 1;
    /* serialize bqual */
    p = (byte_t *)&(xid->data[xid->gtrid_length]);
    for (i=0; i<xid->bqual_length; ++i) {
        sprintf(lmsx+j, "%2.2x", p[i]);
        j += 2;
    }
    /* put the last apostrophe, a comma and formatID */
    /* serialize formatID */
    sprintf(lmsx+j, "',%ld", xid->formatID);

    LIXA_TRACE(("lixa_my_xid_serialize: %s\n", lmsx));
    return TRUE;
}



int lixa_my_xid_deserialize(XID *xid, const char *formatID,
                            const char *gtrid_length, const char *bqual_length,
                            const char *data, int rm_subtype)
{
    long i, s;
    const char *p, *q;
    char tmp[3];
    unsigned int b = 0;
    char stripped_data[XIDDATASIZE*8];
    
    LIXA_TRACE(("lixa_my_xid_deserialize: formatID='%s', gtrid_length='%s', "
                "bqual_length='%s', data='%s', rm_subtype=%d\n",
                formatID, gtrid_length, bqual_length, data, rm_subtype));
    memset(xid, 0, sizeof(XID));
    xid->formatID = strtol(formatID, NULL, 0);
    xid->gtrid_length = strtol(gtrid_length, NULL, 0);
    xid->bqual_length = strtol(bqual_length, NULL, 0);
    s = xid->gtrid_length + xid->bqual_length;
    if (XIDDATASIZE < s) {
        LIXA_TRACE(("lixa_my_xid_deserialize: gtrid_length (%ld) + "
                    "bqual_length (%ld)"
                    "exceeds XIDDATASIZE (%d)\n", xid->gtrid_length,
                    xid->bqual_length, XIDDATASIZE));
        return FALSE;
    }
    // check data is manageable
    if (strlen(data) > sizeof(stripped_data) - 1) {
        LIXA_TRACE(("lixa_my_xid_deserialize: data is too long and "
		"unmanageable, something string is happening: >%s<, buffer "
		"size is " SIZE_T_FORMAT ")\n", data, sizeof(stripped_data)));
	return FALSE;
    }
    // in case of MariaDB the format is like this:
    // X'3b86b218ef234eb7827536da8979f1e4',X'9a3c699a6eb346cd92bba720568f542c',1279875137
    // in case of genuine MySQL the format is like this:
    // 0x04389AC1D8E24F0BB551004C72A8A00A5120360E06828F8ABDC431C5919E09CB
    // MariaDB will be converted to MySQL...
    if (rm_subtype == LIXA_SW_STATUS_RM_SUBTYPE_MARIADB) {
        int state = 0;
	char *q;
        p = data;
	q = stripped_data;
	for (i=0; i<strlen(data); ++i) {
	    switch (state) {
                case 0: // search for first X
		    if (*p == 'X') {
			// move to next state
			state = 1;
			// put '0'
			*q = '0';
			q++;
		    }
		    break;
		case 1: // search for first \'
		    if (*p == '\'') {
		        // move to next state
			state = 2;
			// put 'x'
			*q = 'x';
			q++;
		     }
		     break;
		 case 2: // search for HEX char or \'
                     if (((*p >= '0' && *p <= '9') || (*p >= 'a' && *p <= 'f')
                       || (*p >= 'A' && *p <= 'F'))) {
		         // put it
			 *q = toupper(*p);
			 q++;
		     } else if (*p == '\'') {
		         // found \', move to next state and ignore it
			 state = 3;
		     }
		     break;
		 case 3: // search for comma ','
		     if (*p == ',') {
		         // move to next state, but without putting
			 state = 4;
		     }
		     break;
		 case 4: // search for second X
		     if (*p == 'X') {
		        // move to next state, but without putting
			state = 5;
		     }
		     break;
		 case 5: // search for third \'
		     if (*p == '\'') {
		         // move to next state, but without putting
			 state = 6;
		     }
		     break;
		 case 6: // search for HEX char or \'
                     if (((*p >= '0' && *p <= '9') || (*p >= 'a' && *p <= 'f')
                       || (*p >= 'A' && *p <= 'F'))) {
		         // put it
			 *q = toupper(*p);
			 q++;
		     } else if (*p == '\'') {
		         // found \', move to next state and ignore it
			 state = 7;
		     }
		     break;
		 case 7: // ignore, parsing completed
		     break;
		 default:
                     LIXA_TRACE(("lixa_my_xid_deserialize: char '%c' skipped "
		                 "IT SHOULD NOT HAPPEN!\n", *p));
	    } // switch (state)
            // move forward
	    p++;
	} // for i
	// put a terminator at the end
	*q = '\0';
    } else {
        // just copy
	strncpy(stripped_data, data, sizeof(stripped_data)-1);
	stripped_data[sizeof(stripped_data)-1] = '\0';
    }
    LIXA_TRACE(("lixa_my_xid_deserialize: stripped_data is '%s'\n",
			    stripped_data));
    if (strlen(stripped_data) > s*2+2) { // take in consideration initial 0x and that
        // hex requires 2 chars for a single byte
        LIXA_TRACE(("lixa_my_xid_deserialize: data length ("
                    SIZE_T_FORMAT ") exceeds "
                    "gtrid and bqual length (%ld)\n", strlen(stripped_data), s*2+2));
        return FALSE;
    }
    tmp[2] = '\0';

    // check the data starts with '0x'
    if (stripped_data[0] != '0' || stripped_data[1] != 'x') {
        LIXA_TRACE(("lixa_my_xid_deserialize: data returned does not start "
                    "with prefix '0x' (data='%s')\n", data));
        return FALSE;
    }
    // skip the prefix
    p = stripped_data+2;
    for (i=0; i<s; ++i) {
        q = p+1;
        if (((*p >= '0' && *p <= '9') || (*p >= 'a' && *p <= 'f')
             || (*p >= 'A' && *p <= 'F')) &&
            ((*q >= '0' && *q <= '9') || (*q >= 'a' && *q <= 'f')
             || (*q >= 'A' && *q <= 'F'))) {
            tmp[0] = *p;
            tmp[1] = *q;
            sscanf(tmp, "%x", &b);
            xid->data[i] = b;
        } else {
            LIXA_TRACE(("lixa_my_xid_deserialize: '%s' invalid "
                        "characters found:\n", p));
            return FALSE;
        }
        p += 2;
    }
    return TRUE;
}


int lixa_my_retrieve_subtype(MYSQL *conn, int *rm_subtype, long *version,
                             long *release, long *minor)
{
    enum Exception {
        MYSQL_SELECT_VERSION_ERROR
        , REGCOMP_ERROR
        , REGEX_ERROR
        , UNSUPPORTED_VERSION
        , NONE
    } excp = NONE;
    int ret_cod = TRUE;
    
    MYSQL_RES *result = NULL;
    MYSQL_ROW  row;
    const char reg_str[] = "^([[:digit:]]+)\\.([[:digit:]]+)\\.([[:digit:]]+)\\-([[:alnum:][:punct:]]+)$";
    char       reg_errbuf[500];
    regex_t    reg_expression;
    regmatch_t reg_matches[5];
    
    LIXA_TRACE(("lixa_my_retrieve_subtype\n"));
    TRY {
        int reg_error;
        int i;
        char *text;
//            "5.5.60-MariaDB"
//            "10.3.28-MariaDB"
//            "10.5.27-MariaDB"
//            "5.7.33-0ubuntu0.16.04.1"
//            "5.5.62-0ubuntu0.14.04.1"
        
        /* retrieve MySQL dialect (subtype) */
        if (mysql_query(conn, "SELECT VERSION();")) {
            LIXA_TRACE(("lixa_my_retrieve_subtype: mysql_query returned "
                        "error: %u: %s\n", mysql_errno(conn),
                        mysql_error(conn)));
            mysql_close(conn);
            THROW(MYSQL_SELECT_VERSION_ERROR);
        }
        result = mysql_store_result(conn);
        row = mysql_fetch_row(result);
        text = row[0];
        LIXA_TRACE(("lixa_my_retrieve_subtype: SELECT VERSION() "
                    "returned '%s'\n", text));
        
        /* initialize the regular expression */
        reg_error = regcomp(&reg_expression, reg_str,
                            REG_EXTENDED|REG_NEWLINE);
        if (0 != reg_error) {
            regerror(reg_error, &reg_expression, reg_errbuf,
                     sizeof(reg_errbuf));
            LIXA_TRACE(("lixa_my_retrieve_subtype: regcomp returned %d "
                        "('%s') instead of 0\n", reg_error, reg_errbuf));
            THROW(REGCOMP_ERROR);
        }
            
        /* check the regular expression matches and extract the matches */
        for (i=0; i<sizeof(reg_matches)/sizeof(regmatch_t); ++i)
            reg_matches[i].rm_so = reg_matches[i].rm_eo = -1;
        reg_error = regexec(&reg_expression, text,
                            sizeof(reg_matches)/sizeof(regmatch_t),
                            reg_matches, 0);
        if (0 != reg_error) {
            regerror(reg_error, &reg_expression, reg_errbuf,
                     sizeof(reg_errbuf));
            LIXA_TRACE(("lixa_my_retrieve_subtype: regex returned %d "
                        "('%s') instead of 0\n", reg_error, reg_errbuf));
            LIXA_SYSLOG((LOG_ERR, LIXA_SYSLOG_LXC040E, text));
            THROW(REGEX_ERROR);
        }

        /* trace the results */
        for (i=0; i<sizeof(reg_matches)/sizeof(regmatch_t); ++i) {
            char buffer[1000];
            long number;
            size_t delta = reg_matches[i].rm_eo - reg_matches[i].rm_so;
            if (delta >= sizeof(buffer))
                delta = sizeof(buffer)-1;
            memcpy(buffer, text+reg_matches[i].rm_so, delta);
            buffer[delta] = '\0';
            /* value is always interpreted using decimal base */
            number = strtol(buffer, NULL, 10);
            /*
            LIXA_TRACE(("lixa_my_retrieve_subtype: regmatch[%ld]='%s', "
                        "number=%d\n", i, buffer, number));
            */
            switch (i) {
                case 0: // full string
                    break;
                case 1: // version
                    *version = number;
                    break;
                case 2: // release
                    *release = number;
                    break;
                case 3: // minor
                    *minor = number;
                    break;
                case 4: // type
                    if (NULL == strstr(buffer, "MariaDB")) {
                        *rm_subtype = LIXA_SW_STATUS_RM_SUBTYPE_MYSQL;
                        LIXA_TRACE(("lixa_my_retrieve_subtype: this is an "
                                    "original MySQL database, version=%ld, "
                                    "release=%ld, minor=%ld\n", *version,
                                    *release, *minor));
                    } else {
                        *rm_subtype = LIXA_SW_STATUS_RM_SUBTYPE_MARIADB;
                        LIXA_TRACE(("lixa_my_retrieve_subtype: this is a "
                                    "MariaDB database, version=%ld, "
                                    "release=%ld, minor=%ld\n", *version,
                                    *release, *minor));
                    }
                default: // nothing to do
                    break;
            } // switch (i)
        }
        
        /* check the version to guarantee appropriate XA support */
        if ((*version < 5) ||
            (*version == 5 && *release < 7)) {
            LIXA_TRACE(("lixa_my_retrieve_subtype: this is an unsupported "
                        "version of MySQL/MariaDB database %ld.%ld.%ld\n",
                        *version, *release, *minor));
            LIXA_SYSLOG((LOG_ERR, LIXA_SYSLOG_LXC041E, *version, *release,
                         *minor));
            THROW(UNSUPPORTED_VERSION);
        }
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case MYSQL_SELECT_VERSION_ERROR:
            case REGCOMP_ERROR:
            case REGEX_ERROR:
            case UNSUPPORTED_VERSION:
                ret_cod = FALSE;
                break;
            case NONE:
                ret_cod = TRUE;
                break;
            default:
                ret_cod = FALSE;
        } /* switch (excp) */
    } /* TRY-CATCH */
    /* free memory */
    if (NULL != result)
        mysql_free_result(result);
    if (REGCOMP_ERROR < excp)
        regfree(&reg_expression);
    LIXA_TRACE(("lixa_my_retrieve_subtype/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



int lixa_my_open(char *xa_info, int rmid, long flags)
{
    enum Exception { INVALID_FLAGS
                     , ASYNC_NOT_SUPPORTED
                     , G_HASH_TABLE_NEW_FULL_ERROR
                     , MALLOC_ERROR
                     , RM_TYPE_ERROR
                     , PARSE_ERROR
                     , MYSQL_INIT_ERROR
                     , MYSQL_REAL_CONNECT_ERROR
                     , MYSQL_RETRIEVE_SUBTYPE_ERROR
                     , NONE } excp;
    int xa_rc = XAER_RMERR;

    MYSQL            *conn = NULL;
    lixa_sw_status_t *lps = NULL;
    
    LIXA_TRACE(("lixa_my_open: xa_info='%s', rmid=%d, flags=%ld\n",
                xa_info, rmid, flags));
    TRY {
        pthread_t key = pthread_self();
        guint i;
        const long valid_flags = TMASYNC|TMNOFLAGS;
        struct lixa_mysql_real_connect_s lmrc;
        int insert_in_lixa_sw_status = FALSE;
        
        /* lock the status mutex */
        g_mutex_lock(&lixa_sw_status_mutex);

        if ((flags|valid_flags) != valid_flags) {
            LIXA_TRACE(("lixa_my_open: invalid flag in 0x%x\n", flags));
            THROW(INVALID_FLAGS);
        }
        
        /* asynchronous operations are not supported */
        if (TMASYNC & flags) {
            LIXA_TRACE(("lixa_my_open: TMASYNC flag is not supported\n"));
            THROW(ASYNC_NOT_SUPPORTED);
        }

        if (NULL == lixa_sw_status) {
            /* the status structure must be initialized from scratch */
            if (NULL == (lixa_sw_status = g_hash_table_new_full(
                             g_direct_hash, g_direct_equal, NULL,
                             lixa_sw_status_destroy))) {
                LIXA_TRACE(("lixa_my_open/g_hash_table_new_full: unable to "
                            "allocate hash table status\n"));
                THROW(G_HASH_TABLE_NEW_FULL_ERROR);
            }
        }

        /* look for this thread status */
        if (NULL == (lps = (lixa_sw_status_t *)g_hash_table_lookup(
                         lixa_sw_status, (gconstpointer)key))) {
            LIXA_TRACE(("lixa_my_open: status for thread " PTHREAD_T_FORMAT
                        " not found, allocating it...\n", key));
            if (NULL == (lps = (lixa_sw_status_t *)malloc(
                             sizeof(lixa_sw_status_t)))) {
                LIXA_TRACE(("lixa_my_open: unable to allocate %u bytes\n",
                            sizeof(lixa_sw_status_t)));
                THROW(MALLOC_ERROR);
            }
            lixa_sw_status_init(lps);
            insert_in_lixa_sw_status = TRUE;
        }

        /* check if rmid is already connected */
        for (i=0; i<lps->rm->len; ++i) {
            struct lixa_sw_status_rm_s *lpsr = &g_array_index(
                lps->rm, struct lixa_sw_status_rm_s, i);
            if (rmid == lpsr->rmid) {
                if (LIXA_SW_STATUS_RM_TYPE_MYSQL != lpsr->rm_type) {
                    LIXA_TRACE(("lixa_my_open: internal error {lpsr->rmid=%d, "
                                "lpsr->rm_type=%d} (expected value %d)\n",
                                lpsr->rmid, lpsr->rm_type,
                                LIXA_SW_STATUS_RM_TYPE_MYSQL));
                    THROW(RM_TYPE_ERROR);
                }
                conn = (MYSQL *)lpsr->conn;
                break;
            }
        }

        if (NULL == conn) {
            /* create a new connection */
            struct lixa_sw_status_rm_s lpsr;
            int rm_subtype = LIXA_SW_STATUS_RM_SUBTYPE_NULL;
            long version=0, release=0, minor=0;
            lixa_sw_status_rm_init(&lpsr);

            if (XA_OK != (xa_rc = lixa_my_parse_xa_info(xa_info, &lmrc)))
                THROW(PARSE_ERROR);
        
            if (NULL == (conn = mysql_init(NULL))) {
                LIXA_TRACE(("lixa_my_open: mysql_init(NULL) returned NULL\n"));
                THROW(MYSQL_INIT_ERROR);
            }

            if (NULL == mysql_real_connect(
                    conn, lmrc.host, lmrc.user, lmrc.passwd, lmrc.db,
                    lmrc.port, lmrc.unix_socket, lmrc.client_flag)) {
                LIXA_TRACE(("lixa_my_open: mysql_real_connect returned error: "
                            "%u: %s\n", mysql_errno(conn), mysql_error(conn)));
                mysql_close(conn);
                THROW(MYSQL_REAL_CONNECT_ERROR);
            }

            if (!lixa_my_retrieve_subtype(conn, &rm_subtype, &version,
                                          &release, &minor)) {
                LIXA_TRACE(("lixa_my_open: lixa_my_retrieve_sybtype returned "
                            "FALSE\n"));
                THROW(MYSQL_RETRIEVE_SUBTYPE_ERROR);
            }
            
            /* save the connection for this thread/rmid */
            lpsr.rmid = rmid;
            lpsr.rm_type = LIXA_SW_STATUS_RM_TYPE_MYSQL;
            lpsr.rm_subtype = rm_subtype;
            lpsr.state.R = 1;
            lpsr.conn = (gpointer)conn;
            g_array_append_val(lps->rm, lpsr);
            if (insert_in_lixa_sw_status)
                g_hash_table_insert(lixa_sw_status, (gpointer)key,
                                    (gpointer)lps);
        }
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case INVALID_FLAGS:
                xa_rc = XAER_INVAL;
                break;
            case ASYNC_NOT_SUPPORTED:
                xa_rc = XAER_ASYNC;
                break;
            case G_HASH_TABLE_NEW_FULL_ERROR:
            case MALLOC_ERROR:
            case RM_TYPE_ERROR:
                xa_rc = XAER_RMERR;
                break;
            case PARSE_ERROR:
                xa_rc = XAER_INVAL;
                break;
            case MYSQL_INIT_ERROR:
            case MYSQL_REAL_CONNECT_ERROR:
            case MYSQL_RETRIEVE_SUBTYPE_ERROR:
                xa_rc = XAER_RMERR;
                break;
            case NONE:
                xa_rc = XA_OK;
                break;
            default:
                xa_rc = XAER_RMERR;
        } /* switch (excp) */
        if (NONE != excp && NULL != lps)
            free(lps);
        /* unlock the status mutex */
        g_mutex_unlock(&lixa_sw_status_mutex);
    } /* TRY-CATCH */
    LIXA_TRACE(("lixa_my_open/excp=%d/xa_rc=%d\n", excp, xa_rc));
    return xa_rc;
}



int lixa_my_close(char *xa_info, int rmid, long flags)
{
    enum Exception { INVALID_FLAGS
                     , ASYNC_NOT_SUPPORTED
                     , NOTHING_TO_DO1
                     , NOTHING_TO_DO2
                     , PROTOCOL_ERROR
                     , NOTHING_TO_DO3
                     , NONE } excp;
    int xa_rc = XAER_RMFAIL;
    
    LIXA_TRACE(("lixa_my_close\n"));
    TRY {
        guint             i;
        pthread_t         key = pthread_self();
        lixa_sw_status_t *lps = NULL;
        const long valid_flags = TMASYNC|TMNOFLAGS;
        
        /* lock the status mutex */
        g_mutex_lock(&lixa_sw_status_mutex);

        if ((flags|valid_flags) != valid_flags) {
            LIXA_TRACE(("lixa_my_close: invalid flag in 0x%x\n", flags));
            THROW(INVALID_FLAGS);
        }

        /* asynchronous operations are not supported */
        if (TMASYNC & flags) {
            LIXA_TRACE(("lixa_my_close: TMASYNC flag is not supported\n"));
            THROW(ASYNC_NOT_SUPPORTED);
        }

        if (NULL == lixa_sw_status) {
            /* the status structure does not exist, this is a dummy xa_close */
            THROW(NOTHING_TO_DO1);
        }
            
        if (NULL == (lps = (lixa_sw_status_t *)g_hash_table_lookup(
                         lixa_sw_status, (gconstpointer)key))) {
            /* the status structure does not contain a record for this
               thread, this is a dummy xa_close */
            THROW(NOTHING_TO_DO2);
        }

        /* check the state of the resource manager */
        for (i=0; i<lps->rm->len; ++i) {
            struct lixa_sw_status_rm_s *lpsr = &g_array_index(
                lps->rm, struct lixa_sw_status_rm_s, i);
            if (lpsr->rmid == rmid) {
                /* state found, checking protocol errors */
                if (lpsr->state.T == 1 ||
                    lpsr->state.S == 1) {
                    LIXA_TRACE(("lixa_my_close: state is T%d, S%d and "
                                "xa_close() cannot be called\n",
                                lpsr->state.T, lpsr->state.S));
                    THROW(PROTOCOL_ERROR);
                }
                /* state found, checking "Un-initialized" state */
                if (lpsr->state.R == 0)
                    THROW(NOTHING_TO_DO3);
                /* closing database connection if any */
                if (NULL != lpsr->conn) {
                    mysql_close((MYSQL *)lpsr->conn);
                    lpsr->conn = NULL;
                }
                lpsr->state.R = 0;
            }
        }        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case INVALID_FLAGS:
                xa_rc = XAER_INVAL;
                break;
            case ASYNC_NOT_SUPPORTED:
                xa_rc = XAER_ASYNC;
                break;
            case NOTHING_TO_DO1:
            case NOTHING_TO_DO2:
                xa_rc = XA_OK;
                break;
            case PROTOCOL_ERROR:
                xa_rc = XAER_PROTO;
                break;
            case NOTHING_TO_DO3:
            case NONE:
                xa_rc = XA_OK;
                break;
            default:
                xa_rc = XAER_RMFAIL;
        } /* switch (excp) */
        /* unlock the status mutex */
        g_mutex_unlock(&lixa_sw_status_mutex);
    } /* TRY-CATCH */
    LIXA_TRACE(("lixa_my_close/excp=%d/xa_rc=%d\n", excp, xa_rc));
    return xa_rc;
}



int lixa_my_start(const XID *xid, int rmid, long flags)
{
    enum Exception { PROTOCOL_ERROR
                     , NONE} excp;
    int xa_rc = XAER_RMFAIL;
    
    LIXA_TRACE(("lixa_my_start\n"));
    TRY {
        struct lixa_sw_status_rm_s *lpsr = NULL;
        
        /* lock the status mutex */
        g_mutex_lock(&lixa_sw_status_mutex);
        /* retrieve the status for this resource manager */
        if (NULL == (lpsr = lixa_sw_status_rm_get(rmid)))
            THROW(PROTOCOL_ERROR);

        xa_rc = lixa_my_start_core(lpsr, xid, rmid, flags);
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case PROTOCOL_ERROR:
                xa_rc = XAER_PROTO;
                break;
            case NONE:
                break;
            default:
                xa_rc = XAER_RMFAIL;
        } /* switch (excp) */
        /* unlock the status mutex */
        g_mutex_unlock(&lixa_sw_status_mutex);
    } /* TRY-CATCH */
    LIXA_TRACE(("lixa_my_start/excp=%d/xa_rc=%d\n", excp, xa_rc));
    return xa_rc;
}



int lixa_my_start_core(struct lixa_sw_status_rm_s *lpsr,
                       const XID *xid, int rmid, long flags)
    
{
    enum Exception { INVALID_FLAGS1
                     , INVALID_FLAGS2
                     , INVALID_FLAGS3
                     , PROTOCOL_ERROR
                     , NULL_CONN
                     , XID_SERIALIZE_ERROR
                     , MALLOC_ERROR
                     , MYSQL_ERROR
                     , NONE} excp;
    int xa_rc = XAER_RMFAIL;
    char *stmt = NULL;
    
    LIXA_TRACE(("lixa_my_start_core\n"));
    TRY {
        const long valid_flags = TMJOIN|TMRESUME|TMNOWAIT|TMASYNC|TMNOFLAGS;
        /* 2017-03-09: TMJOIN and TMRESUME are still not supported by MySQL */
        const long supp_flags = /*TMJOIN|TMRESUME|*/TMNOFLAGS;
        const char stmt_fmt[] = "XA START %s";
        const char stmt_join[] = " JOIN";
        const char stmt_resume[] = " RESUME";
        size_t stmt_size;
        lixa_my_ser_xid_t lmsx;
        
        if ((flags|valid_flags) != valid_flags) {
            LIXA_TRACE(("lixa_my_start_core: invalid flag in 0x%x\n", flags));
            THROW(INVALID_FLAGS1);
        }
        
        if ((flags|supp_flags) != supp_flags) {
            LIXA_TRACE(("lixa_my_start_core: flags 0x%x are not supported\n",
                        flags));
            THROW(INVALID_FLAGS2);
        }

        if (flags&TMJOIN && flags&TMRESUME) {
            LIXA_TRACE(("lixa_my_start_core: flags 0x%x are invalid (TMJOIN "
                        "and TMRESUME can not be used together\n", flags));
            THROW(INVALID_FLAGS3);            
        }

        /* this step does not check for TMJOIN/TMRESUME: it might be bugged */
        if (lpsr->state.R != 1 || lpsr->state.T != 0 ||
            (lpsr->state.S != 0 && lpsr->state.S != 2)) {
            LIXA_TRACE(("lixa_my_start_core: rmid %d "
                        "state(R,S,T)={%d,%d,%d}\n",
                        rmid, lpsr->state.R, lpsr->state.S, lpsr->state.T));
            THROW(PROTOCOL_ERROR);
        }

        /* this is an internal error :( */
        if (NULL == lpsr->conn) {
            LIXA_TRACE(("lixa_my_start_core: conn is NULL for rmid %d\n",
                        rmid));
            THROW(NULL_CONN);
        }

        /* serialize XID */
        if (!lixa_my_xid_serialize(xid, lmsx)) {
            LIXA_TRACE(("lixa_my_start_core: unable to serialize XID\n"));
            THROW(XID_SERIALIZE_ERROR);
        }
        LIXA_TRACE(("lixa_my_start_core: starting XID %s\n", lmsx));
        
        /* saving xid */
        lpsr->xid = *xid;
        /* preparing statement */
        stmt_size = strlen(lmsx) + sizeof(stmt_fmt) + sizeof(stmt_join) +
            sizeof(stmt_resume);
        if (NULL == (stmt = malloc(stmt_size)))
            THROW(MALLOC_ERROR);
        snprintf(stmt, stmt_size, stmt_fmt, lmsx);
        if (TMJOIN & flags)
            strncat(stmt, stmt_join, stmt_size);
        if (TMRESUME & flags)
            strncat(stmt, stmt_resume, stmt_size);
        /* starting transaction */
        LIXA_TRACE(("lixa_my_start_core: executing statement >%s<\n", stmt));
        if (mysql_query(lpsr->conn, stmt)) {
            LIXA_TRACE(("lixa_my_start_core: MySQL error while executing "
                        "'%s': %u/%s\n", stmt,
                        mysql_errno(lpsr->conn), mysql_error(lpsr->conn)));
            THROW(MYSQL_ERROR);
        }
        lpsr->state.T = 1;
        lpsr->state.S = 1;
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case INVALID_FLAGS1:
            case INVALID_FLAGS2:
            case INVALID_FLAGS3:
                xa_rc = XAER_INVAL;
                break;
            case PROTOCOL_ERROR:
                xa_rc = XAER_PROTO;
                break;
            case NULL_CONN:
                xa_rc = XAER_RMFAIL;
                break;
            case XID_SERIALIZE_ERROR:
            case MALLOC_ERROR:
                xa_rc = XAER_INVAL;
                break;                
            case MYSQL_ERROR:
                xa_rc = XAER_RMERR;
                break;
            case NONE:
                xa_rc = XA_OK;
                break;
            default:
                xa_rc = XAER_RMFAIL;
        } /* switch (excp) */
        if (NULL != stmt)
            free(stmt);
    } /* TRY-CATCH */
    LIXA_TRACE(("lixa_my_start_core/excp=%d/xa_rc=%d\n", excp, xa_rc));
    return xa_rc;
}



int lixa_my_end(const XID *xid, int rmid, long flags)
{
    enum Exception { PROTOCOL_ERROR
                     , NONE } excp;
    int xa_rc = XAER_RMFAIL;
    
    LIXA_TRACE(("lixa_my_end\n"));
    TRY {
        struct lixa_sw_status_rm_s *lpsr = NULL;

        /* lock the status mutex */
        g_mutex_lock(&lixa_sw_status_mutex);
        /* retrieve the status for this resource manager */
        if (NULL == (lpsr = lixa_sw_status_rm_get(rmid)))
            THROW(PROTOCOL_ERROR);

        xa_rc = lixa_my_end_core(lpsr, xid, rmid, flags);
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case PROTOCOL_ERROR:
                xa_rc = XAER_PROTO;
                break;
            case NONE:
                break;
            default:
                xa_rc = XAER_RMFAIL;
        } /* switch (excp) */
        /* unlock the status mutex */
        g_mutex_unlock(&lixa_sw_status_mutex);
    } /* TRY-CATCH */
    LIXA_TRACE(("lixa_my_end/excp=%d/xa_rc=%d\n", excp, xa_rc));
    return xa_rc;
}



int lixa_my_end_core(struct lixa_sw_status_rm_s *lpsr,
                     const XID *xid, int rmid, long flags)
{
    enum Exception { INVALID_FLAGS1
                     , INVALID_FLAGS2
                     , PROTOCOL_ERROR2
                     , NULL_CONN
                     , XID_SERIALIZE_ERROR1
                     , XID_MISMATCH
                     , XID_SERIALIZE_ERROR2
                     , MALLOC_ERROR
                     , MYSQL_ERROR
                     , ROLLBACK_ONLY
                     , NONE } excp;
    int xa_rc = XAER_RMFAIL;
    char *stmt = NULL;
    
    LIXA_TRACE(("lixa_my_end_core\n"));
    TRY {
        const long valid_flags = TMSUSPEND|TMMIGRATE|TMSUCCESS|TMFAIL|TMASYNC;
        /* 2017-03-09: TMSUSPEND and TMMIGRATE are still not supported by
           MySQL */
        const long supp_flags = /*TMSUSPEND|TMMIGRATE|*/TMSUCCESS;
        const char stmt_fmt[] = "XA END %s";
        const char stmt_suspend[] = " SUSPEND";
        const char stmt_migrate[] = " FOR MIGRATE";
        size_t stmt_size;
        lixa_my_ser_xid_t lmsx;

        if ((flags|valid_flags) != valid_flags) {
            LIXA_TRACE(("lixa_my_end_core: invalid flag in 0x%x\n", flags));
            THROW(INVALID_FLAGS1);
        }

        if (flags&TMFAIL) {
            LIXA_TRACE(("lixa_my_end_core: MySQL/MariaDB does not support "
                        "TMFAIL, ignoring it...\n"));
            flags ^= TMFAIL;
        }
        
        if ((flags|supp_flags) != supp_flags) {
            LIXA_TRACE(("lixa_my_end_core: flags 0x%x are not supported\n",
                        flags));
            THROW(INVALID_FLAGS2);
        }
        
        if (lpsr->state.R != 1 || lpsr->state.T != 1 ||
            (lpsr->state.S != 1 && lpsr->state.S != 2)) {
            LIXA_TRACE(("lixa_my_end_core: rmid %d state(R,S,T)={%d,%d,%d}\n",
                        rmid, lpsr->state.R, lpsr->state.S, lpsr->state.T));
            THROW(PROTOCOL_ERROR2);
        }

        /* this is an internal error :( */
        if (NULL == lpsr->conn) {
            LIXA_TRACE(("lixa_my_end_core: conn is NULL for rmid %d\n", rmid));
            THROW(NULL_CONN);
        }

        /* serialize XID */

        /* checking xid is the started one */
        if (0 != lixa_xid_compare(xid, &(lpsr->xid))) {
            lixa_my_ser_xid_t lmsx1, lmsx2;
            if (!lixa_my_xid_serialize(xid, lmsx1) ||
                !lixa_my_xid_serialize(&(lpsr->xid), lmsx2)) {
                LIXA_TRACE(("lixa_my_end_core: unable to serialize XID\n"));
                THROW(XID_SERIALIZE_ERROR1);
            }
            LIXA_TRACE(("lixa_my_end_core: ending XID %s is not the same "
                        "of started XID %s\n", lmsx1, lmsx2));
            THROW(XID_MISMATCH);
        }

        /* serialize XID */
        if (!lixa_my_xid_serialize(xid,lmsx)) {
            LIXA_TRACE(("lixa_my_end_core: unable to serialize XID\n"));
            THROW(XID_SERIALIZE_ERROR2);
        }
        LIXA_TRACE(("lixa_my_end_core: ending XID %s\n", lmsx));
        
        /* saving xid */
        lpsr->xid = *xid;
        /* preparing statement */
        stmt_size = strlen(lmsx) + sizeof(stmt_fmt) + sizeof(stmt_suspend) +
            sizeof(stmt_migrate);
        if (NULL == (stmt = malloc(stmt_size)))
            THROW(MALLOC_ERROR);
        snprintf(stmt, stmt_size, stmt_fmt, lmsx);
        if (TMSUSPEND&flags)
            strncat(stmt, stmt_suspend, stmt_size);
        if (TMMIGRATE&flags)
            strncat(stmt, stmt_migrate, stmt_size);
        /* starting transaction */
        LIXA_TRACE(("lixa_my_end_core: executing statement >%s<\n", stmt));
        if (mysql_query(lpsr->conn, stmt)) {
            LIXA_TRACE(("lixa_my_end_core: MySQL error while executing "
                        "'%s': %u/%s\n", stmt,
                        mysql_errno(lpsr->conn), mysql_error(lpsr->conn)));
            THROW(MYSQL_ERROR);
        }
        
        /* check if the TM marked as failed the transaction */
        if (TMFAIL & flags) {
            LIXA_TRACE(("lixa_my_end_core: TMFAIL detected, entering "
                        "'Rollback Only' state\n"));
            lpsr->state.S = 4;
            lpsr->state.T = 0;
            THROW(ROLLBACK_ONLY);
        } else {
            lpsr->state.S = 2;
            lpsr->state.T = 0;
        }        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case INVALID_FLAGS1:
            case INVALID_FLAGS2:
                xa_rc = XAER_INVAL;
                break;
            case PROTOCOL_ERROR2:
                xa_rc = XAER_PROTO;
                break;
            case NULL_CONN:
                xa_rc = XAER_RMFAIL;
                break;
            case XID_SERIALIZE_ERROR1:
                xa_rc = XAER_INVAL;
                break;
            case XID_MISMATCH:
                xa_rc = XAER_NOTA;
                break;
            case XID_SERIALIZE_ERROR2:
                xa_rc = XAER_INVAL;
                break;
            case MALLOC_ERROR:
                xa_rc = XAER_INVAL;
                break;                
            case MYSQL_ERROR:
                xa_rc = XAER_RMERR;
                break;
            case ROLLBACK_ONLY:
                xa_rc = XA_RBROLLBACK;
                break;
            case NONE:
                xa_rc = XA_OK;
                break;
            default:
                xa_rc = XAER_RMFAIL;
        } /* switch (excp) */
        if (NULL != stmt)
            free(stmt);
    } /* TRY-CATCH */
    LIXA_TRACE(("lixa_my_end_core/excp=%d/xa_rc=%d\n", excp, xa_rc));
    return xa_rc;
}



int lixa_my_rollback(const XID *xid, int rmid, long flags)
{
    enum Exception { PROTOCOL_ERROR
                     , NONE } excp;
    int xa_rc = XAER_RMFAIL;
    
    LIXA_TRACE(("lixa_my_rollback\n"));
    TRY {
        struct lixa_sw_status_rm_s *lpsr = NULL;

        /* lock the status mutex */
        g_mutex_lock(&lixa_sw_status_mutex);
        /* retrieve the status for this resource manager */
        if (NULL == (lpsr = lixa_sw_status_rm_get(rmid)))
            THROW(PROTOCOL_ERROR);

        xa_rc = lixa_my_rollback_core(lpsr, xid, rmid, flags);
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case PROTOCOL_ERROR:
                xa_rc = XAER_PROTO;
                break;
            case NONE:
                break;
            default:
                xa_rc = XAER_RMFAIL;
        } /* switch (excp) */
        /* unlock the status mutex */
        g_mutex_unlock(&lixa_sw_status_mutex);
    } /* TRY-CATCH */
    LIXA_TRACE(("lixa_my_rollback/excp=%d/xa_rc=%d\n", excp, xa_rc));
    return xa_rc;
}



int lixa_my_rollback_core(struct lixa_sw_status_rm_s *lpsr,
                          const XID *xid, int rmid, long flags)
{
    enum Exception {
        INVALID_FLAGS1,
        INVALID_FLAGS2,
        PROTOCOL_ERROR2,
        NULL_CONN,
        XID_SERIALIZE_ERROR,
        AUTOCOMMIT_ERROR,
        XA_RECOVER_ERROR,
        XID_NOT_AVAILABLE,
        PROTOCOL_ERROR3,
        XID_MISMATCH,
        ROLLBACK_ERROR,
        NONE
    } excp;
    int xa_rc = XAER_RMFAIL;
    
    LIXA_TRACE(("lixa_my_rollback_core\n"));
    TRY {
        const long valid_flags = TMFAIL|TMASYNC;
        lixa_my_ser_xid_t lmsx;

        if ((flags|valid_flags) != valid_flags) {
            LIXA_TRACE(("lixa_my_rollback_core: invalid flag in 0x%x\n",
                        flags));
            THROW(INVALID_FLAGS1);
        }
        
        if (TMASYNC & flags) {
            LIXA_TRACE(("lixa_my_rollback_core: flags 0x%x are not "
                        "supported\n", flags));
            THROW(INVALID_FLAGS2);
        }
        
        if (lpsr->state.R != 1 || lpsr->state.T != 0) {
            LIXA_TRACE(("lixa_my_rollback_core: rmid %d "
                        "state(R,S,T)={%d,%d,%d}\n", rmid, lpsr->state.R,
                        lpsr->state.S, lpsr->state.T));
            THROW(PROTOCOL_ERROR2);
        }
        
        /* this is an internal error :( */
        if (NULL == lpsr->conn) {
            LIXA_TRACE(("lixa_my_rollback_core: conn is NULL for rmid %d\n",
                        rmid));
            THROW(NULL_CONN);
        }

        /* serialize XID */
        if (!lixa_my_xid_serialize(xid, lmsx)) {
            LIXA_TRACE(("lixa_my_rollback_core: unable to serialize XID\n"));
            THROW(XID_SERIALIZE_ERROR);
        }

        if (lpsr->state.S != 2 && lpsr->state.S != 3 && lpsr->state.S != 4) {
            MYSQL_RES *res;
            MYSQL_ROW row;
            XID xid_r;
            int num_fields;
            const char *autocommit_stmt = "SET autocommit=1";
            const char *xa_recover_stmt = "";

            if (LIXA_SW_STATUS_RM_SUBTYPE_MARIADB == lpsr->rm_subtype) {
                // MariaDB
                xa_recover_stmt = MARIADB_XA_RECOVER_STMT;
            } else {
                // MySQL
                xa_recover_stmt = MYSQL_XA_RECOVER_STMT;
            }
            
            /* due to bug #87836 in MySQL 5.7.x
               https://bugs.mysql.com/bug.php?id=87836
               we need to force AUTOCOMMIT */            
            LIXA_TRACE(("lixa_my_rollback_core: executing statement >%s<\n",
                        autocommit_stmt));
            if (mysql_query(lpsr->conn, autocommit_stmt)) {
                LIXA_TRACE(("lixa_my_rollback_core: error while executing "
                            ">%s< statement: %u/%s\n", autocommit_stmt,
                            mysql_errno(lpsr->conn), mysql_error(lpsr->conn)));
                THROW(AUTOCOMMIT_ERROR);
            }
            LIXA_TRACE(("lixa_my_rollback_core: executing statement >%s<\n",
                        xa_recover_stmt));
            if (mysql_query(lpsr->conn, xa_recover_stmt)) {
                LIXA_TRACE(("lixa_my_rollback_core: error while executing "
                            ">%s< statement: %u/%s\n", xa_recover_stmt,
                            mysql_errno(lpsr->conn), mysql_error(lpsr->conn)));
                THROW(XA_RECOVER_ERROR);
            }
            res = mysql_store_result(lpsr->conn);
            num_fields = mysql_num_fields(res);
            while ((row = mysql_fetch_row(res))) {
                const char *formatID = row[0] ? row[0] : "";
                const char *gtrid_length = row[1] ? row[1] : "";
                const char *bqual_length = row[2] ? row[2] : "";
                const char *data = row[3] ? row[3] : "";
                LIXA_TRACE(("lixa_my_rollback_core: formatID=%s, "
                            "gtrid_length=%s, bqual_length=%s, data=%s\n",
                            formatID, gtrid_length, bqual_length, data));
                if (!lixa_my_xid_deserialize(&xid_r, formatID, gtrid_length,
                                            bqual_length, data, 
					    lpsr->rm_subtype)) {
                    LIXA_TRACE(("lixa_my_rollback_core: unable to deserialize "
                                "the XID retrieved with XA RECOVER\n"));
                    LIXA_SYSLOG((LOG_WARNING, LIXA_SYSLOG_LXC042W, data));
                }
                if (!lixa_xid_compare(xid, &xid_r)) {
                    LIXA_TRACE(("lixa_my_rollback_core: the transaction %s is "
                                "in prepared state\n", lmsx));
                    lpsr->state.S = 3;
                    lpsr->xid = xid_r;
                    break;
                }
            }
            mysql_free_result(res);
            if (lpsr->state.S != 3) {
                LIXA_TRACE(("lixa_my_rollback_core: the transaction %s is not "
                            "available\n", lmsx));
                THROW(XID_NOT_AVAILABLE);
            }
        }

        if (lpsr->state.S != 2 && lpsr->state.S != 3 && lpsr->state.S != 4) {
            LIXA_TRACE(("lixa_my_rollback_core: rmid %d "
                        "state(R,S,T)={%d,%d,%d}\n",
                        rmid, lpsr->state.R, lpsr->state.S, lpsr->state.T));
            THROW(PROTOCOL_ERROR3);
        }
        
        /* checking xid is the started one */
        if (0 != lixa_xid_compare(xid, &(lpsr->xid))) {
            lixa_my_ser_xid_t lmsx2;
            lixa_my_xid_serialize(&(lpsr->xid), lmsx2);
            LIXA_TRACE(("lixa_my_rollback_core: rolling back XID '%s' is not "
                        "the same of started XID '%s'\n", lmsx, lmsx2));
            THROW(XID_MISMATCH);
        }
        LIXA_TRACE(("lixa_my_rollback_core: rolling back XID %s\n", lmsx));

        if (lpsr->state.S == 3 || lpsr->state.S == 2) {
            const char ROLLBACK_PREP_FMT[] = "XA ROLLBACK %s";
            char my_cmd_buf[sizeof(ROLLBACK_PREP_FMT) +
                            sizeof(lixa_my_ser_xid_t)];
            lpsr->state.S = 0;
            /* rolling back the transaction */
            snprintf(my_cmd_buf, sizeof(my_cmd_buf), ROLLBACK_PREP_FMT, lmsx);
            if (mysql_query(lpsr->conn, my_cmd_buf)) {
                LIXA_TRACE(("lixa_my_rollback_core: error while executing "
                            ">%s> statement: %u/%s\n", my_cmd_buf,
                            mysql_errno(lpsr->conn), mysql_error(lpsr->conn)));
                THROW(ROLLBACK_ERROR);
            }
        }
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case INVALID_FLAGS1:
            case INVALID_FLAGS2:
                xa_rc = XAER_INVAL;
                break;
            case PROTOCOL_ERROR2:
            case PROTOCOL_ERROR3:
                xa_rc = XAER_PROTO;
                break;
            case XID_NOT_AVAILABLE:
                xa_rc = XAER_NOTA;
                break;
            case AUTOCOMMIT_ERROR:
            case XA_RECOVER_ERROR:
            case NULL_CONN:
                xa_rc = XAER_RMFAIL;
                break;
            case XID_SERIALIZE_ERROR:
                xa_rc = XAER_INVAL;
                break;
            case XID_MISMATCH:
                xa_rc = XAER_NOTA;
                break;
            case ROLLBACK_ERROR:
                xa_rc = XAER_RMERR;
                break;
            case NONE:
                xa_rc = XA_OK;
                break;
            default:
                xa_rc = XAER_RMFAIL;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("lixa_my_rollback_core/excp=%d/xa_rc=%d\n", excp, xa_rc));
    return xa_rc;
}



int lixa_my_prepare(const XID *xid, int rmid, long flags)
{
    enum Exception { PROTOCOL_ERROR
                     , NONE } excp;
    int xa_rc = XAER_RMFAIL;
    
    LIXA_TRACE(("lixa_my_prepare\n"));
    TRY {
        struct lixa_sw_status_rm_s *lpsr = NULL;

        /* lock the status mutex */
        g_mutex_lock(&lixa_sw_status_mutex);
        /* retrieve the status for this resource manager */
        if (NULL == (lpsr = lixa_sw_status_rm_get(rmid)))
            THROW(PROTOCOL_ERROR);

        xa_rc = lixa_my_prepare_core(lpsr, xid, rmid, flags);
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case PROTOCOL_ERROR:
                xa_rc = XAER_PROTO;
                break;
            case NONE:
                break;
            default:
                xa_rc = XAER_RMFAIL;
        } /* switch (excp) */
        /* unlock the status mutex */
        g_mutex_unlock(&lixa_sw_status_mutex);
    } /* TRY-CATCH */
    LIXA_TRACE(("lixa_my_prepare/excp=%d/xa_rc=%d\n", excp, xa_rc));
    return xa_rc;
}



int lixa_my_prepare_core(struct lixa_sw_status_rm_s *lpsr,
                         const XID *xid, int rmid, long flags)
{
    enum Exception { INVALID_FLAGS1
                     , INVALID_FLAGS2
                     , PROTOCOL_ERROR2
                     , NULL_CONN
                     , XID_SERIALIZE_ERROR
                     , XID_MISMATCH
                     , PREPARE_ERROR
                     , NONE } excp;
    int xa_rc = XAER_RMFAIL;
    
    LIXA_TRACE(("lixa_my_prepare_core\n"));
    TRY {
        const long valid_flags = TMFAIL|TMASYNC;
        lixa_my_ser_xid_t lmsx;
        const char PREPARE_TRANS_FMT[] = "XA PREPARE %s";
        char my_cmd_buf[sizeof(PREPARE_TRANS_FMT) + sizeof(lixa_my_ser_xid_t)];
        
        if ((flags|valid_flags) != valid_flags) {
            LIXA_TRACE(("lixa_my_prepare_core: invalid flag in 0x%x\n",
                        flags));
            THROW(INVALID_FLAGS1);
        }
        
        if (TMASYNC & flags) {
            LIXA_TRACE(("lixa_my_prepare_core: flags 0x%x are not supported\n",
                        flags));
            THROW(INVALID_FLAGS2);
        }
        
        if (lpsr->state.R != 1 || lpsr->state.T != 0 || lpsr->state.S != 2) {
            LIXA_TRACE(("lixa_my_prepare_core: rmid %d "
                        "state(R,S,T)={%d,%d,%d}\n",
                        rmid, lpsr->state.R, lpsr->state.S, lpsr->state.T));
            THROW(PROTOCOL_ERROR2);
        }

        /* this is an internal error :( */
        if (NULL == lpsr->conn) {
            LIXA_TRACE(("lixa_my_prepare_core: conn is NULL for rmid %d\n",
                        rmid));
            THROW(NULL_CONN);
        }

        /* serialize XID */
        if (!lixa_my_xid_serialize(xid, lmsx)) {
            LIXA_TRACE(("lixa_my_prepare_core: unable to serialize XID\n"));
            THROW(XID_SERIALIZE_ERROR);
        }

        /* checking xid is the started one */
        if (0 != lixa_xid_compare(xid, &(lpsr->xid))) {
            lixa_my_ser_xid_t lmsx2;
            lixa_my_xid_serialize(&(lpsr->xid), lmsx2);
            LIXA_TRACE(("lixa_my_prepare_core: preparing XID %s is not the "
                        "same of started XID %s\n", lmsx, lmsx2));
            THROW(XID_MISMATCH);
        }
        LIXA_TRACE(("lixa_my_prepare_core: preparing XID %s\n", lmsx));

        /* preparing transaction */
        snprintf(my_cmd_buf, sizeof(my_cmd_buf), PREPARE_TRANS_FMT, lmsx);
        if (mysql_query(lpsr->conn, my_cmd_buf)) {
            LIXA_TRACE(("lixa_my_prepare_core: error while executing "
                        "'%s': %u/%s)\n", my_cmd_buf,
                        mysql_errno(lpsr->conn), mysql_error(lpsr->conn)));
            /* the resource manager could unilaterally rollback and return
             * XA_RBROLLBACK to the transaction manager, but it is leaved
             * as a future improvment if necessary */
            THROW(PREPARE_ERROR);
        }
        lpsr->state.S = 3;

        THROW(NONE);
    } CATCH {
        switch (excp) {
            case INVALID_FLAGS1:
            case INVALID_FLAGS2:
                xa_rc = XAER_INVAL;
                break;
            case PROTOCOL_ERROR2:
                xa_rc = XAER_PROTO;
                break;
            case NULL_CONN:
                xa_rc = XAER_RMFAIL;
                break;
            case XID_SERIALIZE_ERROR:
                xa_rc = XAER_INVAL;
                break;
            case XID_MISMATCH:
                xa_rc = XAER_NOTA;
                break;
            case PREPARE_ERROR:
                xa_rc = XAER_RMERR;
                break;
            case NONE:
                xa_rc = XA_OK;
                break;
            default:
                xa_rc = XAER_RMFAIL;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("lixa_my_prepare_core/excp=%d/xa_rc=%d\n", excp, xa_rc));
    return xa_rc;
}



int lixa_my_commit(const XID *xid, int rmid, long flags)
{
    enum Exception { PROTOCOL_ERROR
                     , NONE } excp;
    int xa_rc = XAER_RMFAIL;
    
    LIXA_TRACE(("lixa_my_commit\n"));
    TRY {
        struct lixa_sw_status_rm_s *lpsr = NULL;

        /* lock the status mutex */
        g_mutex_lock(&lixa_sw_status_mutex);
        /* retrieve the status for this resource manager */
        if (NULL == (lpsr = lixa_sw_status_rm_get(rmid)))
            THROW(PROTOCOL_ERROR);

        xa_rc = lixa_my_commit_core(lpsr, xid, rmid, flags);
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case PROTOCOL_ERROR:
                xa_rc = XAER_PROTO;
                break;
            case NONE:
                break;
            default:
                xa_rc = XAER_RMFAIL;
        } /* switch (excp) */
        /* unlock the status mutex */
        g_mutex_unlock(&lixa_sw_status_mutex);
    } /* TRY-CATCH */
    LIXA_TRACE(("lixa_my_commit/excp=%d/xa_rc=%d\n", excp, xa_rc));
    return xa_rc;
}



int lixa_my_commit_core(struct lixa_sw_status_rm_s *lpsr,
                        const XID *xid, int rmid, long flags)
{
    enum Exception {
        INVALID_FLAGS1,
        INVALID_FLAGS2,
        PROTOCOL_ERROR2,
        NULL_CONN,
        XID_SERIALIZE_ERROR,
        AUTOCOMMIT_ERROR,
        XA_RECOVER_ERROR,
        XID_NOT_AVAILABLE,
        XID_MISMATCH,
        PROTOCOL_ERROR3,
        COMMIT_ERROR,
        NONE
    } excp;
    int xa_rc = XAER_RMFAIL;
    
    LIXA_TRACE(("lixa_my_commit_core\n"));
    TRY {
        const long valid_flags = TMNOWAIT|TMASYNC|TMONEPHASE|TMNOFLAGS;
        lixa_my_ser_xid_t lmsx;
        int one_phase = flags & TMONEPHASE;

        if ((flags|valid_flags) != valid_flags) {
            LIXA_TRACE(("lixa_my_commit_core: invalid flag in 0x%x\n", flags));
            THROW(INVALID_FLAGS1);
        }
        
        if (TMNOWAIT & flags || TMASYNC & flags) {
            LIXA_TRACE(("lixa_my_commit_core: flags 0x%x are not supported\n",
                        flags));
            THROW(INVALID_FLAGS2);
        }        
        
        if (lpsr->state.R != 1 || lpsr->state.T != 0) {
            LIXA_TRACE(("lixa_my_commit_core: rmid %d "
                        "state(R,S,T)={%d,%d,%d}\n",
                        rmid, lpsr->state.R, lpsr->state.S, lpsr->state.T));
            THROW(PROTOCOL_ERROR2);
        }

        /* this is an internal error :( */
        if (NULL == lpsr->conn) {
            LIXA_TRACE(("lixa_my_commit_core: conn is NULL for rmid %d\n",
                        rmid));
            THROW(NULL_CONN);
        }

        /* serialize XID */
        if (!lixa_my_xid_serialize(xid, lmsx)) {
            LIXA_TRACE(("lixa_my_commit_core: unable to serialize XID\n"));
            THROW(XID_SERIALIZE_ERROR);
        }

        LIXA_TRACE(("lixa_my_commit_core: lpsr->state.S=%d, one_phase=%d\n",
                    lpsr->state.S, one_phase));
        if (lpsr->state.S != 3 && !one_phase) {
            MYSQL_RES *res;
            MYSQL_ROW row;
            XID xid_r;
            int num_fields;
            const char *autocommit_stmt = "SET autocommit=1";
            const char *xa_recover_stmt = "";

            if (LIXA_SW_STATUS_RM_SUBTYPE_MARIADB == lpsr->rm_subtype) {
                // MariaDB
                xa_recover_stmt = MARIADB_XA_RECOVER_STMT;
            } else {
                // MySQL
                xa_recover_stmt = MYSQL_XA_RECOVER_STMT;
            }
            
            /* due to bug #87836 in MySQL 5.7.x
               https://bugs.mysql.com/bug.php?id=87836
               we need to forse AUTOCOMMIT */            
            LIXA_TRACE(("lixa_my_commit_core: executing statement >%s<\n",
                        autocommit_stmt));
            if (mysql_query(lpsr->conn, autocommit_stmt)) {
                LIXA_TRACE(("lixa_my_commit_core: error while executing "
                            ">%s< statement: %u/%s\n", autocommit_stmt,
                            mysql_errno(lpsr->conn), mysql_error(lpsr->conn)));
                THROW(AUTOCOMMIT_ERROR);
            }
            LIXA_TRACE(("lixa_my_commit_core: executing statement >%s<\n",
                        xa_recover_stmt));
            if (mysql_query(lpsr->conn, xa_recover_stmt)) {
                LIXA_TRACE(("lixa_my_commit_core: error while executing "
                            ">%s< statement: %u/%s\n", xa_recover_stmt,
                            mysql_errno(lpsr->conn), mysql_error(lpsr->conn)));
                THROW(XA_RECOVER_ERROR);
            }
            res = mysql_store_result(lpsr->conn);
            num_fields = mysql_num_fields(res);
            while ((row = mysql_fetch_row(res))) {
                const char *formatID = row[0] ? row[0] : "";
                const char *gtrid_length = row[1] ? row[1] : "";
                const char *bqual_length = row[2] ? row[2] : "";
                const char *data = row[3] ? row[3] : "";
                LIXA_TRACE(("lixa_my_commit_core: formatID=%s, "
                            "gtrid_length=%s, bqual_length=%s, data=%s\n",
                            formatID, gtrid_length, bqual_length, data));
                if (!lixa_my_xid_deserialize(&xid_r, formatID, gtrid_length,
                                            bqual_length, data,
					    lpsr->rm_subtype)) {
                    LIXA_TRACE(("lixa_my_commit_core: unable to deserialize "
                                "the XID retrieved with XA RECOVER\n"));
                    LIXA_SYSLOG((LOG_WARNING, LIXA_SYSLOG_LXC042W, data));
                }
                if (!lixa_xid_compare(xid, &xid_r)) {
                    LIXA_TRACE(("lixa_my_commit_core: the transaction %s is "
                                "in prepared state\n", lmsx));
                    lpsr->state.S = 3;
                    lpsr->xid = xid_r;
                    break;
                }
            }
            mysql_free_result(res);
            if (lpsr->state.S != 3) {
                LIXA_TRACE(("lixa_my_commit_core: the transaction %s is not "
                            "available\n", lmsx));
                THROW(XID_NOT_AVAILABLE);
            }
        }

        /* checking xid is the started one */
        if (0 != lixa_xid_compare(xid, &(lpsr->xid))) {
            lixa_my_ser_xid_t lmsx2;
            lixa_my_xid_serialize(&(lpsr->xid), lmsx2);
            LIXA_TRACE(("lixa_my_commit_core: committing XID %s is not the "
                        "same of started XID %s\n", lmsx, lmsx2));
            THROW(XID_MISMATCH);
        }
        LIXA_TRACE(("lixa_my_commit_core: committing XID %s\n", lmsx));

        if ((lpsr->state.S != 2 && one_phase) ||
            (lpsr->state.S != 3 && !one_phase)) {
            LIXA_TRACE(("lixa_my_commit_core: rmid %d "
                        "state(R,S,T)={%d,%d,%d}, "
                        "one_phase_commit_core=%d\n",
                        rmid, lpsr->state.R, lpsr->state.S, lpsr->state.T,
                        one_phase));
            THROW(PROTOCOL_ERROR3);
        } else {
            const char COMMIT_1_FMT[] = "XA COMMIT %s ONE PHASE";
            const char COMMIT_2_FMT[] = "XA COMMIT %s";
            char my_cmd_buf[sizeof(COMMIT_1_FMT) +
                            sizeof(lixa_my_ser_xid_t)];
            lpsr->state.S = 0;
            /* committing transaction */
            snprintf(my_cmd_buf, sizeof(my_cmd_buf),
                     one_phase ? COMMIT_1_FMT : COMMIT_2_FMT, lmsx);
            LIXA_TRACE(("lixa_my_commit_core: executing statement >%s<\n",
                        my_cmd_buf));
            if (mysql_query(lpsr->conn, my_cmd_buf)) {
                LIXA_TRACE(("lixa_my_commit_core: error while executing "
                            ">%s< statement: %u/%s\n", my_cmd_buf,
                            mysql_errno(lpsr->conn), mysql_error(lpsr->conn)));
                THROW(COMMIT_ERROR);
            }
        }
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case INVALID_FLAGS1:
            case INVALID_FLAGS2:
                xa_rc = XAER_INVAL;
                break;
            case PROTOCOL_ERROR2:
            case PROTOCOL_ERROR3:
                xa_rc = XAER_PROTO;
                break;
            case XID_NOT_AVAILABLE:
                xa_rc = XAER_NOTA;
                break;
            case AUTOCOMMIT_ERROR:
            case XA_RECOVER_ERROR:
            case NULL_CONN:
                xa_rc = XAER_RMFAIL;
                break;
            case XID_SERIALIZE_ERROR:
                xa_rc = XAER_INVAL;
                break;
            case XID_MISMATCH:
                xa_rc = XAER_NOTA;
                break;
            case COMMIT_ERROR:
                xa_rc = XAER_RMERR;
                break;
            case NONE:
                xa_rc = XA_OK;
                break;
            default:
                xa_rc = XAER_RMFAIL;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("lixa_my_commit_core/excp=%d/xa_rc=%d\n", excp, xa_rc));
    return xa_rc;
}



int lixa_my_recover(XID *xids, long count, int rmid, long flags)
{
    enum Exception { PROTOCOL_ERROR
                     , NONE } excp;
    int xa_rc = XAER_RMFAIL;
    
    LIXA_TRACE(("lixa_my_recover\n"));
    TRY {
        struct lixa_sw_status_rm_s *lpsr = NULL;

        /* lock the status mutex */
        g_mutex_lock(&lixa_sw_status_mutex);
        /* retrieve the status for this resource manager */
        if (NULL == (lpsr = lixa_sw_status_rm_get(rmid)))
            THROW(PROTOCOL_ERROR);

        xa_rc = lixa_my_recover_core(lpsr, xids, count, rmid, flags);
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case PROTOCOL_ERROR:
                xa_rc = XAER_PROTO;
                break;
            case NONE:
                break;
            default:
                xa_rc = XAER_RMFAIL;
        } /* switch (excp) */
        /* unlock the status mutex */
        g_mutex_unlock(&lixa_sw_status_mutex);
    } /* TRY-CATCH */
    LIXA_TRACE(("lixa_my_recover/excp=%d/xa_rc=%d\n", excp, xa_rc));
    return xa_rc;
}



int lixa_my_recover_core(struct lixa_sw_status_rm_s *lpsr,
                         XID *xids, long count, int rmid, long flags)
{
    enum Exception { INVALID_FLAGS
                     , INVALID_OPTIONS1
                     , INVALID_OPTIONS2
                     , PROTOCOL_ERROR2
                     , NULL_CONN
                     , XA_RECOVER_ERROR
                     , NONE } excp;
    int xa_rc = XAER_RMFAIL;
    long out_count = 0;
    
    LIXA_TRACE(("lixa_my_recover_core\n"));
    TRY {
        const long valid_flags = TMSTARTRSCAN|TMENDRSCAN|TMNOFLAGS;

        const char *xa_recover_stmt = "";

        if (LIXA_SW_STATUS_RM_SUBTYPE_MARIADB == lpsr->rm_subtype) {
            // MariaDB
            xa_recover_stmt = MARIADB_XA_RECOVER_STMT;
        } else {
            // MySQL
            xa_recover_stmt = MYSQL_XA_RECOVER_STMT;
        }
            
        if ((flags|valid_flags) != valid_flags) {
            LIXA_TRACE(("lixa_my_recover_core: invalid flag in 0x%x\n",
                        flags));
            THROW(INVALID_FLAGS);
        }

        if ((NULL == xids && count > 0) || (count < 0)) {
            LIXA_TRACE(("lixa_my_recover_core: xids=%p, count=%ld\n",
                        xids, count));
            THROW(INVALID_OPTIONS1);
        }
        
        if (!(flags & TMSTARTRSCAN)) {
            LIXA_TRACE(("lixa_my_recover_core: TMSTARTRSCAN flag must be "
                        "set\n"));
            THROW(INVALID_OPTIONS2);
        }
        
        if (lpsr->state.R != 1) {
            LIXA_TRACE(("lixa_my_recover_core: rmid %d "
                        "state(R,S,T)={%d,%d,%d}\n",
                        rmid, lpsr->state.R, lpsr->state.S, lpsr->state.T));
            THROW(PROTOCOL_ERROR2);
        }

        /* this is an internal error :( */
        if (NULL == lpsr->conn) {
            LIXA_TRACE(("lixa_my_recover_core: conn is NULL for rmid %d\n",
                        rmid));
            THROW(NULL_CONN);
        }

        /* execute XA RECOVER */
        if (flags & TMSTARTRSCAN) {
            MYSQL_RES *res;
            MYSQL_ROW row;
            XID xid;
            int num_fields;
            LIXA_TRACE(("lixa_my_recover_core: executing statement >%s<\n",
                        xa_recover_stmt));
            if (mysql_query(lpsr->conn, xa_recover_stmt)) {
                LIXA_TRACE(("lixa_my_recover_core: error while executing "
                            "'%s' command: %u/%s\n", xa_recover_stmt,
                            mysql_errno(lpsr->conn), mysql_error(lpsr->conn)));
                THROW(XA_RECOVER_ERROR);
            }
            res = mysql_store_result(lpsr->conn);
            num_fields = mysql_num_fields(res);
            LIXA_TRACE(("lixa_my_recover_core: num_fields=%d\n", num_fields));
            while ((row = mysql_fetch_row(res)) && (out_count < count)) {
                const char *formatID = row[0] ? row[0] : "";
                const char *gtrid_length = row[1] ? row[1] : "";
                const char *bqual_length = row[2] ? row[2] : "";
                const char *data = row[3] ? row[3] : "";
                LIXA_TRACE(("lixa_my_recover_core: formatID=%s, "
                            "gtrid_length=%s, bqual_length=%s, data=%s\n",
                            formatID, gtrid_length, bqual_length, data));
                if (lixa_my_xid_deserialize(&xid, formatID, gtrid_length,
                                            bqual_length, data,
					    lpsr->rm_subtype)) {
                    xids[out_count++] = xid;
                } else {
                    LIXA_TRACE(("lixa_my_recover_core: unable to deserialize "
                                "the XID retrieved with XA RECOVER\n"));
                    LIXA_SYSLOG((LOG_WARNING, LIXA_SYSLOG_LXC042W, data));
                }
            }
            mysql_free_result(res);
        }
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case INVALID_FLAGS:
            case INVALID_OPTIONS1:
            case INVALID_OPTIONS2:
                xa_rc = XAER_INVAL;
                break;
            case PROTOCOL_ERROR2:
                xa_rc = XAER_PROTO;
                break;
            case NULL_CONN:
                xa_rc = XAER_RMFAIL;
                break;
            case XA_RECOVER_ERROR:
                xa_rc = XAER_RMFAIL;
                break;
            case NONE:
                xa_rc = out_count;
                break;
            default:
                xa_rc = XAER_RMFAIL;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("lixa_my_recover_core/excp=%d/xa_rc=%d\n", excp, xa_rc));
    return xa_rc;
}



int lixa_my_forget(const XID *xid, int rmid, long flags)
{
    /* this Resource Manager does not heuristically resolve the transactions
       and this function should never be called */
    return XAER_PROTO;
}



int lixa_my_forget_core(struct lixa_sw_status_rm_s *lpsr,
                        const XID *xid, int rmid, long flags)
{
    /* this Resource Manager does not heuristically resolve the transactions
       and this function should never be called */
    return XAER_PROTO;
}



int lixa_my_complete(int *handle, int *retval, int rmid, long flags)
{
    /* asynchronous mode is not supported by this wrapper */
    return XAER_INVAL;
}



MYSQL *lixa_my_get_conn_by_rmid(int rmid) {
    return (MYSQL *)lixa_sw_get_conn_by_rmid(
        rmid, LIXA_SW_STATUS_RM_TYPE_MYSQL);
}



MYSQL *lixa_my_get_conn_by_pos(int pos)
{
    return lixa_sw_get_conn_by_pos(pos, LIXA_SW_STATUS_RM_TYPE_MYSQL);
}



MYSQL *lixa_my_get_conn(void)
{
    return lixa_sw_get_conn_by_pos(0, LIXA_SW_STATUS_RM_TYPE_MYSQL);
}
