/*
 * Copyright (c) 2009-2018, Christian Ferrari <tiian@users.sourceforge.net>
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



#ifdef HAVE_REGEX_H
# include <regex.h>
#endif
#ifdef HAVE_STRING_H
# include <string.h>
#endif



#include <lixa_errors.h>
#include <lixa_trace.h>
#include <lixanonapi.h>



/* set module trace flag */
#ifdef LIXA_TRACE_MODULE
# undef LIXA_TRACE_MODULE
#endif /* LIXA_TRACE_MODULE */
#define LIXA_TRACE_MODULE   LIXA_TRACE_MOD_CLIENT_GENERIC



int lixa_nonapi_parse_conn_string(const char *conn_string,
                                  int *use_lixa_conn,
                                  int *use_lixa_rmid,
                                  int *use_lixa_id)
{
    enum Exception { INVALID_CONN_STRING
                     , REGCOMP_ERROR
                     , REGEXEC_NO_MATCH
                     , REGEXEC_ERROR
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    regex_t preg;

    LIXA_TRACE_INIT;
    LIXA_TRACE(("lixa_nonapi_parse_conn_string\n"));
    TRY {
        int reg_error;
        char reg_errbuf[200];
        regmatch_t pmatch[3];
        size_t nmatch = sizeof(pmatch)/sizeof(regmatch_t);
        char match_buf[100];

        /* reset output values */
        *use_lixa_conn = FALSE;
        *use_lixa_rmid = FALSE;
        *use_lixa_id = 0;
        
        /* check database string, is it "lixa/[rmid-pos]{0-1}/n{0-1}" */
        if (NULL == conn_string || 0 == strlen(conn_string)) {
            LIXA_TRACE(("lixa_nonapi_parse_conn_string: conn_string is "
                        "not valid\n"));
            THROW(INVALID_CONN_STRING);
        }
        /* compile regular expression */
        reg_error = regcomp(
            &preg, "^lixa\\/(rmid|pos)?\\/([[:digit:]]*)$",
            REG_EXTENDED|REG_ICASE);
        if (0 != reg_error) {
            regerror(reg_error, &preg, reg_errbuf, sizeof(reg_errbuf));
            LIXA_TRACE(("lixa_nonapi_parse_conn_string: error while compiling "
                        "regular expression ('%s')\n", reg_errbuf));
            THROW(REGCOMP_ERROR);
        }
        /* parsing conn_string with regular expression */
        reg_error = regexec(&preg, conn_string, nmatch, pmatch, 0);
        if (REG_NOMATCH == reg_error) {
            regerror(reg_error, &preg, reg_errbuf, sizeof(reg_errbuf));
            LIXA_TRACE(("lixa_nonapi_parse_conn_string: conn_string does "
                        "not match regular expression ('%s')\n", reg_errbuf));
            THROW(REGEXEC_NO_MATCH);
        }
        if (0 != reg_error) {
            regerror(reg_error, &preg, reg_errbuf, sizeof(reg_errbuf));
            LIXA_TRACE(("lixa_nonapi_parse_conn_string: error while applying "
                        "regular expression ('%s')\n", reg_errbuf));
            THROW(REGEXEC_ERROR);
        }

        /* conn_string is a valid string, LIXA connection must be used */
        *use_lixa_conn = TRUE;
        
        /* rmid/pos detected? */
        if (pmatch[1].rm_so > 0 && pmatch[1].rm_eo > 0) {
            size_t len = pmatch[1].rm_eo - pmatch[1].rm_so;
            if (0 < len) {
                strncpy(match_buf, conn_string+pmatch[1].rm_so, len);
                match_buf[len] = '\0';
                if (0 == strcasecmp("rmid", match_buf))
                    *use_lixa_rmid = TRUE;
            }
        }
        
        /* id detected? */
        if (pmatch[2].rm_so > 0 && pmatch[2].rm_eo > 0) {
            size_t len = pmatch[2].rm_eo - pmatch[2].rm_so;
            if (0 < len) {
                long num;
                strncpy(match_buf, conn_string+pmatch[2].rm_so, len);
                match_buf[len] = '\0';
                num = strtol(match_buf, NULL, 10);
                *use_lixa_id = (int)num;
            }
        }
        
        THROW(NONE);
    } CATCH {
        /* release memory */
        if (REGCOMP_ERROR < excp)
            regfree(&preg);
        switch (excp) {
            case INVALID_CONN_STRING:
                ret_cod = LIXA_RC_NULL_OBJECT;
                break;
            case REGCOMP_ERROR:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
                break;
            case REGEXEC_NO_MATCH:
                ret_cod = LIXA_RC_OK;
                break;                
            case REGEXEC_ERROR:
                ret_cod = LIXA_RC_INVALID_OPTION;
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("lixa_nonapi_parse_conn_string/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}

