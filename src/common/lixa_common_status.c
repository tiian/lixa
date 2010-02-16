/*
 * Copyright (c) 2009, Christian Ferrari
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. Neither the names of the copyright holders nor the names of its
 *    contributors may be used to endorse or promote products derived from
 *    this software without specific prior written permission.
 *
 * Alternatively, this software may be distributed under the terms of the
 * GNU General Public License ("GPL") version 2 as published by the Free
 * Software Foundation.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 */
#include <config.h>



#ifdef HAVE_ARPA_INET_H
# include <arpa/inet.h>
#endif
#ifdef HAVE_SYS_SOCKET_H
# include <sys/socket.h>
#endif



#include <lixa_errors.h>
#include <lixa_common_status.h>



/* set module trace flag */
#ifdef LIXA_TRACE_MODULE
# undef LIXA_TRACE_MODULE
#endif /* LIXA_TRACE_MODULE */
#define LIXA_TRACE_MODULE   LIXA_TRACE_MOD_COMMON_STATUS



uuid_t lixa_xid_global_bqual;



void xid_create_new(XID *xid)
{
    uuid_t uuid_obj;
    char *gtrid, *bqual;
    
    xid->formatID = LIXA_XID_FORMAT_ID;
    xid->gtrid_length = sizeof(uuid_t);
    xid->bqual_length = sizeof(uuid_t);
    memset(xid->data, 0, XIDDATASIZE); /* this is not necessary, but... */
    uuid_generate(uuid_obj);
    memcpy(xid->data, uuid_obj, sizeof(uuid_t));
    memcpy(xid->data + sizeof(uuid_t), lixa_xid_global_bqual, sizeof(uuid_t));
#ifdef _TRACE
    gtrid = xid_get_gtrid_ascii(xid);
    bqual = xid_get_bqual_ascii(xid);
    if (NULL != gtrid && NULL != bqual)
        LIXA_TRACE(("xid_create_new: gtrid='%s'; bqual='%s'\n", gtrid, bqual));
    if (NULL != bqual) free(bqual);
    if (NULL != gtrid) free(gtrid);
#endif /* _TRACE */
}



char *xid_get_gtrid_ascii(const XID *xid)
{
    char *gtrid;
    if (NULL == (gtrid = (char *)malloc(2*sizeof(uuid_t)+4+1)))
        return NULL;
    uuid_unparse((unsigned char *)xid->data, gtrid);
    return gtrid;
}



char *xid_get_bqual_ascii(const XID *xid)
{
    char *bqual;
    if (NULL == (bqual = (char *)malloc(2*sizeof(uuid_t)+4+1)))
        return NULL;
    uuid_unparse((unsigned char *)xid->data + sizeof(uuid_t), bqual);
    return bqual;
}



char *xid_serialize(const XID *xid)
{
    char *ser;
    if (NULL == (ser = (char *)malloc(2*(2*sizeof(uuid_t)+4)+2)))
        return NULL;
    uuid_unparse((unsigned char *)xid->data, ser);
    uuid_unparse((unsigned char *)xid->data + sizeof(uuid_t),
                 ser + (2*sizeof(uuid_t)+4+1));
    ser[2*sizeof(uuid_t)+4] = LIXA_XID_SEPARATOR;
    return ser;
}



int xid_deserialize(char *ser_xid, XID *xid)
{
    enum Exception { SEPARATOR_NOT_FOUND
                     , UUID_PARSE_ERROR1
                     , UUID_PARSE_ERROR2
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("xid_deserialize\n"));
    TRY {
        uuid_t uuid_obj;
        
        /* check separator */
        if (LIXA_XID_SEPARATOR != ser_xid[2*sizeof(uuid_t)+4])
            THROW(SEPARATOR_NOT_FOUND);
        /* initialize the format */
        xid->formatID = LIXA_XID_FORMAT_ID;
        /* separates gtrid from bqual strings */
        ser_xid[2*sizeof(uuid_t)+4] = '\0';
        /* parse & store gtrid */
        LIXA_TRACE(("xid_deserialize: gtrid='%s'\n", ser_xid));
        if (0 != uuid_parse(ser_xid, uuid_obj))
            THROW(UUID_PARSE_ERROR1);
        memcpy(xid->data, uuid_obj, sizeof(uuid_t));
        xid->gtrid_length = sizeof(uuid_t);
        /* parse & store bqual */
        LIXA_TRACE(("xid_deserialize: bqual='%s'\n",
                    ser_xid + (2*sizeof(uuid_t)+4+1)));
        if (0 != uuid_parse(ser_xid + (2*sizeof(uuid_t)+4+1), uuid_obj))
            THROW(UUID_PARSE_ERROR2);
        memcpy(xid->data + sizeof(uuid_t), uuid_obj, sizeof(uuid_t));
        xid->bqual_length = sizeof(uuid_t);
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case SEPARATOR_NOT_FOUND:
                ret_cod = LIXA_RC_OBJ_CORRUPTED;
                break;
            case UUID_PARSE_ERROR1:
            case UUID_PARSE_ERROR2:
                ret_cod = LIXA_RC_UUID_PARSE_ERROR;
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
        /* restore original separator */
        if (SEPARATOR_NOT_FOUND < ret_cod)
            ser_xid[2*sizeof(uuid_t)+4] = LIXA_XID_SEPARATOR;
    } /* TRY-CATCH */
    LIXA_TRACE(("xid_deserialize/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



int lixa_job_set_raw(lixa_job_t *job, const char *raw_job)
{
    enum Exception { NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;

    int truncated = FALSE;
    
    LIXA_TRACE(("lixa_job_set_raw\n"));
    TRY {
        size_t raw_size = strlen(raw_job);
        
        if (raw_size >= LIXA_JOB_RAW_LEN) {
            raw_size = LIXA_JOB_RAW_LEN - 1;
            truncated = TRUE;
        }
        
        memset(job->fields.source_ip, ' ', LIXA_JOB_RAW_LEN - 1);
        strncpy(job->raw, raw_job, raw_size);
        job->raw[LIXA_JOB_RAW_LEN] = '\0';

        THROW(NONE);
    } CATCH {
        switch (excp) {
            case NONE:
                ret_cod = truncated ? LIXA_RC_TRUNCATION_OCCURRED : LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("lixa_job_set_raw/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



int lixa_job_set_path_profile(lixa_job_t *job, const char *path,
                              const char *profile)
{
    enum Exception { G_CHECKSUM_NEW_ERROR
                     , G_CHECKSUM_GET_STRING_ERROR
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    GChecksum *checksum = NULL;

    LIXA_TRACE(("\n"));
    TRY {
        const gchar *tmp = NULL;
        
        /* create a new checksum */
        if (NULL == (checksum = g_checksum_new(G_CHECKSUM_MD5)))
            THROW(G_CHECKSUM_NEW_ERROR);
        g_checksum_update(checksum, (guchar *)path, strlen(path));
        g_checksum_update(checksum, (guchar *)profile, strlen(profile));

        if (NULL == (tmp = g_checksum_get_string(checksum)))
            THROW(G_CHECKSUM_GET_STRING_ERROR);
        strncpy(job->fields.path_profile_digest, (const char *)tmp,
                MD5_DIGEST_LENGTH * 2);
        strncpy(job->fields.profile, profile, strlen(profile));
        job->fields.terminator = '\0';
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case G_CHECKSUM_NEW_ERROR:
                ret_cod = LIXA_RC_G_CHECKSUM_NEW_ERROR;
                break;
            case G_CHECKSUM_GET_STRING_ERROR:
                ret_cod = LIXA_RC_G_CHECKSUM_GET_STRING_ERROR;
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}



int lixa_job_set_source_ip(lixa_job_t *job, int fd)
{
    enum Exception { GETSOCKNAME_ERROR
                     , NONE } excp;
    int ret_cod = LIXA_RC_INTERNAL_ERROR;
    
    LIXA_TRACE(("lixa_job_set_source_ip\n"));
    TRY {
        struct sockaddr_in local_sock_addr;
        socklen_t serv_addr_len;
        static GStaticMutex mutex = G_STATIC_MUTEX_INIT;
        char *tmp_source_ip;
        size_t source_ip_len = 0;
        
        serv_addr_len = sizeof(struct sockaddr_in);
        if (0 != getsockname(fd, (struct sockaddr *)&local_sock_addr,
                             &serv_addr_len))
            THROW(GETSOCKNAME_ERROR);        

        memset(job->fields.source_ip, ' ', LIXA_JOB_SOURCE_IP_LEN);
        g_static_mutex_lock(&mutex);
        tmp_source_ip = inet_ntoa(local_sock_addr.sin_addr);
        source_ip_len = strlen(tmp_source_ip);
        if (source_ip_len > LIXA_JOB_SOURCE_IP_LEN)
            source_ip_len = LIXA_JOB_SOURCE_IP_LEN;
        strncpy(job->fields.source_ip, tmp_source_ip, source_ip_len);
        g_static_mutex_unlock(&mutex);
        job->fields.terminator = '\0';
        
        THROW(NONE);
    } CATCH {
        switch (excp) {
            case GETSOCKNAME_ERROR:
                ret_cod = LIXA_RC_GETSOCKNAME_ERROR;
                break;
            case NONE:
                ret_cod = LIXA_RC_OK;
                break;
            default:
                ret_cod = LIXA_RC_INTERNAL_ERROR;
        } /* switch (excp) */
    } /* TRY-CATCH */
    LIXA_TRACE(("lixa_job_set_source_ip/excp=%d/"
                "ret_cod=%d/errno=%d\n", excp, ret_cod, errno));
    return ret_cod;
}

