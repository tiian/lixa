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



/*
 * This is an example that shows as you can use LIXA TX (Transaction
 * Demarcation) API and WebSphereMQ API together.
 * Please refer to LIXA manual for more information about this sample.
 */



/* standard UNIX headers */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* WebSphere MQ C Interface API header */
#include <cmqc.h>

/* TX (Transaction Demarcation) header */
#include <tx.h>



int main(int argc, char **argv)
{
    int      txrc, delete, mq_no_disc;
  
    MQOD     obj_descr = {MQOD_DEFAULT};
    MQMD     msg_descr = {MQMD_DEFAULT};
    MQPMO    put_msg_opt = {MQPMO_DEFAULT};
    MQGMO    get_msg_opt = {MQGMO_DEFAULT};
    MQHCONN  mq_conn;
    MQHOBJ   mq_queue;
    MQLONG   open_opt, mq_cc, mq_rc, msg_len, buf_len;
    char     buffer[4096], mq_name[MQ_Q_MGR_NAME_LENGTH+1];
    
    if (argc > 1 && (!strcmp(argv[1], "delete") || !strcmp(argv[1], "DELETE")))
        delete = 1;
    else
        delete = 0;
    
    /* open the resource manager(s) */
    if (TX_OK != (txrc = tx_open())) {
        fprintf(stderr, "tx_open error: %d\n", txrc);
        exit(txrc);
    }

    /* connect to queue manager */
    memset(mq_name, 0, sizeof(mq_name));
    strcpy(mq_name, "LIXA");
    MQCONN(mq_name, &mq_conn, &mq_cc, &mq_rc);
    if (MQRC_ALREADY_CONNECTED == mq_rc)
        mq_no_disc = 1;
    else
        mq_no_disc = 0;
    if (MQCC_FAILED == mq_cc) {
        fprintf(stderr, "MQCONN reason code: %ld\n", mq_rc);
        exit(1);
    }
    
    /* open queue */
    memset(obj_descr.ObjectName, 0, sizeof(obj_descr.ObjectName));
    strcpy(obj_descr.ObjectName, "LIXA.QLOCAL");
    printf("target queue is %s\n", obj_descr.ObjectName);
    open_opt = MQOO_FAIL_IF_QUIESCING;
    if (delete)
        open_opt |= MQOO_INPUT_AS_Q_DEF;
    else
        open_opt |= MQOO_OUTPUT;
    MQOPEN(mq_conn, &obj_descr, open_opt, &mq_queue, &mq_cc, &mq_rc);
    if (MQCC_FAILED == mq_cc) {
        fprintf(stderr, "MQOPEN reason code: %ld\n", mq_rc);
        exit(1);
    }
    
    if (TX_OK != (txrc = tx_begin())) {
        fprintf(stderr, "tx_begin error: %d\n", txrc);
        exit(txrc);
    }

    if (delete) {
        /* retrieve the first message */
        get_msg_opt.Options = MQGMO_NO_WAIT | MQGMO_SYNCPOINT | MQGMO_CONVERT;
        buf_len = sizeof(buffer) - 1;
        memcpy(msg_descr.MsgId, MQMI_NONE, sizeof(msg_descr.MsgId));
        memcpy(msg_descr.CorrelId, MQCI_NONE, sizeof(msg_descr.CorrelId));
        msg_descr.Encoding = MQENC_NATIVE;
        msg_descr.CodedCharSetId = MQCCSI_Q_MGR;
        MQGET(mq_conn, mq_queue, &msg_descr, &get_msg_opt, 
              buf_len, buffer, &msg_len, &mq_cc, &mq_rc);
        if (MQRC_NONE != mq_rc) {
            if (MQRC_NO_MSG_AVAILABLE == mq_rc)
                printf("No messages available\n");
            else
                fprintf(stderr, "MQGET reason code: %ld\n", mq_rc);
        }
        if (MQCC_FAILED != mq_cc) {
            buffer[msg_len] = '\0';
            printf("Message retrieved from queue %s: '%s'\n", 
                   obj_descr.ObjectName, buffer);
        }
    } else {
        /* put a message */
        memcpy(msg_descr.Format, MQFMT_STRING, (size_t)MQ_FORMAT_LENGTH);
        put_msg_opt.Options = MQPMO_SYNCPOINT | MQPMO_FAIL_IF_QUIESCING | 
            MQPMO_NEW_MSG_ID | MQPMO_NEW_CORREL_ID;
        strcpy(buffer, "Test message for LIXA");
        msg_len = strlen(buffer);
        MQPUT(mq_conn, mq_queue, &msg_descr, &put_msg_opt, 
              msg_len, buffer, &mq_cc, &mq_rc);
        if (MQCC_FAILED == mq_cc) {
            fprintf(stderr, "MQPUT reason code: %ld\n", mq_rc);
            exit(1);
        }
        printf("Message inserted in queue %s: '%s'\n", 
               obj_descr.ObjectName, buffer);
    }

    if (TX_OK != (txrc = tx_commit())) {
        fprintf(stderr, "tx_commit error: %d\n", txrc);
        exit(txrc);
    }

    /*
    if (TX_OK != (txrc = tx_rollback())) {
        fprintf(stderr, "tx_rollback error: %d\n", txrc);
        exit(txrc);
    }
    */
    
    /* close queue */
    MQCLOSE(mq_conn, &mq_queue, MQCO_NONE, &mq_cc, &mq_rc);
    if (MQCC_FAILED == mq_cc) {
        printf("MQCLOSE reason code: %ld\n", mq_rc);
        exit(1);
    }
    
    /* disconnect from queue manager */
    if (!mq_no_disc) {
        MQDISC(&mq_conn, &mq_cc, &mq_rc);
        if (MQCC_FAILED == mq_cc) {
            printf("MQDISC reason code: %ld\n", mq_rc);
            exit(1);
        }
    }
    
    /* close the resource manager(s) */
    if (TX_OK != (txrc = tx_close())) {
        fprintf(stderr, "tx_close error: %d\n", txrc);
        exit(txrc);
    }

    return 0;
}
