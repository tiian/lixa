/*
 * Copyright (c) 2009-2023, Christian Ferrari <tiian@users.sourceforge.net>
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
 * Test xta_config_t and its methods
 */



/*
 * EXIT CODES:
 *  0: OK
 *  1: generic error
 */



// Standard headers
#include <iostream>
// This header is necessary for all the stuff related to XTA
#include "cpp/Xta.hpp"



int main(int argc, char *argv[])
{
    /* First parameter: connection timeout */
    int                           connection_timeout;
    /* XTA Transaction Manager object reference */
    xta::TransactionManager      *tm = NULL;
    /*
     * Check command line parameters
     */
    if (argc < 2) {
        cerr << "This program requires at least one parameter\n" ;
        return 1;
    }
    connection_timeout = strtol(argv[1], NULL, 0);

    /*
     * initialize XTA environment
     */
    xta::Xta::init();
    /*
     * create XTA objects necessary to start a transaction
     */
    try {
        // create a new XTA Transaction Manager object
        tm = new xta::TransactionManager();
        // Create a new XA global transaction and retrieve a reference from
        // the TransactionManager object
        xta::Transaction tx = tm->createTransaction();
        // retrieve transaction config object (reference)
        xta::Config config = tx.getConfig();
        cout << "Config.getConnectionTimeout() = " 
             << config.getConnectionTimeout() << endl;
        /* check the value from config file or environment variable */
        if (connection_timeout != config.getConnectionTimeout()) {
            cerr << "expected value = " << connection_timeout <<
                ", real value = " << config.getConnectionTimeout() << endl;
            return 1;
        }
        /* set a new value and check it */
        config.setConnectionTimeout(1234);
        if (1234 != config.getConnectionTimeout()) {
            cerr << "expected value = 1234, real value = "
                 << config.getConnectionTimeout() << endl;
            return 1;
        }
        
        /*
         * Delete Transaction Manager object
         */
        delete tm;
        
    } catch (xta::Exception e) {
        /*
         * what() is a standard method that describes the exception
         * where() is a method provided by XTA to describe the XTA C function
         *         that raised the exception
         * getReturnCode() is a method provided by XTA to retrieve the
         *                 integer reason code returned by XTA C function
         *                 (see file lixa_errors.h)
         */
        cerr << "Exception in function '" << e.where() <<
            "', return code description: '" << e.what() << "', " <<
            "return code: " << e.getReturnCode() << endl;
        return 1;
    }
    
    return 0;
}
