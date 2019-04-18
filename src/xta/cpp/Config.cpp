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



/* set module trace flag */
#ifdef LIXA_TRACE_MODULE
# undef LIXA_TRACE_MODULE
#endif /* LIXA_TRACE_MODULE */
#define LIXA_TRACE_MODULE   LIXA_TRACE_MOD_XTA



#include "Exception.hpp"
#include "Config.hpp"



namespace xta {
    
    Config::Config(xta_config_t *config)
    {
        this->config = config;
    };

    Config::~Config()
    {
        /* this->tx is just a reference to an object created/destroyed by
           TransactionManager */
        this->config = NULL;
    };

    int Config::getConnectionTimeout()
    {
        if (NULL == config)
            throw Exception(LIXA_RC_NULL_OBJECT,
                            "xta_config_get_connection_timeout");
        else
            return xta_config_get_connection_timeout(config);
    }    
}
