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
#ifndef CONFIG_HPP
# define CONFIG_HPP



/* include C++ standard header files */
#include <string>

/* include XTA header file */
#include "xta.h"
#include "Xid.hpp"
#include "XaResource.hpp"



using namespace std;



namespace xta {
    /**
     * XTA Config Manager class
     */
    class Config {
        public:
        ~Config();
        /**
         * Retrieve connection timeout
         * @return the value of connection timeout
         */
        int getConnectionTimeout();
        /**
         * Set a new connection timeout
         * @param[in] value will be the new connection timeout
         */
        void setConnectionTimeout(int value);
        private:
        /**
         * A Config object can be created only if the base C object has
         * been already created (this is just a wrapper, not a native C++
         * implementation)
         */
        Config(void);
        Config(xta_config_t *config);
        friend class Transaction;
        /**
         * Pointer to the native C object
         */
        xta_config_t *config;
    };
};



#endif /* CONFIG_HPP */
