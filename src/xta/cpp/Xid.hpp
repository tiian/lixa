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
#ifndef XID_HPP
# define XID_HPP



namespace xta {
    class Xid {
        public:
        /*
         * Create a new Transaction Identifier object, this class is intended
         * only to be a proxy of the C base class: it does not allocate and/or
         * create a real identifier, just map an existent one
         * @param[in] xid is the C base object that will be wrapped
         */
        Xid(const xta_xid_t *xid);
        ~Xid();
        /**
         * Convert the transaction ID to an ASCII string
         */
        std::string toString();
        
        private:
        const xta_xid_t    *xid;
    };
};



#endif /* XID_HPP */
