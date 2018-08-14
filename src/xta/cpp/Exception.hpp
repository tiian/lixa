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
#ifndef EXCEPTION_HPP
# define EXCEPTION_HPP



/* include C++ standard header files */
#include <exception>
#include <string>

/* include XTA header file */
#include "xta.h"



using namespace std;



namespace xta {

    /**
     * XTA exception class, it extends standard exception class and adds the
     * return code property that maps on C API
     */
    class Exception : public exception {
        private:
        /**
         * Return code returned by the failed C function
         */
        int ReturnCode;
        string Function;
        public:
        virtual const char* what() const throw()
        {
            return lixa_strerror(ReturnCode);
        }
        /**
         * @param[in] ret_cod : the return code of the failed C function
         */
        Exception(int ret_cod, const string& func) {
            ReturnCode = ret_cod;
            Function = func;
        }
        ~Exception() throw() {;}
        const string& where() { return Function; }
        /**
         * Retrieve the numeric return code of the failed C function
         */
        int getReturnCode() { return ReturnCode; }
        /**
         * Retrieve the description associated to the numeric return code of
         * the failed C function
         */
        string getReturnCodeText() {
            return (string(lixa_strerror(ReturnCode))); }
    };

};



#endif /* EXCEPTION_HPP */
