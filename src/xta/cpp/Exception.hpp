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
#ifndef EXCEPTION_HPP
# define EXCEPTION_HPP



/* include C++ standard header files */
#include <stdexcept>
#include <string>

/* include XTA header file */
#include "xta.h"



using namespace std;



namespace xta {

    /**
     * XTA exception class, it extends standard exception class and adds the
     * return code property that maps on C API.
     * It inherits from stdexcept::runtime_error, so you can simply catch
     * "exception" or "runtime_error", but add LIXA specific info: the reason
     * code returned by the failed C XTA function and the name of the C XTA
     * function that failed
     */
    class Exception : public runtime_error {
        private:
        /**
         * LIXA return code returned by the failed C function
         */
        int ReturnCode;
        /**
         * Name of the function where exception happened
         */
        string Function;
        
        public:
        /**
         * Default constructor
         * @param[in] ret_cod returned by the failed C XTA function
         * @param[in] function name where the expection has been raised
         */
        Exception(int ret_cod, const string& function) :
            runtime_error(lixa_strerror(ret_cod)) {
            ReturnCode = ret_cod;
            Function = function;
        }
        ~Exception() throw() {;}
        const string& where() { return Function; }
        /**
         * Retrieve the numeric return code of the failed C function
         * @return a LIXA/XTA return code
         */
        int getReturnCode() { return ReturnCode; }
        /**
         * Retrieve the description associated to the numeric return code of
         * the failed C function
         * @return the human readable description associated to the return code
         *         of the failed XTA function
         */
        string getReturnCodeText() {
            return (string(lixa_strerror(ReturnCode))); }
    };

};



#endif /* EXCEPTION_HPP */
