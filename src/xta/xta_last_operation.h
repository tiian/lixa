/*
 * Copyright (c) 2009-2017, Christian Ferrari <tiian@users.sourceforge.net>
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
#ifndef XTA_LAST_OPERATION_H
# define XTA_LAST_OPERATION_H



/* save old LIXA_TRACE_MODULE and set a new value */
#ifdef LIXA_TRACE_MODULE
# define LIXA_TRACE_MODULE_SAVE LIXA_TRACE_MODULE
# undef LIXA_TRACE_MODULE
#else
# undef LIXA_TRACE_MODULE_SAVE
#endif /* LIXA_TRACE_MODULE */
#define LIXA_TRACE_MODULE      LIXA_TRACE_MOD_XTA



/*
 * @@@
 * 2017-03-24: a little bit pedantic, maybe useless, remove at the end
 */

   

/**
 * Type injected in every XTA object to catch the return code of the last
 * result; especially useful for methods that do not return a ret_cod
 */
typedef struct {
    /**
     * Name of the last called method
     */
    const char *method;
    /**
     * Internal exception thrown by the last called method
     */
    int excp;
    /**
     * Internal return code of the last called method
     */
    int ret_cod;
    /**
     * Number of last error (POSIX) caught by the last method
     */
    int error;
} xta_last_operation_properties_t;



/**
 * Macro used to inject last operation type in every XTA object
 */
#define XTA_LAST_OPERATION_PROPERTIES \
    xta_last_operation_properties_t lo_properties



/**
 * Macro that must be called at the end of every method to set the context as
 * caught by the method
 */
#define XTA_LAST_OPERATION_SET(object, excp, ret_cod) \
    { if (NULL != object) { \
        object->lo_properties.method = __func__;    \
        object->lo_properties.ret_cod = ret_cod;    \
        object->lo_properties.excp = excp;    \
        object->lo_properties.error = errno;        \
    } }



/**
 * Extract last called method for the passed XTA object
 */
#define XTA_LAST_OPERATION_GET_METHOD(object) \
    (NULL != object ? object->lo_properties.method : "")



/**
 * Extract last (internal) ret_cod for the passed XTA object
 */
#define XTA_LAST_OPERATION_GET_RET_COD(object)           \
    (NULL != object ? object->lo_properties.ret_cod : LIXA_RC_NULL_OBJECT)



/**
 * Extract last internal exception for the passed XTA object
 */
#define XTA_LAST_OPERATION_GET_EXCP(object)                          \
    (NULL != object ? object->lo_properties.excp : LIXA_RC_NULL_OBJECT)



/**
 * Extract last internal errno for the passed XTA object
 */
#define XTA_LAST_OPERATION_GET_ERROR(object)                          \
    (NULL != object ? object->lo_properties.error : LIXA_RC_NULL_OBJECT)



#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */



#ifdef __cplusplus
}
#endif /* __cplusplus */



/* restore old value of LIXA_TRACE_MODULE */
#ifdef LIXA_TRACE_MODULE_SAVE
# undef LIXA_TRACE_MODULE
# define LIXA_TRACE_MODULE LIXA_TRACE_MODULE_SAVE
# undef LIXA_TRACE_MODULE_SAVE
#endif /* LIXA_TRACE_MODULE_SAVE */



#endif /* XTA_LAST_OPERATION_H */
