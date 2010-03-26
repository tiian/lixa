/*
 * Copyright (c) 2009-2010, Christian Ferrari <tiian@users.sourceforge.net>
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
#ifndef LIXA_CRASH_H
# define LIXA_CRASH_H



#include <config.h>



#include <lixa_defines.h>



/* save old LIXA_TRACE_MODULE and set a new value */
#ifdef LIXA_TRACE_MODULE
# define LIXA_TRACE_MODULE_SAVE LIXA_TRACE_MODULE
# undef LIXA_TRACE_MODULE
#else
# undef LIXA_TRACE_MODULE_SAVE
#endif /* LIXA_TRACE_MODULE */
#define LIXA_TRACE_MODULE      LIXA_TRACE_MOD_COMMON_UTILS



#ifdef _CRASH
# define LIXA_CRASH(a,b)         lixa_crash(a,b)
# define LIXA_CRASH_INIT         lixa_crash_init()
#else
# define LIXA_CRASH(a,b)
# define LIXA_CRASH_INIT
#endif



/**
 * No crash point set
 */
#define LIXA_CRASH_POINT_NULL          0
/**
 * The Transaction Manager will crash after successfully prepared all the
 * resource manager
 */
#define LIXA_CRASH_POINT_PREPARE_1     1



#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */



    /**
     * Initialize the crash simulation envinroment; initialization is a per
     * process routine: only the first thread will initialize the environtment,
     * successive calls will be ignored
     */
    void lixa_crash_init(void);
    


    /**
     * Simulate a crash if the number of calls for the specific crash point
     * has been reached
     * @param crash_point IN specifies the point in code must simulate the
     *                    crash
     * @param count IN/OUT keeps track of the number of times the crash point
     *              has been already traversed
     */
    void lixa_crash(lixa_word_t crash_point, int *count);

    

#ifdef __cplusplus
}
#endif /* __cplusplus */



/* restore old value of LIXA_TRACE_MODULE */
#ifdef LIXA_TRACE_MODULE_SAVE
# undef LIXA_TRACE_MODULE
# define LIXA_TRACE_MODULE LIXA_TRACE_MODULE_SAVE
# undef LIXA_TRACE_MODULE_SAVE
#endif /* LIXA_TRACE_MODULE_SAVE */



#endif /* LIXA_CRASH_H */
