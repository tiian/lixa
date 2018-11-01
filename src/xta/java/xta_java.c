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
#include "config.h"



/* set module trace flag */
#ifdef LIXA_TRACE_MODULE
# undef LIXA_TRACE_MODULE
#endif /* LIXA_TRACE_MODULE */
#define LIXA_TRACE_MODULE   LIXA_TRACE_MOD_XTA



#include <jni.h>


/* This macro is necessary to avoid header files related to native resources:
   they are not used by XTA for Java */
#define XTA_FOR_JAVA
#include "xta.h"



JNIEXPORT void JNICALL Java_org_tiian_lixa_xta_Xta_initJNI(JNIEnv *env)
{
    xta_init();
}



JNIEXPORT jstring JNICALL Java_org_tiian_lixa_xta_ErrorCodes_getText
(JNIEnv *env, jclass this_obj, jint code)
{
    return (*env)->NewStringUTF(env, lixa_strerror(code));
}
