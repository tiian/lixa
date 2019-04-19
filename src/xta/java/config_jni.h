/*
 * Copyright (c) 2009-2019, Christian Ferrari <tiian@users.sourceforge.net>
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



#include <jni.h>



/* This macro is necessary to avoid header files related to native resources:
   they are not used by XTA for Java */
#define XTA_FOR_JAVA
/* LIXA includes */
#include "lixa_errors.h"
#include "lixa_trace.h"
/* XTA includes */
#include "xta.h"




/*
 * Class:     org_tiian_lixa_xta_Config
 * Method:    newJNI
 * Signature: ()V
 */
JNIEXPORT void JNICALL Java_org_tiian_lixa_xta_Config_newJNI
  (JNIEnv *, jobject);

/*
 * Class:     org_tiian_lixa_xta_Config
 * Method:    deleteJNI
 * Signature: ()V
 */
JNIEXPORT void JNICALL Java_org_tiian_lixa_xta_Config_deleteJNI
  (JNIEnv *, jobject);

/*
 * Class:     org_tiian_lixa_xta_Config
 * Method:    getConnectionTimeout
 * Signature: ()I
 */
JNIEXPORT jint JNICALL Java_org_tiian_lixa_xta_Config_getConnectionTimeout
  (JNIEnv *, jobject);

/*
 * Class:     org_tiian_lixa_xta_Config
 * Method:    setConnectionTimeout
 * Signature: (I)V
 */
JNIEXPORT void JNICALL Java_org_tiian_lixa_xta_Config_setConnectionTimeout
  (JNIEnv *, jobject, jint);


