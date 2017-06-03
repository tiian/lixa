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
#ifndef XTA_IFACE_H
# define XTA_IFACE_H



/**
 * XTA interface that are used to collect the function pointers
 */
struct xta_iface_s {
    /**
     * name of resource manager
     */
    char name[RMNAMESZ];
    /**
     * options specific to the resource manager
     */
    long flags;
    /**
     * must be 0
     */
    long version;
    /**
     * xa_open function pointer
     */
    int (*xa_open_entry)(char *, int);
    /**
     * xa_close function pointer
     */
    int (*xa_close_entry)(char *, int);
    /**
     * xa_start function pointer
     */
    int (*xa_start_entry)(const XID *, int, long);
    /**
     * xa_end function pointer
     */
    int (*xa_end_entry)(XID *, int, long); 
    /**
     * xa_rollback function pointer
     */
    int (*xa_rollback_entry)(XID *, int);
    /**
     * xa_prepare function pointer
     */
    int (*xa_prepare_entry)(XID *, int);
    /**
     * xa_commit function pointer
     */
    int (*xa_commit_entry)(XID *, int, long); 
    /**
     * xa_recover function pointer
     */
    int (*xa_recover_entry)(XID *, long, int, long);
    /**
     * xa_forget function pointer
     */
    int (*xa_forget_entry)(XID *, int); 
};


    
#endif /* XTA_IFACE_H */
