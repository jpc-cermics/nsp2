/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGFileMonitor
#define NSP_INC_NspGFileMonitor

/*
 * Copyright (C) 1998-2019 Jean-Philippe Chancelier Enpc/Cermics
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 */

/* NspGFileMonitor */

#include <nsp/gtk/gobject.h>

/*
 * NspGFileMonitor inherits from GObject
 * just change some type attributes 
 */

typedef NspGObject NspGFileMonitor ;
typedef NspTypeGObject NspTypeGFileMonitor ;

extern int nsp_type_gfilemonitor_id;
extern NspTypeGFileMonitor *nsp_type_gfilemonitor;

/* type instances for gobject */

NspTypeGFileMonitor *new_type_gfilemonitor(type_mode mode);

/* instance for NspGFileMonitor */

NspGFileMonitor *new_gfilemonitor();

/*
 * Object methods redefined for gfilemonitor 
 */

#define NULLGFILEMONITOR (NspGFileMonitor*) 0


/* from NspGFileMonitorObj.c */

extern NspGFileMonitor *nsp_gfilemonitor_object (NspObject *O);
extern int IsGFileMonitorObj (Stack stack, int i);
extern int IsGFileMonitor(NspObject *O);
extern NspGFileMonitor *GetGFileMonitorCopy (Stack stack, int i);
extern NspGFileMonitor *GetGFileMonitor (Stack stack, int i);

#endif /* NSP_INC_NspGFileMonitor */ 

#ifdef NspGFileMonitor_Private 
static int init_gfilemonitor(NspGFileMonitor *o,NspTypeGFileMonitor *type);
static char *nsp_gfilemonitor_type_as_string(void);
static char *nsp_gfilemonitor_type_short_string(NspObject *v);
static AttrTab gfilemonitor_attrs[];
static NspMethods *gfilemonitor_get_methods(void);
/* static int int_gfilemonitor_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGFileMonitor_Private */
