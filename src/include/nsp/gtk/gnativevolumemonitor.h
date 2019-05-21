/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGNativeVolumeMonitor
#define NSP_INC_NspGNativeVolumeMonitor

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

/* NspGNativeVolumeMonitor */

#include <nsp/gtk/gvolumemonitor.h>

/*
 * NspGNativeVolumeMonitor inherits from GVolumeMonitor
 * just change some type attributes 
 */

typedef NspGVolumeMonitor NspGNativeVolumeMonitor ;
typedef NspTypeGVolumeMonitor NspTypeGNativeVolumeMonitor ;

extern int nsp_type_gnativevolumemonitor_id;
extern NspTypeGNativeVolumeMonitor *nsp_type_gnativevolumemonitor;

/* type instances for gvolumemonitor */

NspTypeGNativeVolumeMonitor *new_type_gnativevolumemonitor(type_mode mode);

/* instance for NspGNativeVolumeMonitor */

NspGNativeVolumeMonitor *new_gnativevolumemonitor();

/*
 * Object methods redefined for gnativevolumemonitor 
 */

#define NULLGNATIVEVOLUMEMONITOR (NspGNativeVolumeMonitor*) 0


/* from NspGNativeVolumeMonitorObj.c */

extern NspGNativeVolumeMonitor *nsp_gnativevolumemonitor_object (NspObject *O);
extern int IsGNativeVolumeMonitorObj (Stack stack, int i);
extern int IsGNativeVolumeMonitor(NspObject *O);
extern NspGNativeVolumeMonitor *GetGNativeVolumeMonitorCopy (Stack stack, int i);
extern NspGNativeVolumeMonitor *GetGNativeVolumeMonitor (Stack stack, int i);

#endif /* NSP_INC_NspGNativeVolumeMonitor */ 

#ifdef NspGNativeVolumeMonitor_Private 
static int init_gnativevolumemonitor(NspGNativeVolumeMonitor *o,NspTypeGNativeVolumeMonitor *type);
static char *nsp_gnativevolumemonitor_type_as_string(void);
static char *nsp_gnativevolumemonitor_type_short_string(NspObject *v);
static AttrTab gnativevolumemonitor_attrs[];
static NspMethods *gnativevolumemonitor_get_methods(void);
/* static int int_gnativevolumemonitor_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGNativeVolumeMonitor_Private */
