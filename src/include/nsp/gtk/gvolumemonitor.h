/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGVolumeMonitor
#define NSP_INC_NspGVolumeMonitor

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

/* NspGVolumeMonitor */

#include <nsp/gtk/gobject.h>

/*
 * NspGVolumeMonitor inherits from GObject
 * just change some type attributes 
 */

typedef NspGObject NspGVolumeMonitor ;
typedef NspTypeGObject NspTypeGVolumeMonitor ;

extern int nsp_type_gvolumemonitor_id;
extern NspTypeGVolumeMonitor *nsp_type_gvolumemonitor;

/* type instances for gobject */

NspTypeGVolumeMonitor *new_type_gvolumemonitor(type_mode mode);

/* instance for NspGVolumeMonitor */

NspGVolumeMonitor *new_gvolumemonitor();

/*
 * Object methods redefined for gvolumemonitor 
 */

#define NULLGVOLUMEMONITOR (NspGVolumeMonitor*) 0


/* from NspGVolumeMonitorObj.c */

extern NspGVolumeMonitor *nsp_gvolumemonitor_object (NspObject *O);
extern int IsGVolumeMonitorObj (Stack stack, int i);
extern int IsGVolumeMonitor(NspObject *O);
extern NspGVolumeMonitor *GetGVolumeMonitorCopy (Stack stack, int i);
extern NspGVolumeMonitor *GetGVolumeMonitor (Stack stack, int i);

#endif /* NSP_INC_NspGVolumeMonitor */ 

#ifdef NspGVolumeMonitor_Private 
static int init_gvolumemonitor(NspGVolumeMonitor *o,NspTypeGVolumeMonitor *type);
static char *nsp_gvolumemonitor_type_as_string(void);
static char *nsp_gvolumemonitor_type_short_string(NspObject *v);
static AttrTab gvolumemonitor_attrs[];
static NspMethods *gvolumemonitor_get_methods(void);
/* static int int_gvolumemonitor_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGVolumeMonitor_Private */
