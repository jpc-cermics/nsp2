/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGVolume
#define NSP_INC_NspGVolume

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

/* NspGVolume */

#include <nsp/gtk/gobject.h>

/*
 * NspGVolume inherits from GObject
 * just change some type attributes 
 */

typedef NspGObject NspGVolume ;
typedef NspTypeGObject NspTypeGVolume ;

extern int nsp_type_gvolume_id;
extern NspTypeGVolume *nsp_type_gvolume;

/* type instances for gobject */

NspTypeGVolume *new_type_gvolume(type_mode mode);

/* instance for NspGVolume */

NspGVolume *new_gvolume();

/*
 * Object methods redefined for gvolume 
 */

#define NULLGVOLUME (NspGVolume*) 0


/* from NspGVolumeObj.c */

extern NspGVolume *nsp_gvolume_object (NspObject *O);
extern int IsGVolumeObj (Stack stack, int i);
extern int IsGVolume(NspObject *O);
extern NspGVolume *GetGVolumeCopy (Stack stack, int i);
extern NspGVolume *GetGVolume (Stack stack, int i);

#endif /* NSP_INC_NspGVolume */ 

#ifdef NspGVolume_Private 
static int init_gvolume(NspGVolume *o,NspTypeGVolume *type);
static char *nsp_gvolume_type_as_string(void);
static char *nsp_gvolume_type_short_string(NspObject *v);
static AttrTab gvolume_attrs[];
static NspMethods *gvolume_get_methods(void);
/* static int int_gvolume_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGVolume_Private */
