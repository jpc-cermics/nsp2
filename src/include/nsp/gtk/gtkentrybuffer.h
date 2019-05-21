/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkEntryBuffer
#define NSP_INC_NspGtkEntryBuffer

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

/* NspGtkEntryBuffer */

#include <nsp/gtk/gobject.h>

/*
 * NspGtkEntryBuffer inherits from GObject
 * just change some type attributes 
 */

typedef NspGObject NspGtkEntryBuffer ;
typedef NspTypeGObject NspTypeGtkEntryBuffer ;

extern int nsp_type_gtkentrybuffer_id;
extern NspTypeGtkEntryBuffer *nsp_type_gtkentrybuffer;

/* type instances for gobject */

NspTypeGtkEntryBuffer *new_type_gtkentrybuffer(type_mode mode);

/* instance for NspGtkEntryBuffer */

NspGtkEntryBuffer *new_gtkentrybuffer();

/*
 * Object methods redefined for gtkentrybuffer 
 */

#define NULLGTKENTRYBUFFER (NspGtkEntryBuffer*) 0


/* from NspGtkEntryBufferObj.c */

extern NspGtkEntryBuffer *nsp_gtkentrybuffer_object (NspObject *O);
extern int IsGtkEntryBufferObj (Stack stack, int i);
extern int IsGtkEntryBuffer(NspObject *O);
extern NspGtkEntryBuffer *GetGtkEntryBufferCopy (Stack stack, int i);
extern NspGtkEntryBuffer *GetGtkEntryBuffer (Stack stack, int i);

#endif /* NSP_INC_NspGtkEntryBuffer */ 

#ifdef NspGtkEntryBuffer_Private 
static int init_gtkentrybuffer(NspGtkEntryBuffer *o,NspTypeGtkEntryBuffer *type);
static char *nsp_gtkentrybuffer_type_as_string(void);
static char *nsp_gtkentrybuffer_type_short_string(NspObject *v);
static AttrTab gtkentrybuffer_attrs[];
static NspMethods *gtkentrybuffer_get_methods(void);
/* static int int_gtkentrybuffer_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkEntryBuffer_Private */
