/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkTextBuffer
#define NSP_INC_NspGtkTextBuffer

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

/* NspGtkTextBuffer */

#include <nsp/gtk/gobject.h>

/*
 * NspGtkTextBuffer inherits from GObject
 * just change some type attributes 
 */

typedef NspGObject NspGtkTextBuffer ;
typedef NspTypeGObject NspTypeGtkTextBuffer ;

extern int nsp_type_gtktextbuffer_id;
extern NspTypeGtkTextBuffer *nsp_type_gtktextbuffer;

/* type instances for gobject */

NspTypeGtkTextBuffer *new_type_gtktextbuffer(type_mode mode);

/* instance for NspGtkTextBuffer */

NspGtkTextBuffer *new_gtktextbuffer();

/*
 * Object methods redefined for gtktextbuffer 
 */

#define NULLGTKTEXTBUFFER (NspGtkTextBuffer*) 0


/* from NspGtkTextBufferObj.c */

extern NspGtkTextBuffer *nsp_gtktextbuffer_object (NspObject *O);
extern int IsGtkTextBufferObj (Stack stack, int i);
extern int IsGtkTextBuffer(NspObject *O);
extern NspGtkTextBuffer *GetGtkTextBufferCopy (Stack stack, int i);
extern NspGtkTextBuffer *GetGtkTextBuffer (Stack stack, int i);

#endif /* NSP_INC_NspGtkTextBuffer */ 

#ifdef NspGtkTextBuffer_Private 
static int init_gtktextbuffer(NspGtkTextBuffer *o,NspTypeGtkTextBuffer *type);
static char *nsp_gtktextbuffer_type_as_string(void);
static char *nsp_gtktextbuffer_type_short_string(NspObject *v);
static AttrTab gtktextbuffer_attrs[];
static NspMethods *gtktextbuffer_get_methods(void);
/* static int int_gtktextbuffer_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkTextBuffer_Private */
