/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkSourceBuffer
#define NSP_INC_NspGtkSourceBuffer

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

/* NspGtkSourceBuffer */

#include <nsp/gtk/gtktextbuffer.h>

/*
 * NspGtkSourceBuffer inherits from GtkTextBuffer
 * just change some type attributes 
 */

typedef NspGtkTextBuffer NspGtkSourceBuffer ;
typedef NspTypeGtkTextBuffer NspTypeGtkSourceBuffer ;

extern int nsp_type_gtksourcebuffer_id;
extern NspTypeGtkSourceBuffer *nsp_type_gtksourcebuffer;

/* type instances for gtktextbuffer */

NspTypeGtkSourceBuffer *new_type_gtksourcebuffer(type_mode mode);

/* instance for NspGtkSourceBuffer */

NspGtkSourceBuffer *new_gtksourcebuffer();

/*
 * Object methods redefined for gtksourcebuffer 
 */

#define NULLGTKSOURCEBUFFER (NspGtkSourceBuffer*) 0


/* from NspGtkSourceBufferObj.c */

extern NspGtkSourceBuffer *nsp_gtksourcebuffer_object (NspObject *O);
extern int IsGtkSourceBufferObj (Stack stack, int i);
extern int IsGtkSourceBuffer(NspObject *O);
extern NspGtkSourceBuffer *GetGtkSourceBufferCopy (Stack stack, int i);
extern NspGtkSourceBuffer *GetGtkSourceBuffer (Stack stack, int i);

#endif /* NSP_INC_NspGtkSourceBuffer */ 

#ifdef NspGtkSourceBuffer_Private 
static int init_gtksourcebuffer(NspGtkSourceBuffer *o,NspTypeGtkSourceBuffer *type);
static char *nsp_gtksourcebuffer_type_as_string(void);
static char *nsp_gtksourcebuffer_type_short_string(NspObject *v);
static AttrTab gtksourcebuffer_attrs[];
static NspMethods *gtksourcebuffer_get_methods(void);
/* static int int_gtksourcebuffer_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkSourceBuffer_Private */
