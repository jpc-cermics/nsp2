/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkSourceBufferOutputStream
#define NSP_INC_NspGtkSourceBufferOutputStream

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

/* NspGtkSourceBufferOutputStream */

#include <nsp/gtk/goutputstream.h>

/*
 * NspGtkSourceBufferOutputStream inherits from GOutputStream
 * just change some type attributes 
 */

typedef NspGOutputStream NspGtkSourceBufferOutputStream ;
typedef NspTypeGOutputStream NspTypeGtkSourceBufferOutputStream ;

extern int nsp_type_gtksourcebufferoutputstream_id;
extern NspTypeGtkSourceBufferOutputStream *nsp_type_gtksourcebufferoutputstream;

/* type instances for goutputstream */

NspTypeGtkSourceBufferOutputStream *new_type_gtksourcebufferoutputstream(type_mode mode);

/* instance for NspGtkSourceBufferOutputStream */

NspGtkSourceBufferOutputStream *new_gtksourcebufferoutputstream();

/*
 * Object methods redefined for gtksourcebufferoutputstream 
 */

#define NULLGTKSOURCEBUFFEROUTPUTSTREAM (NspGtkSourceBufferOutputStream*) 0


/* from NspGtkSourceBufferOutputStreamObj.c */

extern NspGtkSourceBufferOutputStream *nsp_gtksourcebufferoutputstream_object (NspObject *O);
extern int IsGtkSourceBufferOutputStreamObj (Stack stack, int i);
extern int IsGtkSourceBufferOutputStream(NspObject *O);
extern NspGtkSourceBufferOutputStream *GetGtkSourceBufferOutputStreamCopy (Stack stack, int i);
extern NspGtkSourceBufferOutputStream *GetGtkSourceBufferOutputStream (Stack stack, int i);

#endif /* NSP_INC_NspGtkSourceBufferOutputStream */ 

#ifdef NspGtkSourceBufferOutputStream_Private 
static int init_gtksourcebufferoutputstream(NspGtkSourceBufferOutputStream *o,NspTypeGtkSourceBufferOutputStream *type);
static char *nsp_gtksourcebufferoutputstream_type_as_string(void);
static char *nsp_gtksourcebufferoutputstream_type_short_string(NspObject *v);
static AttrTab gtksourcebufferoutputstream_attrs[];
static NspMethods *gtksourcebufferoutputstream_get_methods(void);
/* static int int_gtksourcebufferoutputstream_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkSourceBufferOutputStream_Private */
