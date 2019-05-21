/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkSourceBufferInputStream
#define NSP_INC_NspGtkSourceBufferInputStream

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

/* NspGtkSourceBufferInputStream */

#include <nsp/gtk/ginputstream.h>

/*
 * NspGtkSourceBufferInputStream inherits from GInputStream
 * just change some type attributes 
 */

typedef NspGInputStream NspGtkSourceBufferInputStream ;
typedef NspTypeGInputStream NspTypeGtkSourceBufferInputStream ;

extern int nsp_type_gtksourcebufferinputstream_id;
extern NspTypeGtkSourceBufferInputStream *nsp_type_gtksourcebufferinputstream;

/* type instances for ginputstream */

NspTypeGtkSourceBufferInputStream *new_type_gtksourcebufferinputstream(type_mode mode);

/* instance for NspGtkSourceBufferInputStream */

NspGtkSourceBufferInputStream *new_gtksourcebufferinputstream();

/*
 * Object methods redefined for gtksourcebufferinputstream 
 */

#define NULLGTKSOURCEBUFFERINPUTSTREAM (NspGtkSourceBufferInputStream*) 0


/* from NspGtkSourceBufferInputStreamObj.c */

extern NspGtkSourceBufferInputStream *nsp_gtksourcebufferinputstream_object (NspObject *O);
extern int IsGtkSourceBufferInputStreamObj (Stack stack, int i);
extern int IsGtkSourceBufferInputStream(NspObject *O);
extern NspGtkSourceBufferInputStream *GetGtkSourceBufferInputStreamCopy (Stack stack, int i);
extern NspGtkSourceBufferInputStream *GetGtkSourceBufferInputStream (Stack stack, int i);

#endif /* NSP_INC_NspGtkSourceBufferInputStream */ 

#ifdef NspGtkSourceBufferInputStream_Private 
static int init_gtksourcebufferinputstream(NspGtkSourceBufferInputStream *o,NspTypeGtkSourceBufferInputStream *type);
static char *nsp_gtksourcebufferinputstream_type_as_string(void);
static char *nsp_gtksourcebufferinputstream_type_short_string(NspObject *v);
static AttrTab gtksourcebufferinputstream_attrs[];
static NspMethods *gtksourcebufferinputstream_get_methods(void);
/* static int int_gtksourcebufferinputstream_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkSourceBufferInputStream_Private */
