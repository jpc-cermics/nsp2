/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGInputStream
#define NSP_INC_NspGInputStream

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

/* NspGInputStream */

#include <nsp/gtk/gobject.h>

/*
 * NspGInputStream inherits from GObject
 * just change some type attributes 
 */

typedef NspGObject NspGInputStream ;
typedef NspTypeGObject NspTypeGInputStream ;

extern int nsp_type_ginputstream_id;
extern NspTypeGInputStream *nsp_type_ginputstream;

/* type instances for gobject */

NspTypeGInputStream *new_type_ginputstream(type_mode mode);

/* instance for NspGInputStream */

NspGInputStream *new_ginputstream();

/*
 * Object methods redefined for ginputstream 
 */

#define NULLGINPUTSTREAM (NspGInputStream*) 0


/* from NspGInputStreamObj.c */

extern NspGInputStream *nsp_ginputstream_object (NspObject *O);
extern int IsGInputStreamObj (Stack stack, int i);
extern int IsGInputStream(NspObject *O);
extern NspGInputStream *GetGInputStreamCopy (Stack stack, int i);
extern NspGInputStream *GetGInputStream (Stack stack, int i);

#endif /* NSP_INC_NspGInputStream */ 

#ifdef NspGInputStream_Private 
static int init_ginputstream(NspGInputStream *o,NspTypeGInputStream *type);
static char *nsp_ginputstream_type_as_string(void);
static char *nsp_ginputstream_type_short_string(NspObject *v);
static AttrTab ginputstream_attrs[];
static NspMethods *ginputstream_get_methods(void);
/* static int int_ginputstream_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGInputStream_Private */
