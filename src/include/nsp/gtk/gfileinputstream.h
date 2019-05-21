/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGFileInputStream
#define NSP_INC_NspGFileInputStream

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

/* NspGFileInputStream */

#include <nsp/gtk/ginputstream.h>

/*
 * NspGFileInputStream inherits from GInputStream
 * just change some type attributes 
 */

typedef NspGInputStream NspGFileInputStream ;
typedef NspTypeGInputStream NspTypeGFileInputStream ;

extern int nsp_type_gfileinputstream_id;
extern NspTypeGFileInputStream *nsp_type_gfileinputstream;

/* type instances for ginputstream */

NspTypeGFileInputStream *new_type_gfileinputstream(type_mode mode);

/* instance for NspGFileInputStream */

NspGFileInputStream *new_gfileinputstream();

/*
 * Object methods redefined for gfileinputstream 
 */

#define NULLGFILEINPUTSTREAM (NspGFileInputStream*) 0


/* from NspGFileInputStreamObj.c */

extern NspGFileInputStream *nsp_gfileinputstream_object (NspObject *O);
extern int IsGFileInputStreamObj (Stack stack, int i);
extern int IsGFileInputStream(NspObject *O);
extern NspGFileInputStream *GetGFileInputStreamCopy (Stack stack, int i);
extern NspGFileInputStream *GetGFileInputStream (Stack stack, int i);

#endif /* NSP_INC_NspGFileInputStream */ 

#ifdef NspGFileInputStream_Private 
static int init_gfileinputstream(NspGFileInputStream *o,NspTypeGFileInputStream *type);
static char *nsp_gfileinputstream_type_as_string(void);
static char *nsp_gfileinputstream_type_short_string(NspObject *v);
static AttrTab gfileinputstream_attrs[];
static NspMethods *gfileinputstream_get_methods(void);
/* static int int_gfileinputstream_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGFileInputStream_Private */
