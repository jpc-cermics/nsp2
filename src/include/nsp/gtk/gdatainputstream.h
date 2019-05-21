/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGDataInputStream
#define NSP_INC_NspGDataInputStream

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

/* NspGDataInputStream */

#include <nsp/gtk/gbufferedinputstream.h>

/*
 * NspGDataInputStream inherits from GBufferedInputStream
 * just change some type attributes 
 */

typedef NspGBufferedInputStream NspGDataInputStream ;
typedef NspTypeGBufferedInputStream NspTypeGDataInputStream ;

extern int nsp_type_gdatainputstream_id;
extern NspTypeGDataInputStream *nsp_type_gdatainputstream;

/* type instances for gbufferedinputstream */

NspTypeGDataInputStream *new_type_gdatainputstream(type_mode mode);

/* instance for NspGDataInputStream */

NspGDataInputStream *new_gdatainputstream();

/*
 * Object methods redefined for gdatainputstream 
 */

#define NULLGDATAINPUTSTREAM (NspGDataInputStream*) 0


/* from NspGDataInputStreamObj.c */

extern NspGDataInputStream *nsp_gdatainputstream_object (NspObject *O);
extern int IsGDataInputStreamObj (Stack stack, int i);
extern int IsGDataInputStream(NspObject *O);
extern NspGDataInputStream *GetGDataInputStreamCopy (Stack stack, int i);
extern NspGDataInputStream *GetGDataInputStream (Stack stack, int i);

#endif /* NSP_INC_NspGDataInputStream */ 

#ifdef NspGDataInputStream_Private 
static int init_gdatainputstream(NspGDataInputStream *o,NspTypeGDataInputStream *type);
static char *nsp_gdatainputstream_type_as_string(void);
static char *nsp_gdatainputstream_type_short_string(NspObject *v);
static AttrTab gdatainputstream_attrs[];
static NspMethods *gdatainputstream_get_methods(void);
/* static int int_gdatainputstream_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGDataInputStream_Private */
