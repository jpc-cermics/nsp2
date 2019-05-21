/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGBufferedInputStream
#define NSP_INC_NspGBufferedInputStream

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

/* NspGBufferedInputStream */

#include <nsp/gtk/gfilterinputstream.h>

/*
 * NspGBufferedInputStream inherits from GFilterInputStream
 * just change some type attributes 
 */

typedef NspGFilterInputStream NspGBufferedInputStream ;
typedef NspTypeGFilterInputStream NspTypeGBufferedInputStream ;

extern int nsp_type_gbufferedinputstream_id;
extern NspTypeGBufferedInputStream *nsp_type_gbufferedinputstream;

/* type instances for gfilterinputstream */

NspTypeGBufferedInputStream *new_type_gbufferedinputstream(type_mode mode);

/* instance for NspGBufferedInputStream */

NspGBufferedInputStream *new_gbufferedinputstream();

/*
 * Object methods redefined for gbufferedinputstream 
 */

#define NULLGBUFFEREDINPUTSTREAM (NspGBufferedInputStream*) 0


/* from NspGBufferedInputStreamObj.c */

extern NspGBufferedInputStream *nsp_gbufferedinputstream_object (NspObject *O);
extern int IsGBufferedInputStreamObj (Stack stack, int i);
extern int IsGBufferedInputStream(NspObject *O);
extern NspGBufferedInputStream *GetGBufferedInputStreamCopy (Stack stack, int i);
extern NspGBufferedInputStream *GetGBufferedInputStream (Stack stack, int i);

#endif /* NSP_INC_NspGBufferedInputStream */ 

#ifdef NspGBufferedInputStream_Private 
static int init_gbufferedinputstream(NspGBufferedInputStream *o,NspTypeGBufferedInputStream *type);
static char *nsp_gbufferedinputstream_type_as_string(void);
static char *nsp_gbufferedinputstream_type_short_string(NspObject *v);
static AttrTab gbufferedinputstream_attrs[];
static NspMethods *gbufferedinputstream_get_methods(void);
/* static int int_gbufferedinputstream_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGBufferedInputStream_Private */
