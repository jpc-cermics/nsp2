/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGConverterInputStream
#define NSP_INC_NspGConverterInputStream

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

/* NspGConverterInputStream */

#include <nsp/gtk/gfilterinputstream.h>

/*
 * NspGConverterInputStream inherits from GFilterInputStream
 * just change some type attributes 
 */

typedef NspGFilterInputStream NspGConverterInputStream ;
typedef NspTypeGFilterInputStream NspTypeGConverterInputStream ;

extern int nsp_type_gconverterinputstream_id;
extern NspTypeGConverterInputStream *nsp_type_gconverterinputstream;

/* type instances for gfilterinputstream */

NspTypeGConverterInputStream *new_type_gconverterinputstream(type_mode mode);

/* instance for NspGConverterInputStream */

NspGConverterInputStream *new_gconverterinputstream();

/*
 * Object methods redefined for gconverterinputstream 
 */

#define NULLGCONVERTERINPUTSTREAM (NspGConverterInputStream*) 0


/* from NspGConverterInputStreamObj.c */

extern NspGConverterInputStream *nsp_gconverterinputstream_object (NspObject *O);
extern int IsGConverterInputStreamObj (Stack stack, int i);
extern int IsGConverterInputStream(NspObject *O);
extern NspGConverterInputStream *GetGConverterInputStreamCopy (Stack stack, int i);
extern NspGConverterInputStream *GetGConverterInputStream (Stack stack, int i);

#endif /* NSP_INC_NspGConverterInputStream */ 

#ifdef NspGConverterInputStream_Private 
static int init_gconverterinputstream(NspGConverterInputStream *o,NspTypeGConverterInputStream *type);
static char *nsp_gconverterinputstream_type_as_string(void);
static char *nsp_gconverterinputstream_type_short_string(NspObject *v);
static AttrTab gconverterinputstream_attrs[];
static NspMethods *gconverterinputstream_get_methods(void);
/* static int int_gconverterinputstream_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGConverterInputStream_Private */
