/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGFilterOutputStream
#define NSP_INC_NspGFilterOutputStream

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

/* NspGFilterOutputStream */

#include <nsp/gtk/goutputstream.h>

/*
 * NspGFilterOutputStream inherits from GOutputStream
 * just change some type attributes 
 */

typedef NspGOutputStream NspGFilterOutputStream ;
typedef NspTypeGOutputStream NspTypeGFilterOutputStream ;

extern int nsp_type_gfilteroutputstream_id;
extern NspTypeGFilterOutputStream *nsp_type_gfilteroutputstream;

/* type instances for goutputstream */

NspTypeGFilterOutputStream *new_type_gfilteroutputstream(type_mode mode);

/* instance for NspGFilterOutputStream */

NspGFilterOutputStream *new_gfilteroutputstream();

/*
 * Object methods redefined for gfilteroutputstream 
 */

#define NULLGFILTEROUTPUTSTREAM (NspGFilterOutputStream*) 0


/* from NspGFilterOutputStreamObj.c */

extern NspGFilterOutputStream *nsp_gfilteroutputstream_object (NspObject *O);
extern int IsGFilterOutputStreamObj (Stack stack, int i);
extern int IsGFilterOutputStream(NspObject *O);
extern NspGFilterOutputStream *GetGFilterOutputStreamCopy (Stack stack, int i);
extern NspGFilterOutputStream *GetGFilterOutputStream (Stack stack, int i);

#endif /* NSP_INC_NspGFilterOutputStream */ 

#ifdef NspGFilterOutputStream_Private 
static int init_gfilteroutputstream(NspGFilterOutputStream *o,NspTypeGFilterOutputStream *type);
static char *nsp_gfilteroutputstream_type_as_string(void);
static char *nsp_gfilteroutputstream_type_short_string(NspObject *v);
static AttrTab gfilteroutputstream_attrs[];
static NspMethods *gfilteroutputstream_get_methods(void);
/* static int int_gfilteroutputstream_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGFilterOutputStream_Private */
