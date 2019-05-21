/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGConverterOutputStream
#define NSP_INC_NspGConverterOutputStream

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

/* NspGConverterOutputStream */

#include <nsp/gtk/gfilteroutputstream.h>

/*
 * NspGConverterOutputStream inherits from GFilterOutputStream
 * just change some type attributes 
 */

typedef NspGFilterOutputStream NspGConverterOutputStream ;
typedef NspTypeGFilterOutputStream NspTypeGConverterOutputStream ;

extern int nsp_type_gconverteroutputstream_id;
extern NspTypeGConverterOutputStream *nsp_type_gconverteroutputstream;

/* type instances for gfilteroutputstream */

NspTypeGConverterOutputStream *new_type_gconverteroutputstream(type_mode mode);

/* instance for NspGConverterOutputStream */

NspGConverterOutputStream *new_gconverteroutputstream();

/*
 * Object methods redefined for gconverteroutputstream 
 */

#define NULLGCONVERTEROUTPUTSTREAM (NspGConverterOutputStream*) 0


/* from NspGConverterOutputStreamObj.c */

extern NspGConverterOutputStream *nsp_gconverteroutputstream_object (NspObject *O);
extern int IsGConverterOutputStreamObj (Stack stack, int i);
extern int IsGConverterOutputStream(NspObject *O);
extern NspGConverterOutputStream *GetGConverterOutputStreamCopy (Stack stack, int i);
extern NspGConverterOutputStream *GetGConverterOutputStream (Stack stack, int i);

#endif /* NSP_INC_NspGConverterOutputStream */ 

#ifdef NspGConverterOutputStream_Private 
static int init_gconverteroutputstream(NspGConverterOutputStream *o,NspTypeGConverterOutputStream *type);
static char *nsp_gconverteroutputstream_type_as_string(void);
static char *nsp_gconverteroutputstream_type_short_string(NspObject *v);
static AttrTab gconverteroutputstream_attrs[];
static NspMethods *gconverteroutputstream_get_methods(void);
/* static int int_gconverteroutputstream_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGConverterOutputStream_Private */
