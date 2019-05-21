/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGBufferedOutputStream
#define NSP_INC_NspGBufferedOutputStream

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

/* NspGBufferedOutputStream */

#include <nsp/gtk/gfilteroutputstream.h>

/*
 * NspGBufferedOutputStream inherits from GFilterOutputStream
 * just change some type attributes 
 */

typedef NspGFilterOutputStream NspGBufferedOutputStream ;
typedef NspTypeGFilterOutputStream NspTypeGBufferedOutputStream ;

extern int nsp_type_gbufferedoutputstream_id;
extern NspTypeGBufferedOutputStream *nsp_type_gbufferedoutputstream;

/* type instances for gfilteroutputstream */

NspTypeGBufferedOutputStream *new_type_gbufferedoutputstream(type_mode mode);

/* instance for NspGBufferedOutputStream */

NspGBufferedOutputStream *new_gbufferedoutputstream();

/*
 * Object methods redefined for gbufferedoutputstream 
 */

#define NULLGBUFFEREDOUTPUTSTREAM (NspGBufferedOutputStream*) 0


/* from NspGBufferedOutputStreamObj.c */

extern NspGBufferedOutputStream *nsp_gbufferedoutputstream_object (NspObject *O);
extern int IsGBufferedOutputStreamObj (Stack stack, int i);
extern int IsGBufferedOutputStream(NspObject *O);
extern NspGBufferedOutputStream *GetGBufferedOutputStreamCopy (Stack stack, int i);
extern NspGBufferedOutputStream *GetGBufferedOutputStream (Stack stack, int i);

#endif /* NSP_INC_NspGBufferedOutputStream */ 

#ifdef NspGBufferedOutputStream_Private 
static int init_gbufferedoutputstream(NspGBufferedOutputStream *o,NspTypeGBufferedOutputStream *type);
static char *nsp_gbufferedoutputstream_type_as_string(void);
static char *nsp_gbufferedoutputstream_type_short_string(NspObject *v);
static AttrTab gbufferedoutputstream_attrs[];
static NspMethods *gbufferedoutputstream_get_methods(void);
/* static int int_gbufferedoutputstream_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGBufferedOutputStream_Private */
