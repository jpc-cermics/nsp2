/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGOutputStream
#define NSP_INC_NspGOutputStream

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

/* NspGOutputStream */

#include <nsp/gtk/gobject.h>

/*
 * NspGOutputStream inherits from GObject
 * just change some type attributes 
 */

typedef NspGObject NspGOutputStream ;
typedef NspTypeGObject NspTypeGOutputStream ;

extern int nsp_type_goutputstream_id;
extern NspTypeGOutputStream *nsp_type_goutputstream;

/* type instances for gobject */

NspTypeGOutputStream *new_type_goutputstream(type_mode mode);

/* instance for NspGOutputStream */

NspGOutputStream *new_goutputstream();

/*
 * Object methods redefined for goutputstream 
 */

#define NULLGOUTPUTSTREAM (NspGOutputStream*) 0


/* from NspGOutputStreamObj.c */

extern NspGOutputStream *nsp_goutputstream_object (NspObject *O);
extern int IsGOutputStreamObj (Stack stack, int i);
extern int IsGOutputStream(NspObject *O);
extern NspGOutputStream *GetGOutputStreamCopy (Stack stack, int i);
extern NspGOutputStream *GetGOutputStream (Stack stack, int i);

#endif /* NSP_INC_NspGOutputStream */ 

#ifdef NspGOutputStream_Private 
static int init_goutputstream(NspGOutputStream *o,NspTypeGOutputStream *type);
static char *nsp_goutputstream_type_as_string(void);
static char *nsp_goutputstream_type_short_string(NspObject *v);
static AttrTab goutputstream_attrs[];
static NspMethods *goutputstream_get_methods(void);
/* static int int_goutputstream_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGOutputStream_Private */
