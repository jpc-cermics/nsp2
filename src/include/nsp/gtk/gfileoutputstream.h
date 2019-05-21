/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGFileOutputStream
#define NSP_INC_NspGFileOutputStream

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

/* NspGFileOutputStream */

#include <nsp/gtk/goutputstream.h>

/*
 * NspGFileOutputStream inherits from GOutputStream
 * just change some type attributes 
 */

typedef NspGOutputStream NspGFileOutputStream ;
typedef NspTypeGOutputStream NspTypeGFileOutputStream ;

extern int nsp_type_gfileoutputstream_id;
extern NspTypeGFileOutputStream *nsp_type_gfileoutputstream;

/* type instances for goutputstream */

NspTypeGFileOutputStream *new_type_gfileoutputstream(type_mode mode);

/* instance for NspGFileOutputStream */

NspGFileOutputStream *new_gfileoutputstream();

/*
 * Object methods redefined for gfileoutputstream 
 */

#define NULLGFILEOUTPUTSTREAM (NspGFileOutputStream*) 0


/* from NspGFileOutputStreamObj.c */

extern NspGFileOutputStream *nsp_gfileoutputstream_object (NspObject *O);
extern int IsGFileOutputStreamObj (Stack stack, int i);
extern int IsGFileOutputStream(NspObject *O);
extern NspGFileOutputStream *GetGFileOutputStreamCopy (Stack stack, int i);
extern NspGFileOutputStream *GetGFileOutputStream (Stack stack, int i);

#endif /* NSP_INC_NspGFileOutputStream */ 

#ifdef NspGFileOutputStream_Private 
static int init_gfileoutputstream(NspGFileOutputStream *o,NspTypeGFileOutputStream *type);
static char *nsp_gfileoutputstream_type_as_string(void);
static char *nsp_gfileoutputstream_type_short_string(NspObject *v);
static AttrTab gfileoutputstream_attrs[];
static NspMethods *gfileoutputstream_get_methods(void);
/* static int int_gfileoutputstream_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGFileOutputStream_Private */
