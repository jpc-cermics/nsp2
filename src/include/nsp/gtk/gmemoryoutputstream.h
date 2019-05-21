/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGMemoryOutputStream
#define NSP_INC_NspGMemoryOutputStream

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

/* NspGMemoryOutputStream */

#include <nsp/gtk/goutputstream.h>

/*
 * NspGMemoryOutputStream inherits from GOutputStream
 * just change some type attributes 
 */

typedef NspGOutputStream NspGMemoryOutputStream ;
typedef NspTypeGOutputStream NspTypeGMemoryOutputStream ;

extern int nsp_type_gmemoryoutputstream_id;
extern NspTypeGMemoryOutputStream *nsp_type_gmemoryoutputstream;

/* type instances for goutputstream */

NspTypeGMemoryOutputStream *new_type_gmemoryoutputstream(type_mode mode);

/* instance for NspGMemoryOutputStream */

NspGMemoryOutputStream *new_gmemoryoutputstream();

/*
 * Object methods redefined for gmemoryoutputstream 
 */

#define NULLGMEMORYOUTPUTSTREAM (NspGMemoryOutputStream*) 0


/* from NspGMemoryOutputStreamObj.c */

extern NspGMemoryOutputStream *nsp_gmemoryoutputstream_object (NspObject *O);
extern int IsGMemoryOutputStreamObj (Stack stack, int i);
extern int IsGMemoryOutputStream(NspObject *O);
extern NspGMemoryOutputStream *GetGMemoryOutputStreamCopy (Stack stack, int i);
extern NspGMemoryOutputStream *GetGMemoryOutputStream (Stack stack, int i);

#endif /* NSP_INC_NspGMemoryOutputStream */ 

#ifdef NspGMemoryOutputStream_Private 
static int init_gmemoryoutputstream(NspGMemoryOutputStream *o,NspTypeGMemoryOutputStream *type);
static char *nsp_gmemoryoutputstream_type_as_string(void);
static char *nsp_gmemoryoutputstream_type_short_string(NspObject *v);
static AttrTab gmemoryoutputstream_attrs[];
static NspMethods *gmemoryoutputstream_get_methods(void);
/* static int int_gmemoryoutputstream_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGMemoryOutputStream_Private */
