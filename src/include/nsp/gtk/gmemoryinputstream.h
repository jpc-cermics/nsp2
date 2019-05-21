/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGMemoryInputStream
#define NSP_INC_NspGMemoryInputStream

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

/* NspGMemoryInputStream */

#include <nsp/gtk/ginputstream.h>

/*
 * NspGMemoryInputStream inherits from GInputStream
 * just change some type attributes 
 */

typedef NspGInputStream NspGMemoryInputStream ;
typedef NspTypeGInputStream NspTypeGMemoryInputStream ;

extern int nsp_type_gmemoryinputstream_id;
extern NspTypeGMemoryInputStream *nsp_type_gmemoryinputstream;

/* type instances for ginputstream */

NspTypeGMemoryInputStream *new_type_gmemoryinputstream(type_mode mode);

/* instance for NspGMemoryInputStream */

NspGMemoryInputStream *new_gmemoryinputstream();

/*
 * Object methods redefined for gmemoryinputstream 
 */

#define NULLGMEMORYINPUTSTREAM (NspGMemoryInputStream*) 0


/* from NspGMemoryInputStreamObj.c */

extern NspGMemoryInputStream *nsp_gmemoryinputstream_object (NspObject *O);
extern int IsGMemoryInputStreamObj (Stack stack, int i);
extern int IsGMemoryInputStream(NspObject *O);
extern NspGMemoryInputStream *GetGMemoryInputStreamCopy (Stack stack, int i);
extern NspGMemoryInputStream *GetGMemoryInputStream (Stack stack, int i);

#endif /* NSP_INC_NspGMemoryInputStream */ 

#ifdef NspGMemoryInputStream_Private 
static int init_gmemoryinputstream(NspGMemoryInputStream *o,NspTypeGMemoryInputStream *type);
static char *nsp_gmemoryinputstream_type_as_string(void);
static char *nsp_gmemoryinputstream_type_short_string(NspObject *v);
static AttrTab gmemoryinputstream_attrs[];
static NspMethods *gmemoryinputstream_get_methods(void);
/* static int int_gmemoryinputstream_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGMemoryInputStream_Private */
