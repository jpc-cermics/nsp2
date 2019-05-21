/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGIOStream
#define NSP_INC_NspGIOStream

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

/* NspGIOStream */

#include <nsp/gtk/gobject.h>

/*
 * NspGIOStream inherits from GObject
 * just change some type attributes 
 */

typedef NspGObject NspGIOStream ;
typedef NspTypeGObject NspTypeGIOStream ;

extern int nsp_type_giostream_id;
extern NspTypeGIOStream *nsp_type_giostream;

/* type instances for gobject */

NspTypeGIOStream *new_type_giostream(type_mode mode);

/* instance for NspGIOStream */

NspGIOStream *new_giostream();

/*
 * Object methods redefined for giostream 
 */

#define NULLGIOSTREAM (NspGIOStream*) 0


/* from NspGIOStreamObj.c */

extern NspGIOStream *nsp_giostream_object (NspObject *O);
extern int IsGIOStreamObj (Stack stack, int i);
extern int IsGIOStream(NspObject *O);
extern NspGIOStream *GetGIOStreamCopy (Stack stack, int i);
extern NspGIOStream *GetGIOStream (Stack stack, int i);

#endif /* NSP_INC_NspGIOStream */ 

#ifdef NspGIOStream_Private 
static int init_giostream(NspGIOStream *o,NspTypeGIOStream *type);
static char *nsp_giostream_type_as_string(void);
static char *nsp_giostream_type_short_string(NspObject *v);
static AttrTab giostream_attrs[];
static NspMethods *giostream_get_methods(void);
/* static int int_giostream_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGIOStream_Private */
