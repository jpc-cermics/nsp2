/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGFileIOStream
#define NSP_INC_NspGFileIOStream

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

/* NspGFileIOStream */

#include <nsp/gtk/giostream.h>

/*
 * NspGFileIOStream inherits from GIOStream
 * just change some type attributes 
 */

typedef NspGIOStream NspGFileIOStream ;
typedef NspTypeGIOStream NspTypeGFileIOStream ;

extern int nsp_type_gfileiostream_id;
extern NspTypeGFileIOStream *nsp_type_gfileiostream;

/* type instances for giostream */

NspTypeGFileIOStream *new_type_gfileiostream(type_mode mode);

/* instance for NspGFileIOStream */

NspGFileIOStream *new_gfileiostream();

/*
 * Object methods redefined for gfileiostream 
 */

#define NULLGFILEIOSTREAM (NspGFileIOStream*) 0


/* from NspGFileIOStreamObj.c */

extern NspGFileIOStream *nsp_gfileiostream_object (NspObject *O);
extern int IsGFileIOStreamObj (Stack stack, int i);
extern int IsGFileIOStream(NspObject *O);
extern NspGFileIOStream *GetGFileIOStreamCopy (Stack stack, int i);
extern NspGFileIOStream *GetGFileIOStream (Stack stack, int i);

#endif /* NSP_INC_NspGFileIOStream */ 

#ifdef NspGFileIOStream_Private 
static int init_gfileiostream(NspGFileIOStream *o,NspTypeGFileIOStream *type);
static char *nsp_gfileiostream_type_as_string(void);
static char *nsp_gfileiostream_type_short_string(NspObject *v);
static AttrTab gfileiostream_attrs[];
static NspMethods *gfileiostream_get_methods(void);
/* static int int_gfileiostream_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGFileIOStream_Private */
