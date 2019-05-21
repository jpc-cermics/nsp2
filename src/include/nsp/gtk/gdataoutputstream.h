/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGDataOutputStream
#define NSP_INC_NspGDataOutputStream

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

/* NspGDataOutputStream */

#include <nsp/gtk/gfilteroutputstream.h>

/*
 * NspGDataOutputStream inherits from GFilterOutputStream
 * just change some type attributes 
 */

typedef NspGFilterOutputStream NspGDataOutputStream ;
typedef NspTypeGFilterOutputStream NspTypeGDataOutputStream ;

extern int nsp_type_gdataoutputstream_id;
extern NspTypeGDataOutputStream *nsp_type_gdataoutputstream;

/* type instances for gfilteroutputstream */

NspTypeGDataOutputStream *new_type_gdataoutputstream(type_mode mode);

/* instance for NspGDataOutputStream */

NspGDataOutputStream *new_gdataoutputstream();

/*
 * Object methods redefined for gdataoutputstream 
 */

#define NULLGDATAOUTPUTSTREAM (NspGDataOutputStream*) 0


/* from NspGDataOutputStreamObj.c */

extern NspGDataOutputStream *nsp_gdataoutputstream_object (NspObject *O);
extern int IsGDataOutputStreamObj (Stack stack, int i);
extern int IsGDataOutputStream(NspObject *O);
extern NspGDataOutputStream *GetGDataOutputStreamCopy (Stack stack, int i);
extern NspGDataOutputStream *GetGDataOutputStream (Stack stack, int i);

#endif /* NSP_INC_NspGDataOutputStream */ 

#ifdef NspGDataOutputStream_Private 
static int init_gdataoutputstream(NspGDataOutputStream *o,NspTypeGDataOutputStream *type);
static char *nsp_gdataoutputstream_type_as_string(void);
static char *nsp_gdataoutputstream_type_short_string(NspObject *v);
static AttrTab gdataoutputstream_attrs[];
static NspMethods *gdataoutputstream_get_methods(void);
/* static int int_gdataoutputstream_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGDataOutputStream_Private */
