/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGPollableOutputStream
#define NSP_INC_NspGPollableOutputStream

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

/* NspGPollableOutputStream */

#include <nsp/gtk/gobject.h>

/*
 * NspGPollableOutputStream inherits from GObject
 * just change some type attributes 
 */

typedef NspGObject NspGPollableOutputStream ;
typedef NspTypeGObject NspTypeGPollableOutputStream ;

extern int nsp_type_gpollableoutputstream_id;
extern NspTypeGPollableOutputStream *nsp_type_gpollableoutputstream;

/* type instances for gobject */

NspTypeGPollableOutputStream *new_type_gpollableoutputstream(type_mode mode);

/* instance for NspGPollableOutputStream */

NspGPollableOutputStream *new_gpollableoutputstream();

/*
 * Object methods redefined for gpollableoutputstream 
 */

#define NULLGPOLLABLEOUTPUTSTREAM (NspGPollableOutputStream*) 0


/* from NspGPollableOutputStreamObj.c */

extern NspGPollableOutputStream *nsp_gpollableoutputstream_object (NspObject *O);
extern int IsGPollableOutputStreamObj (Stack stack, int i);
extern int IsGPollableOutputStream(NspObject *O);
extern NspGPollableOutputStream *GetGPollableOutputStreamCopy (Stack stack, int i);
extern NspGPollableOutputStream *GetGPollableOutputStream (Stack stack, int i);

#endif /* NSP_INC_NspGPollableOutputStream */ 

#ifdef NspGPollableOutputStream_Private 
static int init_gpollableoutputstream(NspGPollableOutputStream *o,NspTypeGPollableOutputStream *type);
static char *nsp_gpollableoutputstream_type_as_string(void);
static char *nsp_gpollableoutputstream_type_short_string(NspObject *v);
static AttrTab gpollableoutputstream_attrs[];
static NspMethods *gpollableoutputstream_get_methods(void);
/* static int int_gpollableoutputstream_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGPollableOutputStream_Private */
