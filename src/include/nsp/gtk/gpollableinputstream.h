/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGPollableInputStream
#define NSP_INC_NspGPollableInputStream

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

/* NspGPollableInputStream */

#include <nsp/gtk/gobject.h>

/*
 * NspGPollableInputStream inherits from GObject
 * just change some type attributes 
 */

typedef NspGObject NspGPollableInputStream ;
typedef NspTypeGObject NspTypeGPollableInputStream ;

extern int nsp_type_gpollableinputstream_id;
extern NspTypeGPollableInputStream *nsp_type_gpollableinputstream;

/* type instances for gobject */

NspTypeGPollableInputStream *new_type_gpollableinputstream(type_mode mode);

/* instance for NspGPollableInputStream */

NspGPollableInputStream *new_gpollableinputstream();

/*
 * Object methods redefined for gpollableinputstream 
 */

#define NULLGPOLLABLEINPUTSTREAM (NspGPollableInputStream*) 0


/* from NspGPollableInputStreamObj.c */

extern NspGPollableInputStream *nsp_gpollableinputstream_object (NspObject *O);
extern int IsGPollableInputStreamObj (Stack stack, int i);
extern int IsGPollableInputStream(NspObject *O);
extern NspGPollableInputStream *GetGPollableInputStreamCopy (Stack stack, int i);
extern NspGPollableInputStream *GetGPollableInputStream (Stack stack, int i);

#endif /* NSP_INC_NspGPollableInputStream */ 

#ifdef NspGPollableInputStream_Private 
static int init_gpollableinputstream(NspGPollableInputStream *o,NspTypeGPollableInputStream *type);
static char *nsp_gpollableinputstream_type_as_string(void);
static char *nsp_gpollableinputstream_type_short_string(NspObject *v);
static AttrTab gpollableinputstream_attrs[];
static NspMethods *gpollableinputstream_get_methods(void);
/* static int int_gpollableinputstream_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGPollableInputStream_Private */
