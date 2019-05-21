/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGSocket
#define NSP_INC_NspGSocket

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

/* NspGSocket */

#include <nsp/gtk/gobject.h>

/*
 * NspGSocket inherits from GObject
 * just change some type attributes 
 */

typedef NspGObject NspGSocket ;
typedef NspTypeGObject NspTypeGSocket ;

extern int nsp_type_gsocket_id;
extern NspTypeGSocket *nsp_type_gsocket;

/* type instances for gobject */

NspTypeGSocket *new_type_gsocket(type_mode mode);

/* instance for NspGSocket */

NspGSocket *new_gsocket();

/*
 * Object methods redefined for gsocket 
 */

#define NULLGSOCKET (NspGSocket*) 0


/* from NspGSocketObj.c */

extern NspGSocket *nsp_gsocket_object (NspObject *O);
extern int IsGSocketObj (Stack stack, int i);
extern int IsGSocket(NspObject *O);
extern NspGSocket *GetGSocketCopy (Stack stack, int i);
extern NspGSocket *GetGSocket (Stack stack, int i);

#endif /* NSP_INC_NspGSocket */ 

#ifdef NspGSocket_Private 
static int init_gsocket(NspGSocket *o,NspTypeGSocket *type);
static char *nsp_gsocket_type_as_string(void);
static char *nsp_gsocket_type_short_string(NspObject *v);
static AttrTab gsocket_attrs[];
static NspMethods *gsocket_get_methods(void);
/* static int int_gsocket_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGSocket_Private */
