/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkSocket
#define NSP_INC_NspGtkSocket

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

/* NspGtkSocket */

#include <nsp/gtk/gtkcontainer.h>

/*
 * NspGtkSocket inherits from GtkContainer
 * just change some type attributes 
 */

typedef NspGtkContainer NspGtkSocket ;
typedef NspTypeGtkContainer NspTypeGtkSocket ;

extern int nsp_type_gtksocket_id;
extern NspTypeGtkSocket *nsp_type_gtksocket;

/* type instances for gtkcontainer */

NspTypeGtkSocket *new_type_gtksocket(type_mode mode);

/* instance for NspGtkSocket */

NspGtkSocket *new_gtksocket();

/*
 * Object methods redefined for gtksocket 
 */

#define NULLGTKSOCKET (NspGtkSocket*) 0


/* from NspGtkSocketObj.c */

extern NspGtkSocket *nsp_gtksocket_object (NspObject *O);
extern int IsGtkSocketObj (Stack stack, int i);
extern int IsGtkSocket(NspObject *O);
extern NspGtkSocket *GetGtkSocketCopy (Stack stack, int i);
extern NspGtkSocket *GetGtkSocket (Stack stack, int i);

#endif /* NSP_INC_NspGtkSocket */ 

#ifdef NspGtkSocket_Private 
static int init_gtksocket(NspGtkSocket *o,NspTypeGtkSocket *type);
static char *nsp_gtksocket_type_as_string(void);
static char *nsp_gtksocket_type_short_string(NspObject *v);
static AttrTab gtksocket_attrs[];
static NspMethods *gtksocket_get_methods(void);
/* static int int_gtksocket_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkSocket_Private */
