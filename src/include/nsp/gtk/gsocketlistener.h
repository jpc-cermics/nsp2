/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGSocketListener
#define NSP_INC_NspGSocketListener

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

/* NspGSocketListener */

#include <nsp/gtk/gobject.h>

/*
 * NspGSocketListener inherits from GObject
 * just change some type attributes 
 */

typedef NspGObject NspGSocketListener ;
typedef NspTypeGObject NspTypeGSocketListener ;

extern int nsp_type_gsocketlistener_id;
extern NspTypeGSocketListener *nsp_type_gsocketlistener;

/* type instances for gobject */

NspTypeGSocketListener *new_type_gsocketlistener(type_mode mode);

/* instance for NspGSocketListener */

NspGSocketListener *new_gsocketlistener();

/*
 * Object methods redefined for gsocketlistener 
 */

#define NULLGSOCKETLISTENER (NspGSocketListener*) 0


/* from NspGSocketListenerObj.c */

extern NspGSocketListener *nsp_gsocketlistener_object (NspObject *O);
extern int IsGSocketListenerObj (Stack stack, int i);
extern int IsGSocketListener(NspObject *O);
extern NspGSocketListener *GetGSocketListenerCopy (Stack stack, int i);
extern NspGSocketListener *GetGSocketListener (Stack stack, int i);

#endif /* NSP_INC_NspGSocketListener */ 

#ifdef NspGSocketListener_Private 
static int init_gsocketlistener(NspGSocketListener *o,NspTypeGSocketListener *type);
static char *nsp_gsocketlistener_type_as_string(void);
static char *nsp_gsocketlistener_type_short_string(NspObject *v);
static AttrTab gsocketlistener_attrs[];
static NspMethods *gsocketlistener_get_methods(void);
/* static int int_gsocketlistener_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGSocketListener_Private */
