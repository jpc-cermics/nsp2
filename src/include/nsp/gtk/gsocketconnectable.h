/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGSocketConnectable
#define NSP_INC_NspGSocketConnectable

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

/* NspGSocketConnectable */

#include <nsp/gtk/gobject.h>

/*
 * NspGSocketConnectable inherits from GObject
 * just change some type attributes 
 */

typedef NspGObject NspGSocketConnectable ;
typedef NspTypeGObject NspTypeGSocketConnectable ;

extern int nsp_type_gsocketconnectable_id;
extern NspTypeGSocketConnectable *nsp_type_gsocketconnectable;

/* type instances for gobject */

NspTypeGSocketConnectable *new_type_gsocketconnectable(type_mode mode);

/* instance for NspGSocketConnectable */

NspGSocketConnectable *new_gsocketconnectable();

/*
 * Object methods redefined for gsocketconnectable 
 */

#define NULLGSOCKETCONNECTABLE (NspGSocketConnectable*) 0


/* from NspGSocketConnectableObj.c */

extern NspGSocketConnectable *nsp_gsocketconnectable_object (NspObject *O);
extern int IsGSocketConnectableObj (Stack stack, int i);
extern int IsGSocketConnectable(NspObject *O);
extern NspGSocketConnectable *GetGSocketConnectableCopy (Stack stack, int i);
extern NspGSocketConnectable *GetGSocketConnectable (Stack stack, int i);

#endif /* NSP_INC_NspGSocketConnectable */ 

#ifdef NspGSocketConnectable_Private 
static int init_gsocketconnectable(NspGSocketConnectable *o,NspTypeGSocketConnectable *type);
static char *nsp_gsocketconnectable_type_as_string(void);
static char *nsp_gsocketconnectable_type_short_string(NspObject *v);
static AttrTab gsocketconnectable_attrs[];
static NspMethods *gsocketconnectable_get_methods(void);
/* static int int_gsocketconnectable_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGSocketConnectable_Private */
