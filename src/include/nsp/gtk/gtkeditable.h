/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkEditable
#define NSP_INC_NspGtkEditable

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

/* NspGtkEditable */

#include <nsp/gtk/gobject.h>

/*
 * NspGtkEditable inherits from GObject
 * just change some type attributes 
 */

typedef NspGObject NspGtkEditable ;
typedef NspTypeGObject NspTypeGtkEditable ;

extern int nsp_type_gtkeditable_id;
extern NspTypeGtkEditable *nsp_type_gtkeditable;

/* type instances for gobject */

NspTypeGtkEditable *new_type_gtkeditable(type_mode mode);

/* instance for NspGtkEditable */

NspGtkEditable *new_gtkeditable();

/*
 * Object methods redefined for gtkeditable 
 */

#define NULLGTKEDITABLE (NspGtkEditable*) 0


/* from NspGtkEditableObj.c */

extern NspGtkEditable *nsp_gtkeditable_object (NspObject *O);
extern int IsGtkEditableObj (Stack stack, int i);
extern int IsGtkEditable(NspObject *O);
extern NspGtkEditable *GetGtkEditableCopy (Stack stack, int i);
extern NspGtkEditable *GetGtkEditable (Stack stack, int i);

#endif /* NSP_INC_NspGtkEditable */ 

#ifdef NspGtkEditable_Private 
static int init_gtkeditable(NspGtkEditable *o,NspTypeGtkEditable *type);
static char *nsp_gtkeditable_type_as_string(void);
static char *nsp_gtkeditable_type_short_string(NspObject *v);
static AttrTab gtkeditable_attrs[];
static NspMethods *gtkeditable_get_methods(void);
/* static int int_gtkeditable_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkEditable_Private */
