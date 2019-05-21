/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkFixed
#define NSP_INC_NspGtkFixed

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

/* NspGtkFixed */

#include <nsp/gtk/gtkcontainer.h>

/*
 * NspGtkFixed inherits from GtkContainer
 * just change some type attributes 
 */

typedef NspGtkContainer NspGtkFixed ;
typedef NspTypeGtkContainer NspTypeGtkFixed ;

extern int nsp_type_gtkfixed_id;
extern NspTypeGtkFixed *nsp_type_gtkfixed;

/* type instances for gtkcontainer */

NspTypeGtkFixed *new_type_gtkfixed(type_mode mode);

/* instance for NspGtkFixed */

NspGtkFixed *new_gtkfixed();

/*
 * Object methods redefined for gtkfixed 
 */

#define NULLGTKFIXED (NspGtkFixed*) 0


/* from NspGtkFixedObj.c */

extern NspGtkFixed *nsp_gtkfixed_object (NspObject *O);
extern int IsGtkFixedObj (Stack stack, int i);
extern int IsGtkFixed(NspObject *O);
extern NspGtkFixed *GetGtkFixedCopy (Stack stack, int i);
extern NspGtkFixed *GetGtkFixed (Stack stack, int i);

#endif /* NSP_INC_NspGtkFixed */ 

#ifdef NspGtkFixed_Private 
static int init_gtkfixed(NspGtkFixed *o,NspTypeGtkFixed *type);
static char *nsp_gtkfixed_type_as_string(void);
static char *nsp_gtkfixed_type_short_string(NspObject *v);
static AttrTab gtkfixed_attrs[];
static NspMethods *gtkfixed_get_methods(void);
/* static int int_gtkfixed_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkFixed_Private */
