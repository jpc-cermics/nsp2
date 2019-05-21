/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkPaned
#define NSP_INC_NspGtkPaned

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

/* NspGtkPaned */

#include <nsp/gtk/gtkcontainer.h>

/*
 * NspGtkPaned inherits from GtkContainer
 * just change some type attributes 
 */

typedef NspGtkContainer NspGtkPaned ;
typedef NspTypeGtkContainer NspTypeGtkPaned ;

extern int nsp_type_gtkpaned_id;
extern NspTypeGtkPaned *nsp_type_gtkpaned;

/* type instances for gtkcontainer */

NspTypeGtkPaned *new_type_gtkpaned(type_mode mode);

/* instance for NspGtkPaned */

NspGtkPaned *new_gtkpaned();

/*
 * Object methods redefined for gtkpaned 
 */

#define NULLGTKPANED (NspGtkPaned*) 0


/* from NspGtkPanedObj.c */

extern NspGtkPaned *nsp_gtkpaned_object (NspObject *O);
extern int IsGtkPanedObj (Stack stack, int i);
extern int IsGtkPaned(NspObject *O);
extern NspGtkPaned *GetGtkPanedCopy (Stack stack, int i);
extern NspGtkPaned *GetGtkPaned (Stack stack, int i);

#endif /* NSP_INC_NspGtkPaned */ 

#ifdef NspGtkPaned_Private 
static int init_gtkpaned(NspGtkPaned *o,NspTypeGtkPaned *type);
static char *nsp_gtkpaned_type_as_string(void);
static char *nsp_gtkpaned_type_short_string(NspObject *v);
static AttrTab gtkpaned_attrs[];
static NspMethods *gtkpaned_get_methods(void);
/* static int int_gtkpaned_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkPaned_Private */
