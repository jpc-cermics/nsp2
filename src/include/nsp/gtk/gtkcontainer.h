/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkContainer
#define NSP_INC_NspGtkContainer

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

/* NspGtkContainer */

#include <nsp/gtk/gtkwidget.h>

/*
 * NspGtkContainer inherits from GtkWidget
 * just change some type attributes 
 */

typedef NspGtkWidget NspGtkContainer ;
typedef NspTypeGtkWidget NspTypeGtkContainer ;

extern int nsp_type_gtkcontainer_id;
extern NspTypeGtkContainer *nsp_type_gtkcontainer;

/* type instances for gtkwidget */

NspTypeGtkContainer *new_type_gtkcontainer(type_mode mode);

/* instance for NspGtkContainer */

NspGtkContainer *new_gtkcontainer();

/*
 * Object methods redefined for gtkcontainer 
 */

#define NULLGTKCONTAINER (NspGtkContainer*) 0


/* from NspGtkContainerObj.c */

extern NspGtkContainer *nsp_gtkcontainer_object (NspObject *O);
extern int IsGtkContainerObj (Stack stack, int i);
extern int IsGtkContainer(NspObject *O);
extern NspGtkContainer *GetGtkContainerCopy (Stack stack, int i);
extern NspGtkContainer *GetGtkContainer (Stack stack, int i);

#endif /* NSP_INC_NspGtkContainer */ 

#ifdef NspGtkContainer_Private 
static int init_gtkcontainer(NspGtkContainer *o,NspTypeGtkContainer *type);
static char *nsp_gtkcontainer_type_as_string(void);
static char *nsp_gtkcontainer_type_short_string(NspObject *v);
static AttrTab gtkcontainer_attrs[];
static NspMethods *gtkcontainer_get_methods(void);
/* static int int_gtkcontainer_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkContainer_Private */
