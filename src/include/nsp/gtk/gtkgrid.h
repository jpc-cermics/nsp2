/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkGrid
#define NSP_INC_NspGtkGrid

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

/* NspGtkGrid */

#include <nsp/gtk/gtkcontainer.h>

/*
 * NspGtkGrid inherits from GtkContainer
 * just change some type attributes 
 */

typedef NspGtkContainer NspGtkGrid ;
typedef NspTypeGtkContainer NspTypeGtkGrid ;

extern int nsp_type_gtkgrid_id;
extern NspTypeGtkGrid *nsp_type_gtkgrid;

/* type instances for gtkcontainer */

NspTypeGtkGrid *new_type_gtkgrid(type_mode mode);

/* instance for NspGtkGrid */

NspGtkGrid *new_gtkgrid();

/*
 * Object methods redefined for gtkgrid 
 */

#define NULLGTKGRID (NspGtkGrid*) 0


/* from NspGtkGridObj.c */

extern NspGtkGrid *nsp_gtkgrid_object (NspObject *O);
extern int IsGtkGridObj (Stack stack, int i);
extern int IsGtkGrid(NspObject *O);
extern NspGtkGrid *GetGtkGridCopy (Stack stack, int i);
extern NspGtkGrid *GetGtkGrid (Stack stack, int i);

#endif /* NSP_INC_NspGtkGrid */ 

#ifdef NspGtkGrid_Private 
static int init_gtkgrid(NspGtkGrid *o,NspTypeGtkGrid *type);
static char *nsp_gtkgrid_type_as_string(void);
static char *nsp_gtkgrid_type_short_string(NspObject *v);
static AttrTab gtkgrid_attrs[];
static NspMethods *gtkgrid_get_methods(void);
/* static int int_gtkgrid_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkGrid_Private */
