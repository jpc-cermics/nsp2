/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkCellRendererToggle
#define NSP_INC_NspGtkCellRendererToggle

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

/* NspGtkCellRendererToggle */

#include <nsp/gtk/gtkcellrenderer.h>

/*
 * NspGtkCellRendererToggle inherits from GtkCellRenderer
 * just change some type attributes 
 */

typedef NspGtkCellRenderer NspGtkCellRendererToggle ;
typedef NspTypeGtkCellRenderer NspTypeGtkCellRendererToggle ;

extern int nsp_type_gtkcellrenderertoggle_id;
extern NspTypeGtkCellRendererToggle *nsp_type_gtkcellrenderertoggle;

/* type instances for gtkcellrenderer */

NspTypeGtkCellRendererToggle *new_type_gtkcellrenderertoggle(type_mode mode);

/* instance for NspGtkCellRendererToggle */

NspGtkCellRendererToggle *new_gtkcellrenderertoggle();

/*
 * Object methods redefined for gtkcellrenderertoggle 
 */

#define NULLGTKCELLRENDERERTOGGLE (NspGtkCellRendererToggle*) 0


/* from NspGtkCellRendererToggleObj.c */

extern NspGtkCellRendererToggle *nsp_gtkcellrenderertoggle_object (NspObject *O);
extern int IsGtkCellRendererToggleObj (Stack stack, int i);
extern int IsGtkCellRendererToggle(NspObject *O);
extern NspGtkCellRendererToggle *GetGtkCellRendererToggleCopy (Stack stack, int i);
extern NspGtkCellRendererToggle *GetGtkCellRendererToggle (Stack stack, int i);

#endif /* NSP_INC_NspGtkCellRendererToggle */ 

#ifdef NspGtkCellRendererToggle_Private 
static int init_gtkcellrenderertoggle(NspGtkCellRendererToggle *o,NspTypeGtkCellRendererToggle *type);
static char *nsp_gtkcellrenderertoggle_type_as_string(void);
static char *nsp_gtkcellrenderertoggle_type_short_string(NspObject *v);
static AttrTab gtkcellrenderertoggle_attrs[];
static NspMethods *gtkcellrenderertoggle_get_methods(void);
/* static int int_gtkcellrenderertoggle_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkCellRendererToggle_Private */
