/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkCellRenderer
#define NSP_INC_NspGtkCellRenderer

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

/* NspGtkCellRenderer */

#include <nsp/gtk/gobject.h>

/*
 * NspGtkCellRenderer inherits from GObject
 * just change some type attributes 
 */

typedef NspGObject NspGtkCellRenderer ;
typedef NspTypeGObject NspTypeGtkCellRenderer ;

extern int nsp_type_gtkcellrenderer_id;
extern NspTypeGtkCellRenderer *nsp_type_gtkcellrenderer;

/* type instances for gobject */

NspTypeGtkCellRenderer *new_type_gtkcellrenderer(type_mode mode);

/* instance for NspGtkCellRenderer */

NspGtkCellRenderer *new_gtkcellrenderer();

/*
 * Object methods redefined for gtkcellrenderer 
 */

#define NULLGTKCELLRENDERER (NspGtkCellRenderer*) 0


/* from NspGtkCellRendererObj.c */

extern NspGtkCellRenderer *nsp_gtkcellrenderer_object (NspObject *O);
extern int IsGtkCellRendererObj (Stack stack, int i);
extern int IsGtkCellRenderer(NspObject *O);
extern NspGtkCellRenderer *GetGtkCellRendererCopy (Stack stack, int i);
extern NspGtkCellRenderer *GetGtkCellRenderer (Stack stack, int i);

#endif /* NSP_INC_NspGtkCellRenderer */ 

#ifdef NspGtkCellRenderer_Private 
static int init_gtkcellrenderer(NspGtkCellRenderer *o,NspTypeGtkCellRenderer *type);
static char *nsp_gtkcellrenderer_type_as_string(void);
static char *nsp_gtkcellrenderer_type_short_string(NspObject *v);
static AttrTab gtkcellrenderer_attrs[];
static NspMethods *gtkcellrenderer_get_methods(void);
/* static int int_gtkcellrenderer_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkCellRenderer_Private */
