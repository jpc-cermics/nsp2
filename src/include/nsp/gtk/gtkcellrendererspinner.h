/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkCellRendererSpinner
#define NSP_INC_NspGtkCellRendererSpinner

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

/* NspGtkCellRendererSpinner */

#include <nsp/gtk/gtkcellrenderer.h>

/*
 * NspGtkCellRendererSpinner inherits from GtkCellRenderer
 * just change some type attributes 
 */

typedef NspGtkCellRenderer NspGtkCellRendererSpinner ;
typedef NspTypeGtkCellRenderer NspTypeGtkCellRendererSpinner ;

extern int nsp_type_gtkcellrendererspinner_id;
extern NspTypeGtkCellRendererSpinner *nsp_type_gtkcellrendererspinner;

/* type instances for gtkcellrenderer */

NspTypeGtkCellRendererSpinner *new_type_gtkcellrendererspinner(type_mode mode);

/* instance for NspGtkCellRendererSpinner */

NspGtkCellRendererSpinner *new_gtkcellrendererspinner();

/*
 * Object methods redefined for gtkcellrendererspinner 
 */

#define NULLGTKCELLRENDERERSPINNER (NspGtkCellRendererSpinner*) 0


/* from NspGtkCellRendererSpinnerObj.c */

extern NspGtkCellRendererSpinner *nsp_gtkcellrendererspinner_object (NspObject *O);
extern int IsGtkCellRendererSpinnerObj (Stack stack, int i);
extern int IsGtkCellRendererSpinner(NspObject *O);
extern NspGtkCellRendererSpinner *GetGtkCellRendererSpinnerCopy (Stack stack, int i);
extern NspGtkCellRendererSpinner *GetGtkCellRendererSpinner (Stack stack, int i);

#endif /* NSP_INC_NspGtkCellRendererSpinner */ 

#ifdef NspGtkCellRendererSpinner_Private 
static int init_gtkcellrendererspinner(NspGtkCellRendererSpinner *o,NspTypeGtkCellRendererSpinner *type);
static char *nsp_gtkcellrendererspinner_type_as_string(void);
static char *nsp_gtkcellrendererspinner_type_short_string(NspObject *v);
static AttrTab gtkcellrendererspinner_attrs[];
static NspMethods *gtkcellrendererspinner_get_methods(void);
/* static int int_gtkcellrendererspinner_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkCellRendererSpinner_Private */
