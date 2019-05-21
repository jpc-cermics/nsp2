/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkColorSelection
#define NSP_INC_NspGtkColorSelection

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

/* NspGtkColorSelection */

#include <nsp/gtk/gtkvbox.h>

/*
 * NspGtkColorSelection inherits from GtkVBox
 * just change some type attributes 
 */

typedef NspGtkVBox NspGtkColorSelection ;
typedef NspTypeGtkVBox NspTypeGtkColorSelection ;

extern int nsp_type_gtkcolorselection_id;
extern NspTypeGtkColorSelection *nsp_type_gtkcolorselection;

/* type instances for gtkvbox */

NspTypeGtkColorSelection *new_type_gtkcolorselection(type_mode mode);

/* instance for NspGtkColorSelection */

NspGtkColorSelection *new_gtkcolorselection();

/*
 * Object methods redefined for gtkcolorselection 
 */

#define NULLGTKCOLORSELECTION (NspGtkColorSelection*) 0


/* from NspGtkColorSelectionObj.c */

extern NspGtkColorSelection *nsp_gtkcolorselection_object (NspObject *O);
extern int IsGtkColorSelectionObj (Stack stack, int i);
extern int IsGtkColorSelection(NspObject *O);
extern NspGtkColorSelection *GetGtkColorSelectionCopy (Stack stack, int i);
extern NspGtkColorSelection *GetGtkColorSelection (Stack stack, int i);

#endif /* NSP_INC_NspGtkColorSelection */ 

#ifdef NspGtkColorSelection_Private 
static int init_gtkcolorselection(NspGtkColorSelection *o,NspTypeGtkColorSelection *type);
static char *nsp_gtkcolorselection_type_as_string(void);
static char *nsp_gtkcolorselection_type_short_string(NspObject *v);
static AttrTab gtkcolorselection_attrs[];
static NspMethods *gtkcolorselection_get_methods(void);
/* static int int_gtkcolorselection_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkColorSelection_Private */
