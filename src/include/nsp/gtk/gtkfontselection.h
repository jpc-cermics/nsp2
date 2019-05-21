/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkFontSelection
#define NSP_INC_NspGtkFontSelection

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

/* NspGtkFontSelection */

#include <nsp/gtk/gtkvbox.h>

/*
 * NspGtkFontSelection inherits from GtkVBox
 * just change some type attributes 
 */

typedef NspGtkVBox NspGtkFontSelection ;
typedef NspTypeGtkVBox NspTypeGtkFontSelection ;

extern int nsp_type_gtkfontselection_id;
extern NspTypeGtkFontSelection *nsp_type_gtkfontselection;

/* type instances for gtkvbox */

NspTypeGtkFontSelection *new_type_gtkfontselection(type_mode mode);

/* instance for NspGtkFontSelection */

NspGtkFontSelection *new_gtkfontselection();

/*
 * Object methods redefined for gtkfontselection 
 */

#define NULLGTKFONTSELECTION (NspGtkFontSelection*) 0


/* from NspGtkFontSelectionObj.c */

extern NspGtkFontSelection *nsp_gtkfontselection_object (NspObject *O);
extern int IsGtkFontSelectionObj (Stack stack, int i);
extern int IsGtkFontSelection(NspObject *O);
extern NspGtkFontSelection *GetGtkFontSelectionCopy (Stack stack, int i);
extern NspGtkFontSelection *GetGtkFontSelection (Stack stack, int i);

#endif /* NSP_INC_NspGtkFontSelection */ 

#ifdef NspGtkFontSelection_Private 
static int init_gtkfontselection(NspGtkFontSelection *o,NspTypeGtkFontSelection *type);
static char *nsp_gtkfontselection_type_as_string(void);
static char *nsp_gtkfontselection_type_short_string(NspObject *v);
static AttrTab gtkfontselection_attrs[];
static NspMethods *gtkfontselection_get_methods(void);
/* static int int_gtkfontselection_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkFontSelection_Private */
