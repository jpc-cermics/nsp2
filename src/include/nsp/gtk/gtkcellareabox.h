/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkCellAreaBox
#define NSP_INC_NspGtkCellAreaBox

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

/* NspGtkCellAreaBox */

#include <nsp/gtk/gtkcellarea.h>

/*
 * NspGtkCellAreaBox inherits from GtkCellArea
 * just change some type attributes 
 */

typedef NspGtkCellArea NspGtkCellAreaBox ;
typedef NspTypeGtkCellArea NspTypeGtkCellAreaBox ;

extern int nsp_type_gtkcellareabox_id;
extern NspTypeGtkCellAreaBox *nsp_type_gtkcellareabox;

/* type instances for gtkcellarea */

NspTypeGtkCellAreaBox *new_type_gtkcellareabox(type_mode mode);

/* instance for NspGtkCellAreaBox */

NspGtkCellAreaBox *new_gtkcellareabox();

/*
 * Object methods redefined for gtkcellareabox 
 */

#define NULLGTKCELLAREABOX (NspGtkCellAreaBox*) 0


/* from NspGtkCellAreaBoxObj.c */

extern NspGtkCellAreaBox *nsp_gtkcellareabox_object (NspObject *O);
extern int IsGtkCellAreaBoxObj (Stack stack, int i);
extern int IsGtkCellAreaBox(NspObject *O);
extern NspGtkCellAreaBox *GetGtkCellAreaBoxCopy (Stack stack, int i);
extern NspGtkCellAreaBox *GetGtkCellAreaBox (Stack stack, int i);

#endif /* NSP_INC_NspGtkCellAreaBox */ 

#ifdef NspGtkCellAreaBox_Private 
static int init_gtkcellareabox(NspGtkCellAreaBox *o,NspTypeGtkCellAreaBox *type);
static char *nsp_gtkcellareabox_type_as_string(void);
static char *nsp_gtkcellareabox_type_short_string(NspObject *v);
static AttrTab gtkcellareabox_attrs[];
static NspMethods *gtkcellareabox_get_methods(void);
/* static int int_gtkcellareabox_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkCellAreaBox_Private */
