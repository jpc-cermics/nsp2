/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkCellView
#define NSP_INC_NspGtkCellView

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

/* NspGtkCellView */

#include <nsp/gtk/gtkwidget.h>

/*
 * NspGtkCellView inherits from GtkWidget
 * just change some type attributes 
 */

typedef NspGtkWidget NspGtkCellView ;
typedef NspTypeGtkWidget NspTypeGtkCellView ;

extern int nsp_type_gtkcellview_id;
extern NspTypeGtkCellView *nsp_type_gtkcellview;

/* type instances for gtkwidget */

NspTypeGtkCellView *new_type_gtkcellview(type_mode mode);

/* instance for NspGtkCellView */

NspGtkCellView *new_gtkcellview();

/*
 * Object methods redefined for gtkcellview 
 */

#define NULLGTKCELLVIEW (NspGtkCellView*) 0


/* from NspGtkCellViewObj.c */

extern NspGtkCellView *nsp_gtkcellview_object (NspObject *O);
extern int IsGtkCellViewObj (Stack stack, int i);
extern int IsGtkCellView(NspObject *O);
extern NspGtkCellView *GetGtkCellViewCopy (Stack stack, int i);
extern NspGtkCellView *GetGtkCellView (Stack stack, int i);

#endif /* NSP_INC_NspGtkCellView */ 

#ifdef NspGtkCellView_Private 
static int init_gtkcellview(NspGtkCellView *o,NspTypeGtkCellView *type);
static char *nsp_gtkcellview_type_as_string(void);
static char *nsp_gtkcellview_type_short_string(NspObject *v);
static AttrTab gtkcellview_attrs[];
static NspMethods *gtkcellview_get_methods(void);
/* static int int_gtkcellview_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkCellView_Private */
