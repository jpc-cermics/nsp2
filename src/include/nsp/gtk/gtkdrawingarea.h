/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkDrawingArea
#define NSP_INC_NspGtkDrawingArea

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

/* NspGtkDrawingArea */

#include <nsp/gtk/gtkwidget.h>

/*
 * NspGtkDrawingArea inherits from GtkWidget
 * just change some type attributes 
 */

typedef NspGtkWidget NspGtkDrawingArea ;
typedef NspTypeGtkWidget NspTypeGtkDrawingArea ;

extern int nsp_type_gtkdrawingarea_id;
extern NspTypeGtkDrawingArea *nsp_type_gtkdrawingarea;

/* type instances for gtkwidget */

NspTypeGtkDrawingArea *new_type_gtkdrawingarea(type_mode mode);

/* instance for NspGtkDrawingArea */

NspGtkDrawingArea *new_gtkdrawingarea();

/*
 * Object methods redefined for gtkdrawingarea 
 */

#define NULLGTKDRAWINGAREA (NspGtkDrawingArea*) 0


/* from NspGtkDrawingAreaObj.c */

extern NspGtkDrawingArea *nsp_gtkdrawingarea_object (NspObject *O);
extern int IsGtkDrawingAreaObj (Stack stack, int i);
extern int IsGtkDrawingArea(NspObject *O);
extern NspGtkDrawingArea *GetGtkDrawingAreaCopy (Stack stack, int i);
extern NspGtkDrawingArea *GetGtkDrawingArea (Stack stack, int i);

#endif /* NSP_INC_NspGtkDrawingArea */ 

#ifdef NspGtkDrawingArea_Private 
static int init_gtkdrawingarea(NspGtkDrawingArea *o,NspTypeGtkDrawingArea *type);
static char *nsp_gtkdrawingarea_type_as_string(void);
static char *nsp_gtkdrawingarea_type_short_string(NspObject *v);
static AttrTab gtkdrawingarea_attrs[];
static NspMethods *gtkdrawingarea_get_methods(void);
/* static int int_gtkdrawingarea_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkDrawingArea_Private */
