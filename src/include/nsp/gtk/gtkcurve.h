/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkCurve
#define NSP_INC_NspGtkCurve

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

/* NspGtkCurve */

#include <nsp/gtk/gtkdrawingarea.h>

/*
 * NspGtkCurve inherits from GtkDrawingArea
 * just change some type attributes 
 */

typedef NspGtkDrawingArea NspGtkCurve ;
typedef NspTypeGtkDrawingArea NspTypeGtkCurve ;

extern int nsp_type_gtkcurve_id;
extern NspTypeGtkCurve *nsp_type_gtkcurve;

/* type instances for gtkdrawingarea */

NspTypeGtkCurve *new_type_gtkcurve(type_mode mode);

/* instance for NspGtkCurve */

NspGtkCurve *new_gtkcurve();

/*
 * Object methods redefined for gtkcurve 
 */

#define NULLGTKCURVE (NspGtkCurve*) 0


/* from NspGtkCurveObj.c */

extern NspGtkCurve *nsp_gtkcurve_object (NspObject *O);
extern int IsGtkCurveObj (Stack stack, int i);
extern int IsGtkCurve(NspObject *O);
extern NspGtkCurve *GetGtkCurveCopy (Stack stack, int i);
extern NspGtkCurve *GetGtkCurve (Stack stack, int i);

#endif /* NSP_INC_NspGtkCurve */ 

#ifdef NspGtkCurve_Private 
static int init_gtkcurve(NspGtkCurve *o,NspTypeGtkCurve *type);
static char *nsp_gtkcurve_type_as_string(void);
static char *nsp_gtkcurve_type_short_string(NspObject *v);
static AttrTab gtkcurve_attrs[];
static NspMethods *gtkcurve_get_methods(void);
/* static int int_gtkcurve_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkCurve_Private */
