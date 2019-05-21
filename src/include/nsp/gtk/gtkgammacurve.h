/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkGammaCurve
#define NSP_INC_NspGtkGammaCurve

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

/* NspGtkGammaCurve */

#include <nsp/gtk/gtkvbox.h>

/*
 * NspGtkGammaCurve inherits from GtkVBox
 * just change some type attributes 
 */

typedef NspGtkVBox NspGtkGammaCurve ;
typedef NspTypeGtkVBox NspTypeGtkGammaCurve ;

extern int nsp_type_gtkgammacurve_id;
extern NspTypeGtkGammaCurve *nsp_type_gtkgammacurve;

/* type instances for gtkvbox */

NspTypeGtkGammaCurve *new_type_gtkgammacurve(type_mode mode);

/* instance for NspGtkGammaCurve */

NspGtkGammaCurve *new_gtkgammacurve();

/*
 * Object methods redefined for gtkgammacurve 
 */

#define NULLGTKGAMMACURVE (NspGtkGammaCurve*) 0


/* from NspGtkGammaCurveObj.c */

extern NspGtkGammaCurve *nsp_gtkgammacurve_object (NspObject *O);
extern int IsGtkGammaCurveObj (Stack stack, int i);
extern int IsGtkGammaCurve(NspObject *O);
extern NspGtkGammaCurve *GetGtkGammaCurveCopy (Stack stack, int i);
extern NspGtkGammaCurve *GetGtkGammaCurve (Stack stack, int i);

#endif /* NSP_INC_NspGtkGammaCurve */ 

#ifdef NspGtkGammaCurve_Private 
static int init_gtkgammacurve(NspGtkGammaCurve *o,NspTypeGtkGammaCurve *type);
static char *nsp_gtkgammacurve_type_as_string(void);
static char *nsp_gtkgammacurve_type_short_string(NspObject *v);
static AttrTab gtkgammacurve_attrs[];
static NspMethods *gtkgammacurve_get_methods(void);
/* static int int_gtkgammacurve_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkGammaCurve_Private */
