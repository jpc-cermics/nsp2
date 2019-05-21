/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkVScale
#define NSP_INC_NspGtkVScale

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

/* NspGtkVScale */

#include <nsp/gtk/gtkscale.h>

/*
 * NspGtkVScale inherits from GtkScale
 * just change some type attributes 
 */

typedef NspGtkScale NspGtkVScale ;
typedef NspTypeGtkScale NspTypeGtkVScale ;

extern int nsp_type_gtkvscale_id;
extern NspTypeGtkVScale *nsp_type_gtkvscale;

/* type instances for gtkscale */

NspTypeGtkVScale *new_type_gtkvscale(type_mode mode);

/* instance for NspGtkVScale */

NspGtkVScale *new_gtkvscale();

/*
 * Object methods redefined for gtkvscale 
 */

#define NULLGTKVSCALE (NspGtkVScale*) 0


/* from NspGtkVScaleObj.c */

extern NspGtkVScale *nsp_gtkvscale_object (NspObject *O);
extern int IsGtkVScaleObj (Stack stack, int i);
extern int IsGtkVScale(NspObject *O);
extern NspGtkVScale *GetGtkVScaleCopy (Stack stack, int i);
extern NspGtkVScale *GetGtkVScale (Stack stack, int i);

#endif /* NSP_INC_NspGtkVScale */ 

#ifdef NspGtkVScale_Private 
static int init_gtkvscale(NspGtkVScale *o,NspTypeGtkVScale *type);
static char *nsp_gtkvscale_type_as_string(void);
static char *nsp_gtkvscale_type_short_string(NspObject *v);
static AttrTab gtkvscale_attrs[];
static NspMethods *gtkvscale_get_methods(void);
/* static int int_gtkvscale_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkVScale_Private */
