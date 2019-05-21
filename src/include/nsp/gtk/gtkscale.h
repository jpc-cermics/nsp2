/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkScale
#define NSP_INC_NspGtkScale

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

/* NspGtkScale */

#include <nsp/gtk/gtkrange.h>

/*
 * NspGtkScale inherits from GtkRange
 * just change some type attributes 
 */

typedef NspGtkRange NspGtkScale ;
typedef NspTypeGtkRange NspTypeGtkScale ;

extern int nsp_type_gtkscale_id;
extern NspTypeGtkScale *nsp_type_gtkscale;

/* type instances for gtkrange */

NspTypeGtkScale *new_type_gtkscale(type_mode mode);

/* instance for NspGtkScale */

NspGtkScale *new_gtkscale();

/*
 * Object methods redefined for gtkscale 
 */

#define NULLGTKSCALE (NspGtkScale*) 0


/* from NspGtkScaleObj.c */

extern NspGtkScale *nsp_gtkscale_object (NspObject *O);
extern int IsGtkScaleObj (Stack stack, int i);
extern int IsGtkScale(NspObject *O);
extern NspGtkScale *GetGtkScaleCopy (Stack stack, int i);
extern NspGtkScale *GetGtkScale (Stack stack, int i);

#endif /* NSP_INC_NspGtkScale */ 

#ifdef NspGtkScale_Private 
static int init_gtkscale(NspGtkScale *o,NspTypeGtkScale *type);
static char *nsp_gtkscale_type_as_string(void);
static char *nsp_gtkscale_type_short_string(NspObject *v);
static AttrTab gtkscale_attrs[];
static NspMethods *gtkscale_get_methods(void);
/* static int int_gtkscale_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkScale_Private */
