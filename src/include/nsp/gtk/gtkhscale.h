/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkHScale
#define NSP_INC_NspGtkHScale

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

/* NspGtkHScale */

#include <nsp/gtk/gtkscale.h>

/*
 * NspGtkHScale inherits from GtkScale
 * just change some type attributes 
 */

typedef NspGtkScale NspGtkHScale ;
typedef NspTypeGtkScale NspTypeGtkHScale ;

extern int nsp_type_gtkhscale_id;
extern NspTypeGtkHScale *nsp_type_gtkhscale;

/* type instances for gtkscale */

NspTypeGtkHScale *new_type_gtkhscale(type_mode mode);

/* instance for NspGtkHScale */

NspGtkHScale *new_gtkhscale();

/*
 * Object methods redefined for gtkhscale 
 */

#define NULLGTKHSCALE (NspGtkHScale*) 0


/* from NspGtkHScaleObj.c */

extern NspGtkHScale *nsp_gtkhscale_object (NspObject *O);
extern int IsGtkHScaleObj (Stack stack, int i);
extern int IsGtkHScale(NspObject *O);
extern NspGtkHScale *GetGtkHScaleCopy (Stack stack, int i);
extern NspGtkHScale *GetGtkHScale (Stack stack, int i);

#endif /* NSP_INC_NspGtkHScale */ 

#ifdef NspGtkHScale_Private 
static int init_gtkhscale(NspGtkHScale *o,NspTypeGtkHScale *type);
static char *nsp_gtkhscale_type_as_string(void);
static char *nsp_gtkhscale_type_short_string(NspObject *v);
static AttrTab gtkhscale_attrs[];
static NspMethods *gtkhscale_get_methods(void);
/* static int int_gtkhscale_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkHScale_Private */
