/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkToolPalette
#define NSP_INC_NspGtkToolPalette

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

/* NspGtkToolPalette */

#include <nsp/gtk/gtkcontainer.h>

/*
 * NspGtkToolPalette inherits from GtkContainer
 * just change some type attributes 
 */

typedef NspGtkContainer NspGtkToolPalette ;
typedef NspTypeGtkContainer NspTypeGtkToolPalette ;

extern int nsp_type_gtktoolpalette_id;
extern NspTypeGtkToolPalette *nsp_type_gtktoolpalette;

/* type instances for gtkcontainer */

NspTypeGtkToolPalette *new_type_gtktoolpalette(type_mode mode);

/* instance for NspGtkToolPalette */

NspGtkToolPalette *new_gtktoolpalette();

/*
 * Object methods redefined for gtktoolpalette 
 */

#define NULLGTKTOOLPALETTE (NspGtkToolPalette*) 0


/* from NspGtkToolPaletteObj.c */

extern NspGtkToolPalette *nsp_gtktoolpalette_object (NspObject *O);
extern int IsGtkToolPaletteObj (Stack stack, int i);
extern int IsGtkToolPalette(NspObject *O);
extern NspGtkToolPalette *GetGtkToolPaletteCopy (Stack stack, int i);
extern NspGtkToolPalette *GetGtkToolPalette (Stack stack, int i);

#endif /* NSP_INC_NspGtkToolPalette */ 

#ifdef NspGtkToolPalette_Private 
static int init_gtktoolpalette(NspGtkToolPalette *o,NspTypeGtkToolPalette *type);
static char *nsp_gtktoolpalette_type_as_string(void);
static char *nsp_gtktoolpalette_type_short_string(NspObject *v);
static AttrTab gtktoolpalette_attrs[];
static NspMethods *gtktoolpalette_get_methods(void);
/* static int int_gtktoolpalette_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkToolPalette_Private */
