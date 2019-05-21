/* -*- Mode: C -*- */
#ifndef NSP_INC_NspPangoFontFamily
#define NSP_INC_NspPangoFontFamily

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

/* NspPangoFontFamily */

#include <nsp/gtk/gobject.h>

/*
 * NspPangoFontFamily inherits from GObject
 * just change some type attributes 
 */

typedef NspGObject NspPangoFontFamily ;
typedef NspTypeGObject NspTypePangoFontFamily ;

extern int nsp_type_pangofontfamily_id;
extern NspTypePangoFontFamily *nsp_type_pangofontfamily;

/* type instances for gobject */

NspTypePangoFontFamily *new_type_pangofontfamily(type_mode mode);

/* instance for NspPangoFontFamily */

NspPangoFontFamily *new_pangofontfamily();

/*
 * Object methods redefined for pangofontfamily 
 */

#define NULLPANGOFONTFAMILY (NspPangoFontFamily*) 0


/* from NspPangoFontFamilyObj.c */

extern NspPangoFontFamily *nsp_pangofontfamily_object (NspObject *O);
extern int IsPangoFontFamilyObj (Stack stack, int i);
extern int IsPangoFontFamily(NspObject *O);
extern NspPangoFontFamily *GetPangoFontFamilyCopy (Stack stack, int i);
extern NspPangoFontFamily *GetPangoFontFamily (Stack stack, int i);

#endif /* NSP_INC_NspPangoFontFamily */ 

#ifdef NspPangoFontFamily_Private 
static int init_pangofontfamily(NspPangoFontFamily *o,NspTypePangoFontFamily *type);
static char *nsp_pangofontfamily_type_as_string(void);
static char *nsp_pangofontfamily_type_short_string(NspObject *v);
static AttrTab pangofontfamily_attrs[];
static NspMethods *pangofontfamily_get_methods(void);
/* static int int_pangofontfamily_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspPangoFontFamily_Private */
