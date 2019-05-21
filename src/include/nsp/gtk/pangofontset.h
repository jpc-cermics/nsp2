/* -*- Mode: C -*- */
#ifndef NSP_INC_NspPangoFontset
#define NSP_INC_NspPangoFontset

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

/* NspPangoFontset */

#include <nsp/gtk/gobject.h>

/*
 * NspPangoFontset inherits from GObject
 * just change some type attributes 
 */

typedef NspGObject NspPangoFontset ;
typedef NspTypeGObject NspTypePangoFontset ;

extern int nsp_type_pangofontset_id;
extern NspTypePangoFontset *nsp_type_pangofontset;

/* type instances for gobject */

NspTypePangoFontset *new_type_pangofontset(type_mode mode);

/* instance for NspPangoFontset */

NspPangoFontset *new_pangofontset();

/*
 * Object methods redefined for pangofontset 
 */

#define NULLPANGOFONTSET (NspPangoFontset*) 0


/* from NspPangoFontsetObj.c */

extern NspPangoFontset *nsp_pangofontset_object (NspObject *O);
extern int IsPangoFontsetObj (Stack stack, int i);
extern int IsPangoFontset(NspObject *O);
extern NspPangoFontset *GetPangoFontsetCopy (Stack stack, int i);
extern NspPangoFontset *GetPangoFontset (Stack stack, int i);

#endif /* NSP_INC_NspPangoFontset */ 

#ifdef NspPangoFontset_Private 
static int init_pangofontset(NspPangoFontset *o,NspTypePangoFontset *type);
static char *nsp_pangofontset_type_as_string(void);
static char *nsp_pangofontset_type_short_string(NspObject *v);
static AttrTab pangofontset_attrs[];
static NspMethods *pangofontset_get_methods(void);
/* static int int_pangofontset_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspPangoFontset_Private */
