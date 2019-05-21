/* -*- Mode: C -*- */
#ifndef NSP_INC_NspPangoFontMap
#define NSP_INC_NspPangoFontMap

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

/* NspPangoFontMap */

#include <nsp/gtk/gobject.h>

/*
 * NspPangoFontMap inherits from GObject
 * just change some type attributes 
 */

typedef NspGObject NspPangoFontMap ;
typedef NspTypeGObject NspTypePangoFontMap ;

extern int nsp_type_pangofontmap_id;
extern NspTypePangoFontMap *nsp_type_pangofontmap;

/* type instances for gobject */

NspTypePangoFontMap *new_type_pangofontmap(type_mode mode);

/* instance for NspPangoFontMap */

NspPangoFontMap *new_pangofontmap();

/*
 * Object methods redefined for pangofontmap 
 */

#define NULLPANGOFONTMAP (NspPangoFontMap*) 0


/* from NspPangoFontMapObj.c */

extern NspPangoFontMap *nsp_pangofontmap_object (NspObject *O);
extern int IsPangoFontMapObj (Stack stack, int i);
extern int IsPangoFontMap(NspObject *O);
extern NspPangoFontMap *GetPangoFontMapCopy (Stack stack, int i);
extern NspPangoFontMap *GetPangoFontMap (Stack stack, int i);

#endif /* NSP_INC_NspPangoFontMap */ 

#ifdef NspPangoFontMap_Private 
static int init_pangofontmap(NspPangoFontMap *o,NspTypePangoFontMap *type);
static char *nsp_pangofontmap_type_as_string(void);
static char *nsp_pangofontmap_type_short_string(NspObject *v);
static AttrTab pangofontmap_attrs[];
static NspMethods *pangofontmap_get_methods(void);
/* static int int_pangofontmap_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspPangoFontMap_Private */
