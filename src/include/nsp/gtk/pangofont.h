/* -*- Mode: C -*- */
#ifndef NSP_INC_NspPangoFont
#define NSP_INC_NspPangoFont

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

/* NspPangoFont */

#include <nsp/gtk/gobject.h>

/*
 * NspPangoFont inherits from GObject
 * just change some type attributes 
 */

typedef NspGObject NspPangoFont ;
typedef NspTypeGObject NspTypePangoFont ;

extern int nsp_type_pangofont_id;
extern NspTypePangoFont *nsp_type_pangofont;

/* type instances for gobject */

NspTypePangoFont *new_type_pangofont(type_mode mode);

/* instance for NspPangoFont */

NspPangoFont *new_pangofont();

/*
 * Object methods redefined for pangofont 
 */

#define NULLPANGOFONT (NspPangoFont*) 0


/* from NspPangoFontObj.c */

extern NspPangoFont *nsp_pangofont_object (NspObject *O);
extern int IsPangoFontObj (Stack stack, int i);
extern int IsPangoFont(NspObject *O);
extern NspPangoFont *GetPangoFontCopy (Stack stack, int i);
extern NspPangoFont *GetPangoFont (Stack stack, int i);

#endif /* NSP_INC_NspPangoFont */ 

#ifdef NspPangoFont_Private 
static int init_pangofont(NspPangoFont *o,NspTypePangoFont *type);
static char *nsp_pangofont_type_as_string(void);
static char *nsp_pangofont_type_short_string(NspObject *v);
static AttrTab pangofont_attrs[];
static NspMethods *pangofont_get_methods(void);
/* static int int_pangofont_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspPangoFont_Private */
