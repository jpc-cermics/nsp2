/* -*- Mode: C -*- */
#ifndef NSP_INC_NspPangoFontFace
#define NSP_INC_NspPangoFontFace

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

/* NspPangoFontFace */

#include <nsp/gtk/gobject.h>

/*
 * NspPangoFontFace inherits from GObject
 * just change some type attributes 
 */

typedef NspGObject NspPangoFontFace ;
typedef NspTypeGObject NspTypePangoFontFace ;

extern int nsp_type_pangofontface_id;
extern NspTypePangoFontFace *nsp_type_pangofontface;

/* type instances for gobject */

NspTypePangoFontFace *new_type_pangofontface(type_mode mode);

/* instance for NspPangoFontFace */

NspPangoFontFace *new_pangofontface();

/*
 * Object methods redefined for pangofontface 
 */

#define NULLPANGOFONTFACE (NspPangoFontFace*) 0


/* from NspPangoFontFaceObj.c */

extern NspPangoFontFace *nsp_pangofontface_object (NspObject *O);
extern int IsPangoFontFaceObj (Stack stack, int i);
extern int IsPangoFontFace(NspObject *O);
extern NspPangoFontFace *GetPangoFontFaceCopy (Stack stack, int i);
extern NspPangoFontFace *GetPangoFontFace (Stack stack, int i);

#endif /* NSP_INC_NspPangoFontFace */ 

#ifdef NspPangoFontFace_Private 
static int init_pangofontface(NspPangoFontFace *o,NspTypePangoFontFace *type);
static char *nsp_pangofontface_type_as_string(void);
static char *nsp_pangofontface_type_short_string(NspObject *v);
static AttrTab pangofontface_attrs[];
static NspMethods *pangofontface_get_methods(void);
/* static int int_pangofontface_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspPangoFontFace_Private */
