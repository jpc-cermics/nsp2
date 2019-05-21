/* -*- Mode: C -*- */
#ifndef NSP_INC_NspPangoFontDescription
#define NSP_INC_NspPangoFontDescription

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

/* NspPangoFontDescription */

#include <nsp/gtk/gboxed.h>

/*
 * NspPangoFontDescription inherits from GBoxed
 * just change some type attributes 
 */

typedef NspGBoxed NspPangoFontDescription ;
typedef NspTypeGBoxed NspTypePangoFontDescription ;

extern int nsp_type_pangofontdescription_id;
extern NspTypePangoFontDescription *nsp_type_pangofontdescription;

/* type instances for gboxed */

NspTypePangoFontDescription *new_type_pangofontdescription(type_mode mode);

/* instance for NspPangoFontDescription */

NspPangoFontDescription *new_pangofontdescription();

/*
 * Object methods redefined for pangofontdescription 
 */

#define NULLPANGOFONTDESCRIPTION (NspPangoFontDescription*) 0


/* from NspPangoFontDescriptionObj.c */

extern NspPangoFontDescription *nsp_pangofontdescription_object (NspObject *O);
extern int IsPangoFontDescriptionObj (Stack stack, int i);
extern int IsPangoFontDescription(NspObject *O);
extern NspPangoFontDescription *GetPangoFontDescriptionCopy (Stack stack, int i);
extern NspPangoFontDescription *GetPangoFontDescription (Stack stack, int i);

#endif /* NSP_INC_NspPangoFontDescription */ 

#ifdef NspPangoFontDescription_Private 
static int init_pangofontdescription(NspPangoFontDescription *o,NspTypePangoFontDescription *type);
static char *nsp_pangofontdescription_type_as_string(void);
static char *nsp_pangofontdescription_type_short_string(NspObject *v);
static AttrTab pangofontdescription_attrs[];
static NspMethods *pangofontdescription_get_methods(void);
/* static int int_pangofontdescription_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspPangoFontDescription_Private */
