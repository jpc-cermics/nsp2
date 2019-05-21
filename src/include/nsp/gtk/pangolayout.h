/* -*- Mode: C -*- */
#ifndef NSP_INC_NspPangoLayout
#define NSP_INC_NspPangoLayout

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

/* NspPangoLayout */

#include <nsp/gtk/gobject.h>

/*
 * NspPangoLayout inherits from GObject
 * just change some type attributes 
 */

typedef NspGObject NspPangoLayout ;
typedef NspTypeGObject NspTypePangoLayout ;

extern int nsp_type_pangolayout_id;
extern NspTypePangoLayout *nsp_type_pangolayout;

/* type instances for gobject */

NspTypePangoLayout *new_type_pangolayout(type_mode mode);

/* instance for NspPangoLayout */

NspPangoLayout *new_pangolayout();

/*
 * Object methods redefined for pangolayout 
 */

#define NULLPANGOLAYOUT (NspPangoLayout*) 0


/* from NspPangoLayoutObj.c */

extern NspPangoLayout *nsp_pangolayout_object (NspObject *O);
extern int IsPangoLayoutObj (Stack stack, int i);
extern int IsPangoLayout(NspObject *O);
extern NspPangoLayout *GetPangoLayoutCopy (Stack stack, int i);
extern NspPangoLayout *GetPangoLayout (Stack stack, int i);

#endif /* NSP_INC_NspPangoLayout */ 

#ifdef NspPangoLayout_Private 
static int init_pangolayout(NspPangoLayout *o,NspTypePangoLayout *type);
static char *nsp_pangolayout_type_as_string(void);
static char *nsp_pangolayout_type_short_string(NspObject *v);
static AttrTab pangolayout_attrs[];
static NspMethods *pangolayout_get_methods(void);
/* static int int_pangolayout_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspPangoLayout_Private */
