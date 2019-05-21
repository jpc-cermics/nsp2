/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkAccelLabel
#define NSP_INC_NspGtkAccelLabel

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

/* NspGtkAccelLabel */

#include <nsp/gtk/gtklabel.h>

/*
 * NspGtkAccelLabel inherits from GtkLabel
 * just change some type attributes 
 */

typedef NspGtkLabel NspGtkAccelLabel ;
typedef NspTypeGtkLabel NspTypeGtkAccelLabel ;

extern int nsp_type_gtkaccellabel_id;
extern NspTypeGtkAccelLabel *nsp_type_gtkaccellabel;

/* type instances for gtklabel */

NspTypeGtkAccelLabel *new_type_gtkaccellabel(type_mode mode);

/* instance for NspGtkAccelLabel */

NspGtkAccelLabel *new_gtkaccellabel();

/*
 * Object methods redefined for gtkaccellabel 
 */

#define NULLGTKACCELLABEL (NspGtkAccelLabel*) 0


/* from NspGtkAccelLabelObj.c */

extern NspGtkAccelLabel *nsp_gtkaccellabel_object (NspObject *O);
extern int IsGtkAccelLabelObj (Stack stack, int i);
extern int IsGtkAccelLabel(NspObject *O);
extern NspGtkAccelLabel *GetGtkAccelLabelCopy (Stack stack, int i);
extern NspGtkAccelLabel *GetGtkAccelLabel (Stack stack, int i);

#endif /* NSP_INC_NspGtkAccelLabel */ 

#ifdef NspGtkAccelLabel_Private 
static int init_gtkaccellabel(NspGtkAccelLabel *o,NspTypeGtkAccelLabel *type);
static char *nsp_gtkaccellabel_type_as_string(void);
static char *nsp_gtkaccellabel_type_short_string(NspObject *v);
static AttrTab gtkaccellabel_attrs[];
static NspMethods *gtkaccellabel_get_methods(void);
/* static int int_gtkaccellabel_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkAccelLabel_Private */
