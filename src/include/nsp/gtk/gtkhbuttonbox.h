/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkHButtonBox
#define NSP_INC_NspGtkHButtonBox

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

/* NspGtkHButtonBox */

#include <nsp/gtk/gtkbuttonbox.h>

/*
 * NspGtkHButtonBox inherits from GtkButtonBox
 * just change some type attributes 
 */

typedef NspGtkButtonBox NspGtkHButtonBox ;
typedef NspTypeGtkButtonBox NspTypeGtkHButtonBox ;

extern int nsp_type_gtkhbuttonbox_id;
extern NspTypeGtkHButtonBox *nsp_type_gtkhbuttonbox;

/* type instances for gtkbuttonbox */

NspTypeGtkHButtonBox *new_type_gtkhbuttonbox(type_mode mode);

/* instance for NspGtkHButtonBox */

NspGtkHButtonBox *new_gtkhbuttonbox();

/*
 * Object methods redefined for gtkhbuttonbox 
 */

#define NULLGTKHBUTTONBOX (NspGtkHButtonBox*) 0


/* from NspGtkHButtonBoxObj.c */

extern NspGtkHButtonBox *nsp_gtkhbuttonbox_object (NspObject *O);
extern int IsGtkHButtonBoxObj (Stack stack, int i);
extern int IsGtkHButtonBox(NspObject *O);
extern NspGtkHButtonBox *GetGtkHButtonBoxCopy (Stack stack, int i);
extern NspGtkHButtonBox *GetGtkHButtonBox (Stack stack, int i);

#endif /* NSP_INC_NspGtkHButtonBox */ 

#ifdef NspGtkHButtonBox_Private 
static int init_gtkhbuttonbox(NspGtkHButtonBox *o,NspTypeGtkHButtonBox *type);
static char *nsp_gtkhbuttonbox_type_as_string(void);
static char *nsp_gtkhbuttonbox_type_short_string(NspObject *v);
static AttrTab gtkhbuttonbox_attrs[];
static NspMethods *gtkhbuttonbox_get_methods(void);
/* static int int_gtkhbuttonbox_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkHButtonBox_Private */
