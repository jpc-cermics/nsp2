/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkVButtonBox
#define NSP_INC_NspGtkVButtonBox

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

/* NspGtkVButtonBox */

#include <nsp/gtk/gtkbuttonbox.h>

/*
 * NspGtkVButtonBox inherits from GtkButtonBox
 * just change some type attributes 
 */

typedef NspGtkButtonBox NspGtkVButtonBox ;
typedef NspTypeGtkButtonBox NspTypeGtkVButtonBox ;

extern int nsp_type_gtkvbuttonbox_id;
extern NspTypeGtkVButtonBox *nsp_type_gtkvbuttonbox;

/* type instances for gtkbuttonbox */

NspTypeGtkVButtonBox *new_type_gtkvbuttonbox(type_mode mode);

/* instance for NspGtkVButtonBox */

NspGtkVButtonBox *new_gtkvbuttonbox();

/*
 * Object methods redefined for gtkvbuttonbox 
 */

#define NULLGTKVBUTTONBOX (NspGtkVButtonBox*) 0


/* from NspGtkVButtonBoxObj.c */

extern NspGtkVButtonBox *nsp_gtkvbuttonbox_object (NspObject *O);
extern int IsGtkVButtonBoxObj (Stack stack, int i);
extern int IsGtkVButtonBox(NspObject *O);
extern NspGtkVButtonBox *GetGtkVButtonBoxCopy (Stack stack, int i);
extern NspGtkVButtonBox *GetGtkVButtonBox (Stack stack, int i);

#endif /* NSP_INC_NspGtkVButtonBox */ 

#ifdef NspGtkVButtonBox_Private 
static int init_gtkvbuttonbox(NspGtkVButtonBox *o,NspTypeGtkVButtonBox *type);
static char *nsp_gtkvbuttonbox_type_as_string(void);
static char *nsp_gtkvbuttonbox_type_short_string(NspObject *v);
static AttrTab gtkvbuttonbox_attrs[];
static NspMethods *gtkvbuttonbox_get_methods(void);
/* static int int_gtkvbuttonbox_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkVButtonBox_Private */
