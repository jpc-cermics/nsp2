/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkVBox
#define NSP_INC_NspGtkVBox

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

/* NspGtkVBox */

#include <nsp/gtk/gtkbox.h>

/*
 * NspGtkVBox inherits from GtkBox
 * just change some type attributes 
 */

typedef NspGtkBox NspGtkVBox ;
typedef NspTypeGtkBox NspTypeGtkVBox ;

extern int nsp_type_gtkvbox_id;
extern NspTypeGtkVBox *nsp_type_gtkvbox;

/* type instances for gtkbox */

NspTypeGtkVBox *new_type_gtkvbox(type_mode mode);

/* instance for NspGtkVBox */

NspGtkVBox *new_gtkvbox();

/*
 * Object methods redefined for gtkvbox 
 */

#define NULLGTKVBOX (NspGtkVBox*) 0


/* from NspGtkVBoxObj.c */

extern NspGtkVBox *nsp_gtkvbox_object (NspObject *O);
extern int IsGtkVBoxObj (Stack stack, int i);
extern int IsGtkVBox(NspObject *O);
extern NspGtkVBox *GetGtkVBoxCopy (Stack stack, int i);
extern NspGtkVBox *GetGtkVBox (Stack stack, int i);

#endif /* NSP_INC_NspGtkVBox */ 

#ifdef NspGtkVBox_Private 
static int init_gtkvbox(NspGtkVBox *o,NspTypeGtkVBox *type);
static char *nsp_gtkvbox_type_as_string(void);
static char *nsp_gtkvbox_type_short_string(NspObject *v);
static AttrTab gtkvbox_attrs[];
static NspMethods *gtkvbox_get_methods(void);
/* static int int_gtkvbox_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkVBox_Private */
