/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkBox
#define NSP_INC_NspGtkBox

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

/* NspGtkBox */

#include <nsp/gtk/gtkcontainer.h>

/*
 * NspGtkBox inherits from GtkContainer
 * just change some type attributes 
 */

typedef NspGtkContainer NspGtkBox ;
typedef NspTypeGtkContainer NspTypeGtkBox ;

extern int nsp_type_gtkbox_id;
extern NspTypeGtkBox *nsp_type_gtkbox;

/* type instances for gtkcontainer */

NspTypeGtkBox *new_type_gtkbox(type_mode mode);

/* instance for NspGtkBox */

NspGtkBox *new_gtkbox();

/*
 * Object methods redefined for gtkbox 
 */

#define NULLGTKBOX (NspGtkBox*) 0


/* from NspGtkBoxObj.c */

extern NspGtkBox *nsp_gtkbox_object (NspObject *O);
extern int IsGtkBoxObj (Stack stack, int i);
extern int IsGtkBox(NspObject *O);
extern NspGtkBox *GetGtkBoxCopy (Stack stack, int i);
extern NspGtkBox *GetGtkBox (Stack stack, int i);

#endif /* NSP_INC_NspGtkBox */ 

#ifdef NspGtkBox_Private 
static int init_gtkbox(NspGtkBox *o,NspTypeGtkBox *type);
static char *nsp_gtkbox_type_as_string(void);
static char *nsp_gtkbox_type_short_string(NspObject *v);
static AttrTab gtkbox_attrs[];
static NspMethods *gtkbox_get_methods(void);
/* static int int_gtkbox_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkBox_Private */
