/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkImageMenuItem
#define NSP_INC_NspGtkImageMenuItem

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

/* NspGtkImageMenuItem */

#include <nsp/gtk/gtkmenuitem.h>

/*
 * NspGtkImageMenuItem inherits from GtkMenuItem
 * just change some type attributes 
 */

typedef NspGtkMenuItem NspGtkImageMenuItem ;
typedef NspTypeGtkMenuItem NspTypeGtkImageMenuItem ;

extern int nsp_type_gtkimagemenuitem_id;
extern NspTypeGtkImageMenuItem *nsp_type_gtkimagemenuitem;

/* type instances for gtkmenuitem */

NspTypeGtkImageMenuItem *new_type_gtkimagemenuitem(type_mode mode);

/* instance for NspGtkImageMenuItem */

NspGtkImageMenuItem *new_gtkimagemenuitem();

/*
 * Object methods redefined for gtkimagemenuitem 
 */

#define NULLGTKIMAGEMENUITEM (NspGtkImageMenuItem*) 0


/* from NspGtkImageMenuItemObj.c */

extern NspGtkImageMenuItem *nsp_gtkimagemenuitem_object (NspObject *O);
extern int IsGtkImageMenuItemObj (Stack stack, int i);
extern int IsGtkImageMenuItem(NspObject *O);
extern NspGtkImageMenuItem *GetGtkImageMenuItemCopy (Stack stack, int i);
extern NspGtkImageMenuItem *GetGtkImageMenuItem (Stack stack, int i);

#endif /* NSP_INC_NspGtkImageMenuItem */ 

#ifdef NspGtkImageMenuItem_Private 
static int init_gtkimagemenuitem(NspGtkImageMenuItem *o,NspTypeGtkImageMenuItem *type);
static char *nsp_gtkimagemenuitem_type_as_string(void);
static char *nsp_gtkimagemenuitem_type_short_string(NspObject *v);
static AttrTab gtkimagemenuitem_attrs[];
static NspMethods *gtkimagemenuitem_get_methods(void);
/* static int int_gtkimagemenuitem_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkImageMenuItem_Private */
