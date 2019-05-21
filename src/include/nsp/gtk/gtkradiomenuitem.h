/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkRadioMenuItem
#define NSP_INC_NspGtkRadioMenuItem

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

/* NspGtkRadioMenuItem */

#include <nsp/gtk/gtkcheckmenuitem.h>

/*
 * NspGtkRadioMenuItem inherits from GtkCheckMenuItem
 * just change some type attributes 
 */

typedef NspGtkCheckMenuItem NspGtkRadioMenuItem ;
typedef NspTypeGtkCheckMenuItem NspTypeGtkRadioMenuItem ;

extern int nsp_type_gtkradiomenuitem_id;
extern NspTypeGtkRadioMenuItem *nsp_type_gtkradiomenuitem;

/* type instances for gtkcheckmenuitem */

NspTypeGtkRadioMenuItem *new_type_gtkradiomenuitem(type_mode mode);

/* instance for NspGtkRadioMenuItem */

NspGtkRadioMenuItem *new_gtkradiomenuitem();

/*
 * Object methods redefined for gtkradiomenuitem 
 */

#define NULLGTKRADIOMENUITEM (NspGtkRadioMenuItem*) 0


/* from NspGtkRadioMenuItemObj.c */

extern NspGtkRadioMenuItem *nsp_gtkradiomenuitem_object (NspObject *O);
extern int IsGtkRadioMenuItemObj (Stack stack, int i);
extern int IsGtkRadioMenuItem(NspObject *O);
extern NspGtkRadioMenuItem *GetGtkRadioMenuItemCopy (Stack stack, int i);
extern NspGtkRadioMenuItem *GetGtkRadioMenuItem (Stack stack, int i);

#endif /* NSP_INC_NspGtkRadioMenuItem */ 

#ifdef NspGtkRadioMenuItem_Private 
static int init_gtkradiomenuitem(NspGtkRadioMenuItem *o,NspTypeGtkRadioMenuItem *type);
static char *nsp_gtkradiomenuitem_type_as_string(void);
static char *nsp_gtkradiomenuitem_type_short_string(NspObject *v);
static AttrTab gtkradiomenuitem_attrs[];
static NspMethods *gtkradiomenuitem_get_methods(void);
/* static int int_gtkradiomenuitem_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkRadioMenuItem_Private */
