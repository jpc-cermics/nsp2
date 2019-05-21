/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkCheckMenuItem
#define NSP_INC_NspGtkCheckMenuItem

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

/* NspGtkCheckMenuItem */

#include <nsp/gtk/gtkmenuitem.h>

/*
 * NspGtkCheckMenuItem inherits from GtkMenuItem
 * just change some type attributes 
 */

typedef NspGtkMenuItem NspGtkCheckMenuItem ;
typedef NspTypeGtkMenuItem NspTypeGtkCheckMenuItem ;

extern int nsp_type_gtkcheckmenuitem_id;
extern NspTypeGtkCheckMenuItem *nsp_type_gtkcheckmenuitem;

/* type instances for gtkmenuitem */

NspTypeGtkCheckMenuItem *new_type_gtkcheckmenuitem(type_mode mode);

/* instance for NspGtkCheckMenuItem */

NspGtkCheckMenuItem *new_gtkcheckmenuitem();

/*
 * Object methods redefined for gtkcheckmenuitem 
 */

#define NULLGTKCHECKMENUITEM (NspGtkCheckMenuItem*) 0


/* from NspGtkCheckMenuItemObj.c */

extern NspGtkCheckMenuItem *nsp_gtkcheckmenuitem_object (NspObject *O);
extern int IsGtkCheckMenuItemObj (Stack stack, int i);
extern int IsGtkCheckMenuItem(NspObject *O);
extern NspGtkCheckMenuItem *GetGtkCheckMenuItemCopy (Stack stack, int i);
extern NspGtkCheckMenuItem *GetGtkCheckMenuItem (Stack stack, int i);

#endif /* NSP_INC_NspGtkCheckMenuItem */ 

#ifdef NspGtkCheckMenuItem_Private 
static int init_gtkcheckmenuitem(NspGtkCheckMenuItem *o,NspTypeGtkCheckMenuItem *type);
static char *nsp_gtkcheckmenuitem_type_as_string(void);
static char *nsp_gtkcheckmenuitem_type_short_string(NspObject *v);
static AttrTab gtkcheckmenuitem_attrs[];
static NspMethods *gtkcheckmenuitem_get_methods(void);
/* static int int_gtkcheckmenuitem_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkCheckMenuItem_Private */
