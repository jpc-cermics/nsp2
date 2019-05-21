/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkStatusbar
#define NSP_INC_NspGtkStatusbar

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

/* NspGtkStatusbar */

#include <nsp/gtk/gtkbox.h>

/*
 * NspGtkStatusbar inherits from GtkBox
 * just change some type attributes 
 */

typedef NspGtkBox NspGtkStatusbar ;
typedef NspTypeGtkBox NspTypeGtkStatusbar ;

extern int nsp_type_gtkstatusbar_id;
extern NspTypeGtkStatusbar *nsp_type_gtkstatusbar;

/* type instances for gtkbox */

NspTypeGtkStatusbar *new_type_gtkstatusbar(type_mode mode);

/* instance for NspGtkStatusbar */

NspGtkStatusbar *new_gtkstatusbar();

/*
 * Object methods redefined for gtkstatusbar 
 */

#define NULLGTKSTATUSBAR (NspGtkStatusbar*) 0


/* from NspGtkStatusbarObj.c */

extern NspGtkStatusbar *nsp_gtkstatusbar_object (NspObject *O);
extern int IsGtkStatusbarObj (Stack stack, int i);
extern int IsGtkStatusbar(NspObject *O);
extern NspGtkStatusbar *GetGtkStatusbarCopy (Stack stack, int i);
extern NspGtkStatusbar *GetGtkStatusbar (Stack stack, int i);

#endif /* NSP_INC_NspGtkStatusbar */ 

#ifdef NspGtkStatusbar_Private 
static int init_gtkstatusbar(NspGtkStatusbar *o,NspTypeGtkStatusbar *type);
static char *nsp_gtkstatusbar_type_as_string(void);
static char *nsp_gtkstatusbar_type_short_string(NspObject *v);
static AttrTab gtkstatusbar_attrs[];
static NspMethods *gtkstatusbar_get_methods(void);
/* static int int_gtkstatusbar_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkStatusbar_Private */
