/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkSwitch
#define NSP_INC_NspGtkSwitch

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

/* NspGtkSwitch */

#include <nsp/gtk/gtkwidget.h>

/*
 * NspGtkSwitch inherits from GtkWidget
 * just change some type attributes 
 */

typedef NspGtkWidget NspGtkSwitch ;
typedef NspTypeGtkWidget NspTypeGtkSwitch ;

extern int nsp_type_gtkswitch_id;
extern NspTypeGtkSwitch *nsp_type_gtkswitch;

/* type instances for gtkwidget */

NspTypeGtkSwitch *new_type_gtkswitch(type_mode mode);

/* instance for NspGtkSwitch */

NspGtkSwitch *new_gtkswitch();

/*
 * Object methods redefined for gtkswitch 
 */

#define NULLGTKSWITCH (NspGtkSwitch*) 0


/* from NspGtkSwitchObj.c */

extern NspGtkSwitch *nsp_gtkswitch_object (NspObject *O);
extern int IsGtkSwitchObj (Stack stack, int i);
extern int IsGtkSwitch(NspObject *O);
extern NspGtkSwitch *GetGtkSwitchCopy (Stack stack, int i);
extern NspGtkSwitch *GetGtkSwitch (Stack stack, int i);

#endif /* NSP_INC_NspGtkSwitch */ 

#ifdef NspGtkSwitch_Private 
static int init_gtkswitch(NspGtkSwitch *o,NspTypeGtkSwitch *type);
static char *nsp_gtkswitch_type_as_string(void);
static char *nsp_gtkswitch_type_short_string(NspObject *v);
static AttrTab gtkswitch_attrs[];
static NspMethods *gtkswitch_get_methods(void);
/* static int int_gtkswitch_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkSwitch_Private */
