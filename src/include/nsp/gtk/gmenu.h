/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGMenu
#define NSP_INC_NspGMenu

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

/* NspGMenu */

#include <nsp/gtk/gmenumodel.h>

/*
 * NspGMenu inherits from GMenuModel
 * just change some type attributes 
 */

typedef NspGMenuModel NspGMenu ;
typedef NspTypeGMenuModel NspTypeGMenu ;

extern int nsp_type_gmenu_id;
extern NspTypeGMenu *nsp_type_gmenu;

/* type instances for gmenumodel */

NspTypeGMenu *new_type_gmenu(type_mode mode);

/* instance for NspGMenu */

NspGMenu *new_gmenu();

/*
 * Object methods redefined for gmenu 
 */

#define NULLGMENU (NspGMenu*) 0


/* from NspGMenuObj.c */

extern NspGMenu *nsp_gmenu_object (NspObject *O);
extern int IsGMenuObj (Stack stack, int i);
extern int IsGMenu(NspObject *O);
extern NspGMenu *GetGMenuCopy (Stack stack, int i);
extern NspGMenu *GetGMenu (Stack stack, int i);

#endif /* NSP_INC_NspGMenu */ 

#ifdef NspGMenu_Private 
static int init_gmenu(NspGMenu *o,NspTypeGMenu *type);
static char *nsp_gmenu_type_as_string(void);
static char *nsp_gmenu_type_short_string(NspObject *v);
static AttrTab gmenu_attrs[];
static NspMethods *gmenu_get_methods(void);
/* static int int_gmenu_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGMenu_Private */
