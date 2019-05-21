/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkUIManager
#define NSP_INC_NspGtkUIManager

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

/* NspGtkUIManager */

#include <nsp/gtk/gobject.h>

/*
 * NspGtkUIManager inherits from GObject
 * just change some type attributes 
 */

typedef NspGObject NspGtkUIManager ;
typedef NspTypeGObject NspTypeGtkUIManager ;

extern int nsp_type_gtkuimanager_id;
extern NspTypeGtkUIManager *nsp_type_gtkuimanager;

/* type instances for gobject */

NspTypeGtkUIManager *new_type_gtkuimanager(type_mode mode);

/* instance for NspGtkUIManager */

NspGtkUIManager *new_gtkuimanager();

/*
 * Object methods redefined for gtkuimanager 
 */

#define NULLGTKUIMANAGER (NspGtkUIManager*) 0


/* from NspGtkUIManagerObj.c */

extern NspGtkUIManager *nsp_gtkuimanager_object (NspObject *O);
extern int IsGtkUIManagerObj (Stack stack, int i);
extern int IsGtkUIManager(NspObject *O);
extern NspGtkUIManager *GetGtkUIManagerCopy (Stack stack, int i);
extern NspGtkUIManager *GetGtkUIManager (Stack stack, int i);

#endif /* NSP_INC_NspGtkUIManager */ 

#ifdef NspGtkUIManager_Private 
static int init_gtkuimanager(NspGtkUIManager *o,NspTypeGtkUIManager *type);
static char *nsp_gtkuimanager_type_as_string(void);
static char *nsp_gtkuimanager_type_short_string(NspObject *v);
static AttrTab gtkuimanager_attrs[];
static NspMethods *gtkuimanager_get_methods(void);
/* static int int_gtkuimanager_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkUIManager_Private */
