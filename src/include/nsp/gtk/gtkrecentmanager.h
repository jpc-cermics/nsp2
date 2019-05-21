/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkRecentManager
#define NSP_INC_NspGtkRecentManager

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

/* NspGtkRecentManager */

#include <nsp/gtk/gobject.h>

/*
 * NspGtkRecentManager inherits from GObject
 * just change some type attributes 
 */

typedef NspGObject NspGtkRecentManager ;
typedef NspTypeGObject NspTypeGtkRecentManager ;

extern int nsp_type_gtkrecentmanager_id;
extern NspTypeGtkRecentManager *nsp_type_gtkrecentmanager;

/* type instances for gobject */

NspTypeGtkRecentManager *new_type_gtkrecentmanager(type_mode mode);

/* instance for NspGtkRecentManager */

NspGtkRecentManager *new_gtkrecentmanager();

/*
 * Object methods redefined for gtkrecentmanager 
 */

#define NULLGTKRECENTMANAGER (NspGtkRecentManager*) 0


/* from NspGtkRecentManagerObj.c */

extern NspGtkRecentManager *nsp_gtkrecentmanager_object (NspObject *O);
extern int IsGtkRecentManagerObj (Stack stack, int i);
extern int IsGtkRecentManager(NspObject *O);
extern NspGtkRecentManager *GetGtkRecentManagerCopy (Stack stack, int i);
extern NspGtkRecentManager *GetGtkRecentManager (Stack stack, int i);

#endif /* NSP_INC_NspGtkRecentManager */ 

#ifdef NspGtkRecentManager_Private 
static int init_gtkrecentmanager(NspGtkRecentManager *o,NspTypeGtkRecentManager *type);
static char *nsp_gtkrecentmanager_type_as_string(void);
static char *nsp_gtkrecentmanager_type_short_string(NspObject *v);
static AttrTab gtkrecentmanager_attrs[];
static NspMethods *gtkrecentmanager_get_methods(void);
/* static int int_gtkrecentmanager_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkRecentManager_Private */
