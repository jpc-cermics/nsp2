/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkSourceStyleSchemeManager
#define NSP_INC_NspGtkSourceStyleSchemeManager

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

/* NspGtkSourceStyleSchemeManager */

#include <nsp/gtk/gobject.h>

/*
 * NspGtkSourceStyleSchemeManager inherits from GObject
 * just change some type attributes 
 */

typedef NspGObject NspGtkSourceStyleSchemeManager ;
typedef NspTypeGObject NspTypeGtkSourceStyleSchemeManager ;

extern int nsp_type_gtksourcestyleschememanager_id;
extern NspTypeGtkSourceStyleSchemeManager *nsp_type_gtksourcestyleschememanager;

/* type instances for gobject */

NspTypeGtkSourceStyleSchemeManager *new_type_gtksourcestyleschememanager(type_mode mode);

/* instance for NspGtkSourceStyleSchemeManager */

NspGtkSourceStyleSchemeManager *new_gtksourcestyleschememanager();

/*
 * Object methods redefined for gtksourcestyleschememanager 
 */

#define NULLGTKSOURCESTYLESCHEMEMANAGER (NspGtkSourceStyleSchemeManager*) 0


/* from NspGtkSourceStyleSchemeManagerObj.c */

extern NspGtkSourceStyleSchemeManager *nsp_gtksourcestyleschememanager_object (NspObject *O);
extern int IsGtkSourceStyleSchemeManagerObj (Stack stack, int i);
extern int IsGtkSourceStyleSchemeManager(NspObject *O);
extern NspGtkSourceStyleSchemeManager *GetGtkSourceStyleSchemeManagerCopy (Stack stack, int i);
extern NspGtkSourceStyleSchemeManager *GetGtkSourceStyleSchemeManager (Stack stack, int i);

#endif /* NSP_INC_NspGtkSourceStyleSchemeManager */ 

#ifdef NspGtkSourceStyleSchemeManager_Private 
static int init_gtksourcestyleschememanager(NspGtkSourceStyleSchemeManager *o,NspTypeGtkSourceStyleSchemeManager *type);
static char *nsp_gtksourcestyleschememanager_type_as_string(void);
static char *nsp_gtksourcestyleschememanager_type_short_string(NspObject *v);
static AttrTab gtksourcestyleschememanager_attrs[];
static NspMethods *gtksourcestyleschememanager_get_methods(void);
/* static int int_gtksourcestyleschememanager_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkSourceStyleSchemeManager_Private */
