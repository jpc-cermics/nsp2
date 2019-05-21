/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkSourceLanguageManager
#define NSP_INC_NspGtkSourceLanguageManager

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

/* NspGtkSourceLanguageManager */

#include <nsp/gtk/gobject.h>

/*
 * NspGtkSourceLanguageManager inherits from GObject
 * just change some type attributes 
 */

typedef NspGObject NspGtkSourceLanguageManager ;
typedef NspTypeGObject NspTypeGtkSourceLanguageManager ;

extern int nsp_type_gtksourcelanguagemanager_id;
extern NspTypeGtkSourceLanguageManager *nsp_type_gtksourcelanguagemanager;

/* type instances for gobject */

NspTypeGtkSourceLanguageManager *new_type_gtksourcelanguagemanager(type_mode mode);

/* instance for NspGtkSourceLanguageManager */

NspGtkSourceLanguageManager *new_gtksourcelanguagemanager();

/*
 * Object methods redefined for gtksourcelanguagemanager 
 */

#define NULLGTKSOURCELANGUAGEMANAGER (NspGtkSourceLanguageManager*) 0


/* from NspGtkSourceLanguageManagerObj.c */

extern NspGtkSourceLanguageManager *nsp_gtksourcelanguagemanager_object (NspObject *O);
extern int IsGtkSourceLanguageManagerObj (Stack stack, int i);
extern int IsGtkSourceLanguageManager(NspObject *O);
extern NspGtkSourceLanguageManager *GetGtkSourceLanguageManagerCopy (Stack stack, int i);
extern NspGtkSourceLanguageManager *GetGtkSourceLanguageManager (Stack stack, int i);

#endif /* NSP_INC_NspGtkSourceLanguageManager */ 

#ifdef NspGtkSourceLanguageManager_Private 
static int init_gtksourcelanguagemanager(NspGtkSourceLanguageManager *o,NspTypeGtkSourceLanguageManager *type);
static char *nsp_gtksourcelanguagemanager_type_as_string(void);
static char *nsp_gtksourcelanguagemanager_type_short_string(NspObject *v);
static AttrTab gtksourcelanguagemanager_attrs[];
static NspMethods *gtksourcelanguagemanager_get_methods(void);
/* static int int_gtksourcelanguagemanager_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkSourceLanguageManager_Private */
