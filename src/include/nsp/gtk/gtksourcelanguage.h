/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkSourceLanguage
#define NSP_INC_NspGtkSourceLanguage

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

/* NspGtkSourceLanguage */

#include <nsp/gtk/gobject.h>

/*
 * NspGtkSourceLanguage inherits from GObject
 * just change some type attributes 
 */

typedef NspGObject NspGtkSourceLanguage ;
typedef NspTypeGObject NspTypeGtkSourceLanguage ;

extern int nsp_type_gtksourcelanguage_id;
extern NspTypeGtkSourceLanguage *nsp_type_gtksourcelanguage;

/* type instances for gobject */

NspTypeGtkSourceLanguage *new_type_gtksourcelanguage(type_mode mode);

/* instance for NspGtkSourceLanguage */

NspGtkSourceLanguage *new_gtksourcelanguage();

/*
 * Object methods redefined for gtksourcelanguage 
 */

#define NULLGTKSOURCELANGUAGE (NspGtkSourceLanguage*) 0


/* from NspGtkSourceLanguageObj.c */

extern NspGtkSourceLanguage *nsp_gtksourcelanguage_object (NspObject *O);
extern int IsGtkSourceLanguageObj (Stack stack, int i);
extern int IsGtkSourceLanguage(NspObject *O);
extern NspGtkSourceLanguage *GetGtkSourceLanguageCopy (Stack stack, int i);
extern NspGtkSourceLanguage *GetGtkSourceLanguage (Stack stack, int i);

#endif /* NSP_INC_NspGtkSourceLanguage */ 

#ifdef NspGtkSourceLanguage_Private 
static int init_gtksourcelanguage(NspGtkSourceLanguage *o,NspTypeGtkSourceLanguage *type);
static char *nsp_gtksourcelanguage_type_as_string(void);
static char *nsp_gtksourcelanguage_type_short_string(NspObject *v);
static AttrTab gtksourcelanguage_attrs[];
static NspMethods *gtksourcelanguage_get_methods(void);
/* static int int_gtksourcelanguage_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkSourceLanguage_Private */
