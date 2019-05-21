/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkCssProvider
#define NSP_INC_NspGtkCssProvider

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

/* NspGtkCssProvider */

#include <nsp/gtk/gobject.h>

/*
 * NspGtkCssProvider inherits from GObject
 * just change some type attributes 
 */

typedef NspGObject NspGtkCssProvider ;
typedef NspTypeGObject NspTypeGtkCssProvider ;

extern int nsp_type_gtkcssprovider_id;
extern NspTypeGtkCssProvider *nsp_type_gtkcssprovider;

/* type instances for gobject */

NspTypeGtkCssProvider *new_type_gtkcssprovider(type_mode mode);

/* instance for NspGtkCssProvider */

NspGtkCssProvider *new_gtkcssprovider();

/*
 * Object methods redefined for gtkcssprovider 
 */

#define NULLGTKCSSPROVIDER (NspGtkCssProvider*) 0


/* from NspGtkCssProviderObj.c */

extern NspGtkCssProvider *nsp_gtkcssprovider_object (NspObject *O);
extern int IsGtkCssProviderObj (Stack stack, int i);
extern int IsGtkCssProvider(NspObject *O);
extern NspGtkCssProvider *GetGtkCssProviderCopy (Stack stack, int i);
extern NspGtkCssProvider *GetGtkCssProvider (Stack stack, int i);

#endif /* NSP_INC_NspGtkCssProvider */ 

#ifdef NspGtkCssProvider_Private 
static int init_gtkcssprovider(NspGtkCssProvider *o,NspTypeGtkCssProvider *type);
static char *nsp_gtkcssprovider_type_as_string(void);
static char *nsp_gtkcssprovider_type_short_string(NspObject *v);
static AttrTab gtkcssprovider_attrs[];
static NspMethods *gtkcssprovider_get_methods(void);
/* static int int_gtkcssprovider_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkCssProvider_Private */
