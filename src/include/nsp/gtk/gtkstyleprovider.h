/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkStyleProvider
#define NSP_INC_NspGtkStyleProvider

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

/* NspGtkStyleProvider */

#include <nsp/gtk/gobject.h>

/*
 * NspGtkStyleProvider inherits from GObject
 * just change some type attributes 
 */

typedef NspGObject NspGtkStyleProvider ;
typedef NspTypeGObject NspTypeGtkStyleProvider ;

extern int nsp_type_gtkstyleprovider_id;
extern NspTypeGtkStyleProvider *nsp_type_gtkstyleprovider;

/* type instances for gobject */

NspTypeGtkStyleProvider *new_type_gtkstyleprovider(type_mode mode);

/* instance for NspGtkStyleProvider */

NspGtkStyleProvider *new_gtkstyleprovider();

/*
 * Object methods redefined for gtkstyleprovider 
 */

#define NULLGTKSTYLEPROVIDER (NspGtkStyleProvider*) 0


/* from NspGtkStyleProviderObj.c */

extern NspGtkStyleProvider *nsp_gtkstyleprovider_object (NspObject *O);
extern int IsGtkStyleProviderObj (Stack stack, int i);
extern int IsGtkStyleProvider(NspObject *O);
extern NspGtkStyleProvider *GetGtkStyleProviderCopy (Stack stack, int i);
extern NspGtkStyleProvider *GetGtkStyleProvider (Stack stack, int i);

#endif /* NSP_INC_NspGtkStyleProvider */ 

#ifdef NspGtkStyleProvider_Private 
static int init_gtkstyleprovider(NspGtkStyleProvider *o,NspTypeGtkStyleProvider *type);
static char *nsp_gtkstyleprovider_type_as_string(void);
static char *nsp_gtkstyleprovider_type_short_string(NspObject *v);
static AttrTab gtkstyleprovider_attrs[];
static NspMethods *gtkstyleprovider_get_methods(void);
/* static int int_gtkstyleprovider_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkStyleProvider_Private */
