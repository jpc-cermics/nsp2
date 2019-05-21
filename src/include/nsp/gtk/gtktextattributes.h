/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkTextAttributes
#define NSP_INC_NspGtkTextAttributes

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

/* NspGtkTextAttributes */

#include <nsp/gtk/gboxed.h>

/*
 * NspGtkTextAttributes inherits from GBoxed
 * just change some type attributes 
 */

typedef NspGBoxed NspGtkTextAttributes ;
typedef NspTypeGBoxed NspTypeGtkTextAttributes ;

extern int nsp_type_gtktextattributes_id;
extern NspTypeGtkTextAttributes *nsp_type_gtktextattributes;

/* type instances for gboxed */

NspTypeGtkTextAttributes *new_type_gtktextattributes(type_mode mode);

/* instance for NspGtkTextAttributes */

NspGtkTextAttributes *new_gtktextattributes();

/*
 * Object methods redefined for gtktextattributes 
 */

#define NULLGTKTEXTATTRIBUTES (NspGtkTextAttributes*) 0


/* from NspGtkTextAttributesObj.c */

extern NspGtkTextAttributes *nsp_gtktextattributes_object (NspObject *O);
extern int IsGtkTextAttributesObj (Stack stack, int i);
extern int IsGtkTextAttributes(NspObject *O);
extern NspGtkTextAttributes *GetGtkTextAttributesCopy (Stack stack, int i);
extern NspGtkTextAttributes *GetGtkTextAttributes (Stack stack, int i);

#endif /* NSP_INC_NspGtkTextAttributes */ 

#ifdef NspGtkTextAttributes_Private 
static int init_gtktextattributes(NspGtkTextAttributes *o,NspTypeGtkTextAttributes *type);
static char *nsp_gtktextattributes_type_as_string(void);
static char *nsp_gtktextattributes_type_short_string(NspObject *v);
static AttrTab gtktextattributes_attrs[];
static NspMethods *gtktextattributes_get_methods(void);
/* static int int_gtktextattributes_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkTextAttributes_Private */
