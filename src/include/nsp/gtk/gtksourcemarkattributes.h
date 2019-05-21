/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkSourceMarkAttributes
#define NSP_INC_NspGtkSourceMarkAttributes

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

/* NspGtkSourceMarkAttributes */

#include <nsp/gtk/gobject.h>

/*
 * NspGtkSourceMarkAttributes inherits from GObject
 * just change some type attributes 
 */

typedef NspGObject NspGtkSourceMarkAttributes ;
typedef NspTypeGObject NspTypeGtkSourceMarkAttributes ;

extern int nsp_type_gtksourcemarkattributes_id;
extern NspTypeGtkSourceMarkAttributes *nsp_type_gtksourcemarkattributes;

/* type instances for gobject */

NspTypeGtkSourceMarkAttributes *new_type_gtksourcemarkattributes(type_mode mode);

/* instance for NspGtkSourceMarkAttributes */

NspGtkSourceMarkAttributes *new_gtksourcemarkattributes();

/*
 * Object methods redefined for gtksourcemarkattributes 
 */

#define NULLGTKSOURCEMARKATTRIBUTES (NspGtkSourceMarkAttributes*) 0


/* from NspGtkSourceMarkAttributesObj.c */

extern NspGtkSourceMarkAttributes *nsp_gtksourcemarkattributes_object (NspObject *O);
extern int IsGtkSourceMarkAttributesObj (Stack stack, int i);
extern int IsGtkSourceMarkAttributes(NspObject *O);
extern NspGtkSourceMarkAttributes *GetGtkSourceMarkAttributesCopy (Stack stack, int i);
extern NspGtkSourceMarkAttributes *GetGtkSourceMarkAttributes (Stack stack, int i);

#endif /* NSP_INC_NspGtkSourceMarkAttributes */ 

#ifdef NspGtkSourceMarkAttributes_Private 
static int init_gtksourcemarkattributes(NspGtkSourceMarkAttributes *o,NspTypeGtkSourceMarkAttributes *type);
static char *nsp_gtksourcemarkattributes_type_as_string(void);
static char *nsp_gtksourcemarkattributes_type_short_string(NspObject *v);
static AttrTab gtksourcemarkattributes_attrs[];
static NspMethods *gtksourcemarkattributes_get_methods(void);
/* static int int_gtksourcemarkattributes_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkSourceMarkAttributes_Private */
