/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkStyleProperty
#define NSP_INC_NspGtkStyleProperty

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

/* NspGtkStyleProperty */

#include <nsp/gtk/gobject.h>

/*
 * NspGtkStyleProperty inherits from GObject
 * just change some type attributes 
 */

typedef NspGObject NspGtkStyleProperty ;
typedef NspTypeGObject NspTypeGtkStyleProperty ;

extern int nsp_type_gtkstyleproperty_id;
extern NspTypeGtkStyleProperty *nsp_type_gtkstyleproperty;

/* type instances for gobject */

NspTypeGtkStyleProperty *new_type_gtkstyleproperty(type_mode mode);

/* instance for NspGtkStyleProperty */

NspGtkStyleProperty *new_gtkstyleproperty();

/*
 * Object methods redefined for gtkstyleproperty 
 */

#define NULLGTKSTYLEPROPERTY (NspGtkStyleProperty*) 0


/* from NspGtkStylePropertyObj.c */

extern NspGtkStyleProperty *nsp_gtkstyleproperty_object (NspObject *O);
extern int IsGtkStylePropertyObj (Stack stack, int i);
extern int IsGtkStyleProperty(NspObject *O);
extern NspGtkStyleProperty *GetGtkStylePropertyCopy (Stack stack, int i);
extern NspGtkStyleProperty *GetGtkStyleProperty (Stack stack, int i);

#endif /* NSP_INC_NspGtkStyleProperty */ 

#ifdef NspGtkStyleProperty_Private 
static int init_gtkstyleproperty(NspGtkStyleProperty *o,NspTypeGtkStyleProperty *type);
static char *nsp_gtkstyleproperty_type_as_string(void);
static char *nsp_gtkstyleproperty_type_short_string(NspObject *v);
static AttrTab gtkstyleproperty_attrs[];
static NspMethods *gtkstyleproperty_get_methods(void);
/* static int int_gtkstyleproperty_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkStyleProperty_Private */
