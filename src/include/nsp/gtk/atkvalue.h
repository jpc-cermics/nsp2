/* -*- Mode: C -*- */
#ifndef NSP_INC_NspAtkValue
#define NSP_INC_NspAtkValue

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

/* NspAtkValue */

#include <nsp/gtk/gobject.h>

/*
 * NspAtkValue inherits from GObject
 * just change some type attributes 
 */

typedef NspGObject NspAtkValue ;
typedef NspTypeGObject NspTypeAtkValue ;

extern int nsp_type_atkvalue_id;
extern NspTypeAtkValue *nsp_type_atkvalue;

/* type instances for gobject */

NspTypeAtkValue *new_type_atkvalue(type_mode mode);

/* instance for NspAtkValue */

NspAtkValue *new_atkvalue();

/*
 * Object methods redefined for atkvalue 
 */

#define NULLATKVALUE (NspAtkValue*) 0


/* from NspAtkValueObj.c */

extern NspAtkValue *nsp_atkvalue_object (NspObject *O);
extern int IsAtkValueObj (Stack stack, int i);
extern int IsAtkValue(NspObject *O);
extern NspAtkValue *GetAtkValueCopy (Stack stack, int i);
extern NspAtkValue *GetAtkValue (Stack stack, int i);

#endif /* NSP_INC_NspAtkValue */ 

#ifdef NspAtkValue_Private 
static int init_atkvalue(NspAtkValue *o,NspTypeAtkValue *type);
static char *nsp_atkvalue_type_as_string(void);
static char *nsp_atkvalue_type_short_string(NspObject *v);
static AttrTab atkvalue_attrs[];
static NspMethods *atkvalue_get_methods(void);
/* static int int_atkvalue_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspAtkValue_Private */
