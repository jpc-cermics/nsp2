/* -*- Mode: C -*- */
#ifndef NSP_INC_NspAtkSelection
#define NSP_INC_NspAtkSelection

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

/* NspAtkSelection */

#include <nsp/gtk/gobject.h>

/*
 * NspAtkSelection inherits from GObject
 * just change some type attributes 
 */

typedef NspGObject NspAtkSelection ;
typedef NspTypeGObject NspTypeAtkSelection ;

extern int nsp_type_atkselection_id;
extern NspTypeAtkSelection *nsp_type_atkselection;

/* type instances for gobject */

NspTypeAtkSelection *new_type_atkselection(type_mode mode);

/* instance for NspAtkSelection */

NspAtkSelection *new_atkselection();

/*
 * Object methods redefined for atkselection 
 */

#define NULLATKSELECTION (NspAtkSelection*) 0


/* from NspAtkSelectionObj.c */

extern NspAtkSelection *nsp_atkselection_object (NspObject *O);
extern int IsAtkSelectionObj (Stack stack, int i);
extern int IsAtkSelection(NspObject *O);
extern NspAtkSelection *GetAtkSelectionCopy (Stack stack, int i);
extern NspAtkSelection *GetAtkSelection (Stack stack, int i);

#endif /* NSP_INC_NspAtkSelection */ 

#ifdef NspAtkSelection_Private 
static int init_atkselection(NspAtkSelection *o,NspTypeAtkSelection *type);
static char *nsp_atkselection_type_as_string(void);
static char *nsp_atkselection_type_short_string(NspObject *v);
static AttrTab atkselection_attrs[];
static NspMethods *atkselection_get_methods(void);
/* static int int_atkselection_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspAtkSelection_Private */
