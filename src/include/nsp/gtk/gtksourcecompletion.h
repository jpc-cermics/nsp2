/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkSourceCompletion
#define NSP_INC_NspGtkSourceCompletion

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

/* NspGtkSourceCompletion */

#include <nsp/gtk/gobject.h>

/*
 * NspGtkSourceCompletion inherits from GObject
 * just change some type attributes 
 */

typedef NspGObject NspGtkSourceCompletion ;
typedef NspTypeGObject NspTypeGtkSourceCompletion ;

extern int nsp_type_gtksourcecompletion_id;
extern NspTypeGtkSourceCompletion *nsp_type_gtksourcecompletion;

/* type instances for gobject */

NspTypeGtkSourceCompletion *new_type_gtksourcecompletion(type_mode mode);

/* instance for NspGtkSourceCompletion */

NspGtkSourceCompletion *new_gtksourcecompletion();

/*
 * Object methods redefined for gtksourcecompletion 
 */

#define NULLGTKSOURCECOMPLETION (NspGtkSourceCompletion*) 0


/* from NspGtkSourceCompletionObj.c */

extern NspGtkSourceCompletion *nsp_gtksourcecompletion_object (NspObject *O);
extern int IsGtkSourceCompletionObj (Stack stack, int i);
extern int IsGtkSourceCompletion(NspObject *O);
extern NspGtkSourceCompletion *GetGtkSourceCompletionCopy (Stack stack, int i);
extern NspGtkSourceCompletion *GetGtkSourceCompletion (Stack stack, int i);

#endif /* NSP_INC_NspGtkSourceCompletion */ 

#ifdef NspGtkSourceCompletion_Private 
static int init_gtksourcecompletion(NspGtkSourceCompletion *o,NspTypeGtkSourceCompletion *type);
static char *nsp_gtksourcecompletion_type_as_string(void);
static char *nsp_gtksourcecompletion_type_short_string(NspObject *v);
static AttrTab gtksourcecompletion_attrs[];
static NspMethods *gtksourcecompletion_get_methods(void);
/* static int int_gtksourcecompletion_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkSourceCompletion_Private */
