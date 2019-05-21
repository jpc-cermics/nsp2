/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkStyleContext
#define NSP_INC_NspGtkStyleContext

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

/* NspGtkStyleContext */

#include <nsp/gtk/gobject.h>

/*
 * NspGtkStyleContext inherits from GObject
 * just change some type attributes 
 */

typedef NspGObject NspGtkStyleContext ;
typedef NspTypeGObject NspTypeGtkStyleContext ;

extern int nsp_type_gtkstylecontext_id;
extern NspTypeGtkStyleContext *nsp_type_gtkstylecontext;

/* type instances for gobject */

NspTypeGtkStyleContext *new_type_gtkstylecontext(type_mode mode);

/* instance for NspGtkStyleContext */

NspGtkStyleContext *new_gtkstylecontext();

/*
 * Object methods redefined for gtkstylecontext 
 */

#define NULLGTKSTYLECONTEXT (NspGtkStyleContext*) 0


/* from NspGtkStyleContextObj.c */

extern NspGtkStyleContext *nsp_gtkstylecontext_object (NspObject *O);
extern int IsGtkStyleContextObj (Stack stack, int i);
extern int IsGtkStyleContext(NspObject *O);
extern NspGtkStyleContext *GetGtkStyleContextCopy (Stack stack, int i);
extern NspGtkStyleContext *GetGtkStyleContext (Stack stack, int i);

#endif /* NSP_INC_NspGtkStyleContext */ 

#ifdef NspGtkStyleContext_Private 
static int init_gtkstylecontext(NspGtkStyleContext *o,NspTypeGtkStyleContext *type);
static char *nsp_gtkstylecontext_type_as_string(void);
static char *nsp_gtkstylecontext_type_short_string(NspObject *v);
static AttrTab gtkstylecontext_attrs[];
static NspMethods *gtkstylecontext_get_methods(void);
/* static int int_gtkstylecontext_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkStyleContext_Private */
