/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkIMContextSimple
#define NSP_INC_NspGtkIMContextSimple

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

/* NspGtkIMContextSimple */

#include <nsp/gtk/gtkimcontext.h>

/*
 * NspGtkIMContextSimple inherits from GtkIMContext
 * just change some type attributes 
 */

typedef NspGtkIMContext NspGtkIMContextSimple ;
typedef NspTypeGtkIMContext NspTypeGtkIMContextSimple ;

extern int nsp_type_gtkimcontextsimple_id;
extern NspTypeGtkIMContextSimple *nsp_type_gtkimcontextsimple;

/* type instances for gtkimcontext */

NspTypeGtkIMContextSimple *new_type_gtkimcontextsimple(type_mode mode);

/* instance for NspGtkIMContextSimple */

NspGtkIMContextSimple *new_gtkimcontextsimple();

/*
 * Object methods redefined for gtkimcontextsimple 
 */

#define NULLGTKIMCONTEXTSIMPLE (NspGtkIMContextSimple*) 0


/* from NspGtkIMContextSimpleObj.c */

extern NspGtkIMContextSimple *nsp_gtkimcontextsimple_object (NspObject *O);
extern int IsGtkIMContextSimpleObj (Stack stack, int i);
extern int IsGtkIMContextSimple(NspObject *O);
extern NspGtkIMContextSimple *GetGtkIMContextSimpleCopy (Stack stack, int i);
extern NspGtkIMContextSimple *GetGtkIMContextSimple (Stack stack, int i);

#endif /* NSP_INC_NspGtkIMContextSimple */ 

#ifdef NspGtkIMContextSimple_Private 
static int init_gtkimcontextsimple(NspGtkIMContextSimple *o,NspTypeGtkIMContextSimple *type);
static char *nsp_gtkimcontextsimple_type_as_string(void);
static char *nsp_gtkimcontextsimple_type_short_string(NspObject *v);
static AttrTab gtkimcontextsimple_attrs[];
static NspMethods *gtkimcontextsimple_get_methods(void);
/* static int int_gtkimcontextsimple_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkIMContextSimple_Private */
