/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkIMContext
#define NSP_INC_NspGtkIMContext

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

/* NspGtkIMContext */

#include <nsp/gtk/gobject.h>

/*
 * NspGtkIMContext inherits from GObject
 * just change some type attributes 
 */

typedef NspGObject NspGtkIMContext ;
typedef NspTypeGObject NspTypeGtkIMContext ;

extern int nsp_type_gtkimcontext_id;
extern NspTypeGtkIMContext *nsp_type_gtkimcontext;

/* type instances for gobject */

NspTypeGtkIMContext *new_type_gtkimcontext(type_mode mode);

/* instance for NspGtkIMContext */

NspGtkIMContext *new_gtkimcontext();

/*
 * Object methods redefined for gtkimcontext 
 */

#define NULLGTKIMCONTEXT (NspGtkIMContext*) 0


/* from NspGtkIMContextObj.c */

extern NspGtkIMContext *nsp_gtkimcontext_object (NspObject *O);
extern int IsGtkIMContextObj (Stack stack, int i);
extern int IsGtkIMContext(NspObject *O);
extern NspGtkIMContext *GetGtkIMContextCopy (Stack stack, int i);
extern NspGtkIMContext *GetGtkIMContext (Stack stack, int i);

#endif /* NSP_INC_NspGtkIMContext */ 

#ifdef NspGtkIMContext_Private 
static int init_gtkimcontext(NspGtkIMContext *o,NspTypeGtkIMContext *type);
static char *nsp_gtkimcontext_type_as_string(void);
static char *nsp_gtkimcontext_type_short_string(NspObject *v);
static AttrTab gtkimcontext_attrs[];
static NspMethods *gtkimcontext_get_methods(void);
/* static int int_gtkimcontext_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkIMContext_Private */
