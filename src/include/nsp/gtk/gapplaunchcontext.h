/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGAppLaunchContext
#define NSP_INC_NspGAppLaunchContext

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

/* NspGAppLaunchContext */

#include <nsp/gtk/gobject.h>

/*
 * NspGAppLaunchContext inherits from GObject
 * just change some type attributes 
 */

typedef NspGObject NspGAppLaunchContext ;
typedef NspTypeGObject NspTypeGAppLaunchContext ;

extern int nsp_type_gapplaunchcontext_id;
extern NspTypeGAppLaunchContext *nsp_type_gapplaunchcontext;

/* type instances for gobject */

NspTypeGAppLaunchContext *new_type_gapplaunchcontext(type_mode mode);

/* instance for NspGAppLaunchContext */

NspGAppLaunchContext *new_gapplaunchcontext();

/*
 * Object methods redefined for gapplaunchcontext 
 */

#define NULLGAPPLAUNCHCONTEXT (NspGAppLaunchContext*) 0


/* from NspGAppLaunchContextObj.c */

extern NspGAppLaunchContext *nsp_gapplaunchcontext_object (NspObject *O);
extern int IsGAppLaunchContextObj (Stack stack, int i);
extern int IsGAppLaunchContext(NspObject *O);
extern NspGAppLaunchContext *GetGAppLaunchContextCopy (Stack stack, int i);
extern NspGAppLaunchContext *GetGAppLaunchContext (Stack stack, int i);

#endif /* NSP_INC_NspGAppLaunchContext */ 

#ifdef NspGAppLaunchContext_Private 
static int init_gapplaunchcontext(NspGAppLaunchContext *o,NspTypeGAppLaunchContext *type);
static char *nsp_gapplaunchcontext_type_as_string(void);
static char *nsp_gapplaunchcontext_type_short_string(NspObject *v);
static AttrTab gapplaunchcontext_attrs[];
static NspMethods *gapplaunchcontext_get_methods(void);
/* static int int_gapplaunchcontext_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGAppLaunchContext_Private */
