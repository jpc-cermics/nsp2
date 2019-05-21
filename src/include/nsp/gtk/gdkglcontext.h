/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGdkGLContext
#define NSP_INC_NspGdkGLContext

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

/* NspGdkGLContext */

#include <nsp/gtk/gobject.h>

/*
 * NspGdkGLContext inherits from GObject
 * just change some type attributes 
 */

typedef NspGObject NspGdkGLContext ;
typedef NspTypeGObject NspTypeGdkGLContext ;

extern int nsp_type_gdkglcontext_id;
extern NspTypeGdkGLContext *nsp_type_gdkglcontext;

/* type instances for gobject */

NspTypeGdkGLContext *new_type_gdkglcontext(type_mode mode);

/* instance for NspGdkGLContext */

NspGdkGLContext *new_gdkglcontext();

/*
 * Object methods redefined for gdkglcontext 
 */

#define NULLGDKGLCONTEXT (NspGdkGLContext*) 0


/* from NspGdkGLContextObj.c */

extern NspGdkGLContext *nsp_gdkglcontext_object (NspObject *O);
extern int IsGdkGLContextObj (Stack stack, int i);
extern int IsGdkGLContext(NspObject *O);
extern NspGdkGLContext *GetGdkGLContextCopy (Stack stack, int i);
extern NspGdkGLContext *GetGdkGLContext (Stack stack, int i);

#endif /* NSP_INC_NspGdkGLContext */ 

#ifdef NspGdkGLContext_Private 
static int init_gdkglcontext(NspGdkGLContext *o,NspTypeGdkGLContext *type);
static char *nsp_gdkglcontext_type_as_string(void);
static char *nsp_gdkglcontext_type_short_string(NspObject *v);
static AttrTab gdkglcontext_attrs[];
static NspMethods *gdkglcontext_get_methods(void);
/* static int int_gdkglcontext_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGdkGLContext_Private */
