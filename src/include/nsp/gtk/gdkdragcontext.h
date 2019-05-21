/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGdkDragContext
#define NSP_INC_NspGdkDragContext

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

/* NspGdkDragContext */

#include <nsp/gtk/gobject.h>

/*
 * NspGdkDragContext inherits from GObject
 * just change some type attributes 
 */

typedef NspGObject NspGdkDragContext ;
typedef NspTypeGObject NspTypeGdkDragContext ;

extern int nsp_type_gdkdragcontext_id;
extern NspTypeGdkDragContext *nsp_type_gdkdragcontext;

/* type instances for gobject */

NspTypeGdkDragContext *new_type_gdkdragcontext(type_mode mode);

/* instance for NspGdkDragContext */

NspGdkDragContext *new_gdkdragcontext();

/*
 * Object methods redefined for gdkdragcontext 
 */

#define NULLGDKDRAGCONTEXT (NspGdkDragContext*) 0


/* from NspGdkDragContextObj.c */

extern NspGdkDragContext *nsp_gdkdragcontext_object (NspObject *O);
extern int IsGdkDragContextObj (Stack stack, int i);
extern int IsGdkDragContext(NspObject *O);
extern NspGdkDragContext *GetGdkDragContextCopy (Stack stack, int i);
extern NspGdkDragContext *GetGdkDragContext (Stack stack, int i);

#endif /* NSP_INC_NspGdkDragContext */ 

#ifdef NspGdkDragContext_Private 
static int init_gdkdragcontext(NspGdkDragContext *o,NspTypeGdkDragContext *type);
static char *nsp_gdkdragcontext_type_as_string(void);
static char *nsp_gdkdragcontext_type_short_string(NspObject *v);
static AttrTab gdkdragcontext_attrs[];
static NspMethods *gdkdragcontext_get_methods(void);
/* static int int_gdkdragcontext_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGdkDragContext_Private */
