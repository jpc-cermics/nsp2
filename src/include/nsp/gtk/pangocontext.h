/* -*- Mode: C -*- */
#ifndef NSP_INC_NspPangoContext
#define NSP_INC_NspPangoContext

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

/* NspPangoContext */

#include <nsp/gtk/gobject.h>

/*
 * NspPangoContext inherits from GObject
 * just change some type attributes 
 */

typedef NspGObject NspPangoContext ;
typedef NspTypeGObject NspTypePangoContext ;

extern int nsp_type_pangocontext_id;
extern NspTypePangoContext *nsp_type_pangocontext;

/* type instances for gobject */

NspTypePangoContext *new_type_pangocontext(type_mode mode);

/* instance for NspPangoContext */

NspPangoContext *new_pangocontext();

/*
 * Object methods redefined for pangocontext 
 */

#define NULLPANGOCONTEXT (NspPangoContext*) 0


/* from NspPangoContextObj.c */

extern NspPangoContext *nsp_pangocontext_object (NspObject *O);
extern int IsPangoContextObj (Stack stack, int i);
extern int IsPangoContext(NspObject *O);
extern NspPangoContext *GetPangoContextCopy (Stack stack, int i);
extern NspPangoContext *GetPangoContext (Stack stack, int i);

#endif /* NSP_INC_NspPangoContext */ 

#ifdef NspPangoContext_Private 
static int init_pangocontext(NspPangoContext *o,NspTypePangoContext *type);
static char *nsp_pangocontext_type_as_string(void);
static char *nsp_pangocontext_type_short_string(NspObject *v);
static AttrTab pangocontext_attrs[];
static NspMethods *pangocontext_get_methods(void);
/* static int int_pangocontext_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspPangoContext_Private */
