/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGAsyncInitable
#define NSP_INC_NspGAsyncInitable

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

/* NspGAsyncInitable */

#include <nsp/gtk/gobject.h>

/*
 * NspGAsyncInitable inherits from GObject
 * just change some type attributes 
 */

typedef NspGObject NspGAsyncInitable ;
typedef NspTypeGObject NspTypeGAsyncInitable ;

extern int nsp_type_gasyncinitable_id;
extern NspTypeGAsyncInitable *nsp_type_gasyncinitable;

/* type instances for gobject */

NspTypeGAsyncInitable *new_type_gasyncinitable(type_mode mode);

/* instance for NspGAsyncInitable */

NspGAsyncInitable *new_gasyncinitable();

/*
 * Object methods redefined for gasyncinitable 
 */

#define NULLGASYNCINITABLE (NspGAsyncInitable*) 0


/* from NspGAsyncInitableObj.c */

extern NspGAsyncInitable *nsp_gasyncinitable_object (NspObject *O);
extern int IsGAsyncInitableObj (Stack stack, int i);
extern int IsGAsyncInitable(NspObject *O);
extern NspGAsyncInitable *GetGAsyncInitableCopy (Stack stack, int i);
extern NspGAsyncInitable *GetGAsyncInitable (Stack stack, int i);

#endif /* NSP_INC_NspGAsyncInitable */ 

#ifdef NspGAsyncInitable_Private 
static int init_gasyncinitable(NspGAsyncInitable *o,NspTypeGAsyncInitable *type);
static char *nsp_gasyncinitable_type_as_string(void);
static char *nsp_gasyncinitable_type_short_string(NspObject *v);
static AttrTab gasyncinitable_attrs[];
static NspMethods *gasyncinitable_get_methods(void);
/* static int int_gasyncinitable_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGAsyncInitable_Private */
