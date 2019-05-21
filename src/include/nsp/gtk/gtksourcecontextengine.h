/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkSourceContextEngine
#define NSP_INC_NspGtkSourceContextEngine

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

/* NspGtkSourceContextEngine */

#include <nsp/gtk/gobject.h>

/*
 * NspGtkSourceContextEngine inherits from GObject
 * just change some type attributes 
 */

typedef NspGObject NspGtkSourceContextEngine ;
typedef NspTypeGObject NspTypeGtkSourceContextEngine ;

extern int nsp_type_gtksourcecontextengine_id;
extern NspTypeGtkSourceContextEngine *nsp_type_gtksourcecontextengine;

/* type instances for gobject */

NspTypeGtkSourceContextEngine *new_type_gtksourcecontextengine(type_mode mode);

/* instance for NspGtkSourceContextEngine */

NspGtkSourceContextEngine *new_gtksourcecontextengine();

/*
 * Object methods redefined for gtksourcecontextengine 
 */

#define NULLGTKSOURCECONTEXTENGINE (NspGtkSourceContextEngine*) 0


/* from NspGtkSourceContextEngineObj.c */

extern NspGtkSourceContextEngine *nsp_gtksourcecontextengine_object (NspObject *O);
extern int IsGtkSourceContextEngineObj (Stack stack, int i);
extern int IsGtkSourceContextEngine(NspObject *O);
extern NspGtkSourceContextEngine *GetGtkSourceContextEngineCopy (Stack stack, int i);
extern NspGtkSourceContextEngine *GetGtkSourceContextEngine (Stack stack, int i);

#endif /* NSP_INC_NspGtkSourceContextEngine */ 

#ifdef NspGtkSourceContextEngine_Private 
static int init_gtksourcecontextengine(NspGtkSourceContextEngine *o,NspTypeGtkSourceContextEngine *type);
static char *nsp_gtksourcecontextengine_type_as_string(void);
static char *nsp_gtksourcecontextengine_type_short_string(NspObject *v);
static AttrTab gtksourcecontextengine_attrs[];
static NspMethods *gtksourcecontextengine_get_methods(void);
/* static int int_gtksourcecontextengine_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkSourceContextEngine_Private */
