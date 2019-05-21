/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGApplication
#define NSP_INC_NspGApplication

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

/* NspGApplication */

#include <nsp/gtk/gobject.h>

/*
 * NspGApplication inherits from GObject
 * just change some type attributes 
 */

typedef NspGObject NspGApplication ;
typedef NspTypeGObject NspTypeGApplication ;

extern int nsp_type_gapplication_id;
extern NspTypeGApplication *nsp_type_gapplication;

/* type instances for gobject */

NspTypeGApplication *new_type_gapplication(type_mode mode);

/* instance for NspGApplication */

NspGApplication *new_gapplication();

/*
 * Object methods redefined for gapplication 
 */

#define NULLGAPPLICATION (NspGApplication*) 0


/* from NspGApplicationObj.c */

extern NspGApplication *nsp_gapplication_object (NspObject *O);
extern int IsGApplicationObj (Stack stack, int i);
extern int IsGApplication(NspObject *O);
extern NspGApplication *GetGApplicationCopy (Stack stack, int i);
extern NspGApplication *GetGApplication (Stack stack, int i);

#endif /* NSP_INC_NspGApplication */ 

#ifdef NspGApplication_Private 
static int init_gapplication(NspGApplication *o,NspTypeGApplication *type);
static char *nsp_gapplication_type_as_string(void);
static char *nsp_gapplication_type_short_string(NspObject *v);
static AttrTab gapplication_attrs[];
static NspMethods *gapplication_get_methods(void);
/* static int int_gapplication_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGApplication_Private */
