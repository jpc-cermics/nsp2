/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkRcStyle
#define NSP_INC_NspGtkRcStyle

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

/* NspGtkRcStyle */

#include <nsp/gtk/gobject.h>

/*
 * NspGtkRcStyle inherits from GObject
 * just change some type attributes 
 */

typedef NspGObject NspGtkRcStyle ;
typedef NspTypeGObject NspTypeGtkRcStyle ;

extern int nsp_type_gtkrcstyle_id;
extern NspTypeGtkRcStyle *nsp_type_gtkrcstyle;

/* type instances for gobject */

NspTypeGtkRcStyle *new_type_gtkrcstyle(type_mode mode);

/* instance for NspGtkRcStyle */

NspGtkRcStyle *new_gtkrcstyle();

/*
 * Object methods redefined for gtkrcstyle 
 */

#define NULLGTKRCSTYLE (NspGtkRcStyle*) 0


/* from NspGtkRcStyleObj.c */

extern NspGtkRcStyle *nsp_gtkrcstyle_object (NspObject *O);
extern int IsGtkRcStyleObj (Stack stack, int i);
extern int IsGtkRcStyle(NspObject *O);
extern NspGtkRcStyle *GetGtkRcStyleCopy (Stack stack, int i);
extern NspGtkRcStyle *GetGtkRcStyle (Stack stack, int i);

#endif /* NSP_INC_NspGtkRcStyle */ 

#ifdef NspGtkRcStyle_Private 
static int init_gtkrcstyle(NspGtkRcStyle *o,NspTypeGtkRcStyle *type);
static char *nsp_gtkrcstyle_type_as_string(void);
static char *nsp_gtkrcstyle_type_short_string(NspObject *v);
static AttrTab gtkrcstyle_attrs[];
static NspMethods *gtkrcstyle_get_methods(void);
/* static int int_gtkrcstyle_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkRcStyle_Private */
