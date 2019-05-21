/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkCssStyle
#define NSP_INC_NspGtkCssStyle

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

/* NspGtkCssStyle */

#include <nsp/gtk/gobject.h>

/*
 * NspGtkCssStyle inherits from GObject
 * just change some type attributes 
 */

typedef NspGObject NspGtkCssStyle ;
typedef NspTypeGObject NspTypeGtkCssStyle ;

extern int nsp_type_gtkcssstyle_id;
extern NspTypeGtkCssStyle *nsp_type_gtkcssstyle;

/* type instances for gobject */

NspTypeGtkCssStyle *new_type_gtkcssstyle(type_mode mode);

/* instance for NspGtkCssStyle */

NspGtkCssStyle *new_gtkcssstyle();

/*
 * Object methods redefined for gtkcssstyle 
 */

#define NULLGTKCSSSTYLE (NspGtkCssStyle*) 0


/* from NspGtkCssStyleObj.c */

extern NspGtkCssStyle *nsp_gtkcssstyle_object (NspObject *O);
extern int IsGtkCssStyleObj (Stack stack, int i);
extern int IsGtkCssStyle(NspObject *O);
extern NspGtkCssStyle *GetGtkCssStyleCopy (Stack stack, int i);
extern NspGtkCssStyle *GetGtkCssStyle (Stack stack, int i);

#endif /* NSP_INC_NspGtkCssStyle */ 

#ifdef NspGtkCssStyle_Private 
static int init_gtkcssstyle(NspGtkCssStyle *o,NspTypeGtkCssStyle *type);
static char *nsp_gtkcssstyle_type_as_string(void);
static char *nsp_gtkcssstyle_type_short_string(NspObject *v);
static AttrTab gtkcssstyle_attrs[];
static NspMethods *gtkcssstyle_get_methods(void);
/* static int int_gtkcssstyle_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkCssStyle_Private */
