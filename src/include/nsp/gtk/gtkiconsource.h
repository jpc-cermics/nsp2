/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkIconSource
#define NSP_INC_NspGtkIconSource

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

/* NspGtkIconSource */

#include <nsp/gtk/gboxed.h>

/*
 * NspGtkIconSource inherits from GBoxed
 * just change some type attributes 
 */

typedef NspGBoxed NspGtkIconSource ;
typedef NspTypeGBoxed NspTypeGtkIconSource ;

extern int nsp_type_gtkiconsource_id;
extern NspTypeGtkIconSource *nsp_type_gtkiconsource;

/* type instances for gboxed */

NspTypeGtkIconSource *new_type_gtkiconsource(type_mode mode);

/* instance for NspGtkIconSource */

NspGtkIconSource *new_gtkiconsource();

/*
 * Object methods redefined for gtkiconsource 
 */

#define NULLGTKICONSOURCE (NspGtkIconSource*) 0


/* from NspGtkIconSourceObj.c */

extern NspGtkIconSource *nsp_gtkiconsource_object (NspObject *O);
extern int IsGtkIconSourceObj (Stack stack, int i);
extern int IsGtkIconSource(NspObject *O);
extern NspGtkIconSource *GetGtkIconSourceCopy (Stack stack, int i);
extern NspGtkIconSource *GetGtkIconSource (Stack stack, int i);

#endif /* NSP_INC_NspGtkIconSource */ 

#ifdef NspGtkIconSource_Private 
static int init_gtkiconsource(NspGtkIconSource *o,NspTypeGtkIconSource *type);
static char *nsp_gtkiconsource_type_as_string(void);
static char *nsp_gtkiconsource_type_short_string(NspObject *v);
static AttrTab gtkiconsource_attrs[];
static NspMethods *gtkiconsource_get_methods(void);
/* static int int_gtkiconsource_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkIconSource_Private */
