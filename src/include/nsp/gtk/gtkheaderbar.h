/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkHeaderBar
#define NSP_INC_NspGtkHeaderBar

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

/* NspGtkHeaderBar */

#include <nsp/gtk/gtkcontainer.h>

/*
 * NspGtkHeaderBar inherits from GtkContainer
 * just change some type attributes 
 */

typedef NspGtkContainer NspGtkHeaderBar ;
typedef NspTypeGtkContainer NspTypeGtkHeaderBar ;

extern int nsp_type_gtkheaderbar_id;
extern NspTypeGtkHeaderBar *nsp_type_gtkheaderbar;

/* type instances for gtkcontainer */

NspTypeGtkHeaderBar *new_type_gtkheaderbar(type_mode mode);

/* instance for NspGtkHeaderBar */

NspGtkHeaderBar *new_gtkheaderbar();

/*
 * Object methods redefined for gtkheaderbar 
 */

#define NULLGTKHEADERBAR (NspGtkHeaderBar*) 0


/* from NspGtkHeaderBarObj.c */

extern NspGtkHeaderBar *nsp_gtkheaderbar_object (NspObject *O);
extern int IsGtkHeaderBarObj (Stack stack, int i);
extern int IsGtkHeaderBar(NspObject *O);
extern NspGtkHeaderBar *GetGtkHeaderBarCopy (Stack stack, int i);
extern NspGtkHeaderBar *GetGtkHeaderBar (Stack stack, int i);

#endif /* NSP_INC_NspGtkHeaderBar */ 

#ifdef NspGtkHeaderBar_Private 
static int init_gtkheaderbar(NspGtkHeaderBar *o,NspTypeGtkHeaderBar *type);
static char *nsp_gtkheaderbar_type_as_string(void);
static char *nsp_gtkheaderbar_type_short_string(NspObject *v);
static AttrTab gtkheaderbar_attrs[];
static NspMethods *gtkheaderbar_get_methods(void);
/* static int int_gtkheaderbar_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkHeaderBar_Private */
