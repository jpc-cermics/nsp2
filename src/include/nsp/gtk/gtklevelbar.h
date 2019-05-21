/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkLevelBar
#define NSP_INC_NspGtkLevelBar

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

/* NspGtkLevelBar */

#include <nsp/gtk/gtkwidget.h>

/*
 * NspGtkLevelBar inherits from GtkWidget
 * just change some type attributes 
 */

typedef NspGtkWidget NspGtkLevelBar ;
typedef NspTypeGtkWidget NspTypeGtkLevelBar ;

extern int nsp_type_gtklevelbar_id;
extern NspTypeGtkLevelBar *nsp_type_gtklevelbar;

/* type instances for gtkwidget */

NspTypeGtkLevelBar *new_type_gtklevelbar(type_mode mode);

/* instance for NspGtkLevelBar */

NspGtkLevelBar *new_gtklevelbar();

/*
 * Object methods redefined for gtklevelbar 
 */

#define NULLGTKLEVELBAR (NspGtkLevelBar*) 0


/* from NspGtkLevelBarObj.c */

extern NspGtkLevelBar *nsp_gtklevelbar_object (NspObject *O);
extern int IsGtkLevelBarObj (Stack stack, int i);
extern int IsGtkLevelBar(NspObject *O);
extern NspGtkLevelBar *GetGtkLevelBarCopy (Stack stack, int i);
extern NspGtkLevelBar *GetGtkLevelBar (Stack stack, int i);

#endif /* NSP_INC_NspGtkLevelBar */ 

#ifdef NspGtkLevelBar_Private 
static int init_gtklevelbar(NspGtkLevelBar *o,NspTypeGtkLevelBar *type);
static char *nsp_gtklevelbar_type_as_string(void);
static char *nsp_gtklevelbar_type_short_string(NspObject *v);
static AttrTab gtklevelbar_attrs[];
static NspMethods *gtklevelbar_get_methods(void);
/* static int int_gtklevelbar_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkLevelBar_Private */
