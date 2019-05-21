/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkInvisible
#define NSP_INC_NspGtkInvisible

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

/* NspGtkInvisible */

#include <nsp/gtk/gtkwidget.h>

/*
 * NspGtkInvisible inherits from GtkWidget
 * just change some type attributes 
 */

typedef NspGtkWidget NspGtkInvisible ;
typedef NspTypeGtkWidget NspTypeGtkInvisible ;

extern int nsp_type_gtkinvisible_id;
extern NspTypeGtkInvisible *nsp_type_gtkinvisible;

/* type instances for gtkwidget */

NspTypeGtkInvisible *new_type_gtkinvisible(type_mode mode);

/* instance for NspGtkInvisible */

NspGtkInvisible *new_gtkinvisible();

/*
 * Object methods redefined for gtkinvisible 
 */

#define NULLGTKINVISIBLE (NspGtkInvisible*) 0


/* from NspGtkInvisibleObj.c */

extern NspGtkInvisible *nsp_gtkinvisible_object (NspObject *O);
extern int IsGtkInvisibleObj (Stack stack, int i);
extern int IsGtkInvisible(NspObject *O);
extern NspGtkInvisible *GetGtkInvisibleCopy (Stack stack, int i);
extern NspGtkInvisible *GetGtkInvisible (Stack stack, int i);

#endif /* NSP_INC_NspGtkInvisible */ 

#ifdef NspGtkInvisible_Private 
static int init_gtkinvisible(NspGtkInvisible *o,NspTypeGtkInvisible *type);
static char *nsp_gtkinvisible_type_as_string(void);
static char *nsp_gtkinvisible_type_short_string(NspObject *v);
static AttrTab gtkinvisible_attrs[];
static NspMethods *gtkinvisible_get_methods(void);
/* static int int_gtkinvisible_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkInvisible_Private */
