/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkLayout
#define NSP_INC_NspGtkLayout

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

/* NspGtkLayout */

#include <nsp/gtk/gtkcontainer.h>

/*
 * NspGtkLayout inherits from GtkContainer
 * just change some type attributes 
 */

typedef NspGtkContainer NspGtkLayout ;
typedef NspTypeGtkContainer NspTypeGtkLayout ;

extern int nsp_type_gtklayout_id;
extern NspTypeGtkLayout *nsp_type_gtklayout;

/* type instances for gtkcontainer */

NspTypeGtkLayout *new_type_gtklayout(type_mode mode);

/* instance for NspGtkLayout */

NspGtkLayout *new_gtklayout();

/*
 * Object methods redefined for gtklayout 
 */

#define NULLGTKLAYOUT (NspGtkLayout*) 0


/* from NspGtkLayoutObj.c */

extern NspGtkLayout *nsp_gtklayout_object (NspObject *O);
extern int IsGtkLayoutObj (Stack stack, int i);
extern int IsGtkLayout(NspObject *O);
extern NspGtkLayout *GetGtkLayoutCopy (Stack stack, int i);
extern NspGtkLayout *GetGtkLayout (Stack stack, int i);

#endif /* NSP_INC_NspGtkLayout */ 

#ifdef NspGtkLayout_Private 
static int init_gtklayout(NspGtkLayout *o,NspTypeGtkLayout *type);
static char *nsp_gtklayout_type_as_string(void);
static char *nsp_gtklayout_type_short_string(NspObject *v);
static AttrTab gtklayout_attrs[];
static NspMethods *gtklayout_get_methods(void);
/* static int int_gtklayout_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkLayout_Private */
