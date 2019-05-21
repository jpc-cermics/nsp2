/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkCellRendererAccel
#define NSP_INC_NspGtkCellRendererAccel

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

/* NspGtkCellRendererAccel */

#include <nsp/gtk/gtkcellrenderertext.h>

/*
 * NspGtkCellRendererAccel inherits from GtkCellRendererText
 * just change some type attributes 
 */

typedef NspGtkCellRendererText NspGtkCellRendererAccel ;
typedef NspTypeGtkCellRendererText NspTypeGtkCellRendererAccel ;

extern int nsp_type_gtkcellrendereraccel_id;
extern NspTypeGtkCellRendererAccel *nsp_type_gtkcellrendereraccel;

/* type instances for gtkcellrenderertext */

NspTypeGtkCellRendererAccel *new_type_gtkcellrendereraccel(type_mode mode);

/* instance for NspGtkCellRendererAccel */

NspGtkCellRendererAccel *new_gtkcellrendereraccel();

/*
 * Object methods redefined for gtkcellrendereraccel 
 */

#define NULLGTKCELLRENDERERACCEL (NspGtkCellRendererAccel*) 0


/* from NspGtkCellRendererAccelObj.c */

extern NspGtkCellRendererAccel *nsp_gtkcellrendereraccel_object (NspObject *O);
extern int IsGtkCellRendererAccelObj (Stack stack, int i);
extern int IsGtkCellRendererAccel(NspObject *O);
extern NspGtkCellRendererAccel *GetGtkCellRendererAccelCopy (Stack stack, int i);
extern NspGtkCellRendererAccel *GetGtkCellRendererAccel (Stack stack, int i);

#endif /* NSP_INC_NspGtkCellRendererAccel */ 

#ifdef NspGtkCellRendererAccel_Private 
static int init_gtkcellrendereraccel(NspGtkCellRendererAccel *o,NspTypeGtkCellRendererAccel *type);
static char *nsp_gtkcellrendereraccel_type_as_string(void);
static char *nsp_gtkcellrendereraccel_type_short_string(NspObject *v);
static AttrTab gtkcellrendereraccel_attrs[];
static NspMethods *gtkcellrendereraccel_get_methods(void);
/* static int int_gtkcellrendereraccel_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkCellRendererAccel_Private */
