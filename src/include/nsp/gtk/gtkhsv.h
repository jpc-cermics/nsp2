/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkHSV
#define NSP_INC_NspGtkHSV

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

/* NspGtkHSV */

#include <nsp/gtk/gtkwidget.h>

/*
 * NspGtkHSV inherits from GtkWidget
 * just change some type attributes 
 */

typedef NspGtkWidget NspGtkHSV ;
typedef NspTypeGtkWidget NspTypeGtkHSV ;

extern int nsp_type_gtkhsv_id;
extern NspTypeGtkHSV *nsp_type_gtkhsv;

/* type instances for gtkwidget */

NspTypeGtkHSV *new_type_gtkhsv(type_mode mode);

/* instance for NspGtkHSV */

NspGtkHSV *new_gtkhsv();

/*
 * Object methods redefined for gtkhsv 
 */

#define NULLGTKHSV (NspGtkHSV*) 0


/* from NspGtkHSVObj.c */

extern NspGtkHSV *nsp_gtkhsv_object (NspObject *O);
extern int IsGtkHSVObj (Stack stack, int i);
extern int IsGtkHSV(NspObject *O);
extern NspGtkHSV *GetGtkHSVCopy (Stack stack, int i);
extern NspGtkHSV *GetGtkHSV (Stack stack, int i);

#endif /* NSP_INC_NspGtkHSV */ 

#ifdef NspGtkHSV_Private 
static int init_gtkhsv(NspGtkHSV *o,NspTypeGtkHSV *type);
static char *nsp_gtkhsv_type_as_string(void);
static char *nsp_gtkhsv_type_short_string(NspObject *v);
static AttrTab gtkhsv_attrs[];
static NspMethods *gtkhsv_get_methods(void);
/* static int int_gtkhsv_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkHSV_Private */
