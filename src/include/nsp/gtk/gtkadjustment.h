/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkAdjustment
#define NSP_INC_NspGtkAdjustment

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

/* NspGtkAdjustment */

#include <nsp/gtk/gobject.h>

/*
 * NspGtkAdjustment inherits from GObject
 * just change some type attributes 
 */

typedef NspGObject NspGtkAdjustment ;
typedef NspTypeGObject NspTypeGtkAdjustment ;

extern int nsp_type_gtkadjustment_id;
extern NspTypeGtkAdjustment *nsp_type_gtkadjustment;

/* type instances for gobject */

NspTypeGtkAdjustment *new_type_gtkadjustment(type_mode mode);

/* instance for NspGtkAdjustment */

NspGtkAdjustment *new_gtkadjustment();

/*
 * Object methods redefined for gtkadjustment 
 */

#define NULLGTKADJUSTMENT (NspGtkAdjustment*) 0


/* from NspGtkAdjustmentObj.c */

extern NspGtkAdjustment *nsp_gtkadjustment_object (NspObject *O);
extern int IsGtkAdjustmentObj (Stack stack, int i);
extern int IsGtkAdjustment(NspObject *O);
extern NspGtkAdjustment *GetGtkAdjustmentCopy (Stack stack, int i);
extern NspGtkAdjustment *GetGtkAdjustment (Stack stack, int i);

#endif /* NSP_INC_NspGtkAdjustment */ 

#ifdef NspGtkAdjustment_Private 
static int init_gtkadjustment(NspGtkAdjustment *o,NspTypeGtkAdjustment *type);
static char *nsp_gtkadjustment_type_as_string(void);
static char *nsp_gtkadjustment_type_short_string(NspObject *v);
static AttrTab gtkadjustment_attrs[];
static NspMethods *gtkadjustment_get_methods(void);
/* static int int_gtkadjustment_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkAdjustment_Private */
