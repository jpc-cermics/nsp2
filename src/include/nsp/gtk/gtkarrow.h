/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkArrow
#define NSP_INC_NspGtkArrow

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

/* NspGtkArrow */

#include <nsp/gtk/gtkmisc.h>

/*
 * NspGtkArrow inherits from GtkMisc
 * just change some type attributes 
 */

typedef NspGtkMisc NspGtkArrow ;
typedef NspTypeGtkMisc NspTypeGtkArrow ;

extern int nsp_type_gtkarrow_id;
extern NspTypeGtkArrow *nsp_type_gtkarrow;

/* type instances for gtkmisc */

NspTypeGtkArrow *new_type_gtkarrow(type_mode mode);

/* instance for NspGtkArrow */

NspGtkArrow *new_gtkarrow();

/*
 * Object methods redefined for gtkarrow 
 */

#define NULLGTKARROW (NspGtkArrow*) 0


/* from NspGtkArrowObj.c */

extern NspGtkArrow *nsp_gtkarrow_object (NspObject *O);
extern int IsGtkArrowObj (Stack stack, int i);
extern int IsGtkArrow(NspObject *O);
extern NspGtkArrow *GetGtkArrowCopy (Stack stack, int i);
extern NspGtkArrow *GetGtkArrow (Stack stack, int i);

#endif /* NSP_INC_NspGtkArrow */ 

#ifdef NspGtkArrow_Private 
static int init_gtkarrow(NspGtkArrow *o,NspTypeGtkArrow *type);
static char *nsp_gtkarrow_type_as_string(void);
static char *nsp_gtkarrow_type_short_string(NspObject *v);
static AttrTab gtkarrow_attrs[];
static NspMethods *gtkarrow_get_methods(void);
/* static int int_gtkarrow_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkArrow_Private */
