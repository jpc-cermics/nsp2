/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkAlignment
#define NSP_INC_NspGtkAlignment

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

/* NspGtkAlignment */

#include <nsp/gtk/gtkbin.h>

/*
 * NspGtkAlignment inherits from GtkBin
 * just change some type attributes 
 */

typedef NspGtkBin NspGtkAlignment ;
typedef NspTypeGtkBin NspTypeGtkAlignment ;

extern int nsp_type_gtkalignment_id;
extern NspTypeGtkAlignment *nsp_type_gtkalignment;

/* type instances for gtkbin */

NspTypeGtkAlignment *new_type_gtkalignment(type_mode mode);

/* instance for NspGtkAlignment */

NspGtkAlignment *new_gtkalignment();

/*
 * Object methods redefined for gtkalignment 
 */

#define NULLGTKALIGNMENT (NspGtkAlignment*) 0


/* from NspGtkAlignmentObj.c */

extern NspGtkAlignment *nsp_gtkalignment_object (NspObject *O);
extern int IsGtkAlignmentObj (Stack stack, int i);
extern int IsGtkAlignment(NspObject *O);
extern NspGtkAlignment *GetGtkAlignmentCopy (Stack stack, int i);
extern NspGtkAlignment *GetGtkAlignment (Stack stack, int i);

#endif /* NSP_INC_NspGtkAlignment */ 

#ifdef NspGtkAlignment_Private 
static int init_gtkalignment(NspGtkAlignment *o,NspTypeGtkAlignment *type);
static char *nsp_gtkalignment_type_as_string(void);
static char *nsp_gtkalignment_type_short_string(NspObject *v);
static AttrTab gtkalignment_attrs[];
static NspMethods *gtkalignment_get_methods(void);
/* static int int_gtkalignment_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkAlignment_Private */
