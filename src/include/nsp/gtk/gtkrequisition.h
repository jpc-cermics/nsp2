/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkRequisition
#define NSP_INC_NspGtkRequisition

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

/* NspGtkRequisition */

#include <nsp/gtk/gboxed.h>

/*
 * NspGtkRequisition inherits from GBoxed
 * just change some type attributes 
 */

typedef NspGBoxed NspGtkRequisition ;
typedef NspTypeGBoxed NspTypeGtkRequisition ;

extern int nsp_type_gtkrequisition_id;
extern NspTypeGtkRequisition *nsp_type_gtkrequisition;

/* type instances for gboxed */

NspTypeGtkRequisition *new_type_gtkrequisition(type_mode mode);

/* instance for NspGtkRequisition */

NspGtkRequisition *new_gtkrequisition();

/*
 * Object methods redefined for gtkrequisition 
 */

#define NULLGTKREQUISITION (NspGtkRequisition*) 0


/* from NspGtkRequisitionObj.c */

extern NspGtkRequisition *nsp_gtkrequisition_object (NspObject *O);
extern int IsGtkRequisitionObj (Stack stack, int i);
extern int IsGtkRequisition(NspObject *O);
extern NspGtkRequisition *GetGtkRequisitionCopy (Stack stack, int i);
extern NspGtkRequisition *GetGtkRequisition (Stack stack, int i);

#endif /* NSP_INC_NspGtkRequisition */ 

#ifdef NspGtkRequisition_Private 
static int init_gtkrequisition(NspGtkRequisition *o,NspTypeGtkRequisition *type);
static char *nsp_gtkrequisition_type_as_string(void);
static char *nsp_gtkrequisition_type_short_string(NspObject *v);
static AttrTab gtkrequisition_attrs[];
static NspMethods *gtkrequisition_get_methods(void);
/* static int int_gtkrequisition_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkRequisition_Private */
