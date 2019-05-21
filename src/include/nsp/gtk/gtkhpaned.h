/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkHPaned
#define NSP_INC_NspGtkHPaned

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

/* NspGtkHPaned */

#include <nsp/gtk/gtkpaned.h>

/*
 * NspGtkHPaned inherits from GtkPaned
 * just change some type attributes 
 */

typedef NspGtkPaned NspGtkHPaned ;
typedef NspTypeGtkPaned NspTypeGtkHPaned ;

extern int nsp_type_gtkhpaned_id;
extern NspTypeGtkHPaned *nsp_type_gtkhpaned;

/* type instances for gtkpaned */

NspTypeGtkHPaned *new_type_gtkhpaned(type_mode mode);

/* instance for NspGtkHPaned */

NspGtkHPaned *new_gtkhpaned();

/*
 * Object methods redefined for gtkhpaned 
 */

#define NULLGTKHPANED (NspGtkHPaned*) 0


/* from NspGtkHPanedObj.c */

extern NspGtkHPaned *nsp_gtkhpaned_object (NspObject *O);
extern int IsGtkHPanedObj (Stack stack, int i);
extern int IsGtkHPaned(NspObject *O);
extern NspGtkHPaned *GetGtkHPanedCopy (Stack stack, int i);
extern NspGtkHPaned *GetGtkHPaned (Stack stack, int i);

#endif /* NSP_INC_NspGtkHPaned */ 

#ifdef NspGtkHPaned_Private 
static int init_gtkhpaned(NspGtkHPaned *o,NspTypeGtkHPaned *type);
static char *nsp_gtkhpaned_type_as_string(void);
static char *nsp_gtkhpaned_type_short_string(NspObject *v);
static AttrTab gtkhpaned_attrs[];
static NspMethods *gtkhpaned_get_methods(void);
/* static int int_gtkhpaned_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkHPaned_Private */
