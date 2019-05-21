/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkVPaned
#define NSP_INC_NspGtkVPaned

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

/* NspGtkVPaned */

#include <nsp/gtk/gtkpaned.h>

/*
 * NspGtkVPaned inherits from GtkPaned
 * just change some type attributes 
 */

typedef NspGtkPaned NspGtkVPaned ;
typedef NspTypeGtkPaned NspTypeGtkVPaned ;

extern int nsp_type_gtkvpaned_id;
extern NspTypeGtkVPaned *nsp_type_gtkvpaned;

/* type instances for gtkpaned */

NspTypeGtkVPaned *new_type_gtkvpaned(type_mode mode);

/* instance for NspGtkVPaned */

NspGtkVPaned *new_gtkvpaned();

/*
 * Object methods redefined for gtkvpaned 
 */

#define NULLGTKVPANED (NspGtkVPaned*) 0


/* from NspGtkVPanedObj.c */

extern NspGtkVPaned *nsp_gtkvpaned_object (NspObject *O);
extern int IsGtkVPanedObj (Stack stack, int i);
extern int IsGtkVPaned(NspObject *O);
extern NspGtkVPaned *GetGtkVPanedCopy (Stack stack, int i);
extern NspGtkVPaned *GetGtkVPaned (Stack stack, int i);

#endif /* NSP_INC_NspGtkVPaned */ 

#ifdef NspGtkVPaned_Private 
static int init_gtkvpaned(NspGtkVPaned *o,NspTypeGtkVPaned *type);
static char *nsp_gtkvpaned_type_as_string(void);
static char *nsp_gtkvpaned_type_short_string(NspObject *v);
static AttrTab gtkvpaned_attrs[];
static NspMethods *gtkvpaned_get_methods(void);
/* static int int_gtkvpaned_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkVPaned_Private */
