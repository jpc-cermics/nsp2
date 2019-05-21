/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkCellArea
#define NSP_INC_NspGtkCellArea

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

/* NspGtkCellArea */

#include <nsp/gtk/gobject.h>

/*
 * NspGtkCellArea inherits from GObject
 * just change some type attributes 
 */

typedef NspGObject NspGtkCellArea ;
typedef NspTypeGObject NspTypeGtkCellArea ;

extern int nsp_type_gtkcellarea_id;
extern NspTypeGtkCellArea *nsp_type_gtkcellarea;

/* type instances for gobject */

NspTypeGtkCellArea *new_type_gtkcellarea(type_mode mode);

/* instance for NspGtkCellArea */

NspGtkCellArea *new_gtkcellarea();

/*
 * Object methods redefined for gtkcellarea 
 */

#define NULLGTKCELLAREA (NspGtkCellArea*) 0


/* from NspGtkCellAreaObj.c */

extern NspGtkCellArea *nsp_gtkcellarea_object (NspObject *O);
extern int IsGtkCellAreaObj (Stack stack, int i);
extern int IsGtkCellArea(NspObject *O);
extern NspGtkCellArea *GetGtkCellAreaCopy (Stack stack, int i);
extern NspGtkCellArea *GetGtkCellArea (Stack stack, int i);

#endif /* NSP_INC_NspGtkCellArea */ 

#ifdef NspGtkCellArea_Private 
static int init_gtkcellarea(NspGtkCellArea *o,NspTypeGtkCellArea *type);
static char *nsp_gtkcellarea_type_as_string(void);
static char *nsp_gtkcellarea_type_short_string(NspObject *v);
static AttrTab gtkcellarea_attrs[];
static NspMethods *gtkcellarea_get_methods(void);
/* static int int_gtkcellarea_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkCellArea_Private */
