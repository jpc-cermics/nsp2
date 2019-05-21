/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkRange
#define NSP_INC_NspGtkRange

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

/* NspGtkRange */

#include <nsp/gtk/gtkwidget.h>

/*
 * NspGtkRange inherits from GtkWidget
 * just change some type attributes 
 */

typedef NspGtkWidget NspGtkRange ;
typedef NspTypeGtkWidget NspTypeGtkRange ;

extern int nsp_type_gtkrange_id;
extern NspTypeGtkRange *nsp_type_gtkrange;

/* type instances for gtkwidget */

NspTypeGtkRange *new_type_gtkrange(type_mode mode);

/* instance for NspGtkRange */

NspGtkRange *new_gtkrange();

/*
 * Object methods redefined for gtkrange 
 */

#define NULLGTKRANGE (NspGtkRange*) 0


/* from NspGtkRangeObj.c */

extern NspGtkRange *nsp_gtkrange_object (NspObject *O);
extern int IsGtkRangeObj (Stack stack, int i);
extern int IsGtkRange(NspObject *O);
extern NspGtkRange *GetGtkRangeCopy (Stack stack, int i);
extern NspGtkRange *GetGtkRange (Stack stack, int i);

#endif /* NSP_INC_NspGtkRange */ 

#ifdef NspGtkRange_Private 
static int init_gtkrange(NspGtkRange *o,NspTypeGtkRange *type);
static char *nsp_gtkrange_type_as_string(void);
static char *nsp_gtkrange_type_short_string(NspObject *v);
static AttrTab gtkrange_attrs[];
static NspMethods *gtkrange_get_methods(void);
/* static int int_gtkrange_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkRange_Private */
