/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkHRuler
#define NSP_INC_NspGtkHRuler

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

/* NspGtkHRuler */

#include <nsp/gtk/gtkruler.h>

/*
 * NspGtkHRuler inherits from GtkRuler
 * just change some type attributes 
 */

typedef NspGtkRuler NspGtkHRuler ;
typedef NspTypeGtkRuler NspTypeGtkHRuler ;

extern int nsp_type_gtkhruler_id;
extern NspTypeGtkHRuler *nsp_type_gtkhruler;

/* type instances for gtkruler */

NspTypeGtkHRuler *new_type_gtkhruler(type_mode mode);

/* instance for NspGtkHRuler */

NspGtkHRuler *new_gtkhruler();

/*
 * Object methods redefined for gtkhruler 
 */

#define NULLGTKHRULER (NspGtkHRuler*) 0


/* from NspGtkHRulerObj.c */

extern NspGtkHRuler *nsp_gtkhruler_object (NspObject *O);
extern int IsGtkHRulerObj (Stack stack, int i);
extern int IsGtkHRuler(NspObject *O);
extern NspGtkHRuler *GetGtkHRulerCopy (Stack stack, int i);
extern NspGtkHRuler *GetGtkHRuler (Stack stack, int i);

#endif /* NSP_INC_NspGtkHRuler */ 

#ifdef NspGtkHRuler_Private 
static int init_gtkhruler(NspGtkHRuler *o,NspTypeGtkHRuler *type);
static char *nsp_gtkhruler_type_as_string(void);
static char *nsp_gtkhruler_type_short_string(NspObject *v);
static AttrTab gtkhruler_attrs[];
static NspMethods *gtkhruler_get_methods(void);
/* static int int_gtkhruler_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkHRuler_Private */
