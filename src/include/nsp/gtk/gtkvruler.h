/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkVRuler
#define NSP_INC_NspGtkVRuler

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

/* NspGtkVRuler */

#include <nsp/gtk/gtkruler.h>

/*
 * NspGtkVRuler inherits from GtkRuler
 * just change some type attributes 
 */

typedef NspGtkRuler NspGtkVRuler ;
typedef NspTypeGtkRuler NspTypeGtkVRuler ;

extern int nsp_type_gtkvruler_id;
extern NspTypeGtkVRuler *nsp_type_gtkvruler;

/* type instances for gtkruler */

NspTypeGtkVRuler *new_type_gtkvruler(type_mode mode);

/* instance for NspGtkVRuler */

NspGtkVRuler *new_gtkvruler();

/*
 * Object methods redefined for gtkvruler 
 */

#define NULLGTKVRULER (NspGtkVRuler*) 0


/* from NspGtkVRulerObj.c */

extern NspGtkVRuler *nsp_gtkvruler_object (NspObject *O);
extern int IsGtkVRulerObj (Stack stack, int i);
extern int IsGtkVRuler(NspObject *O);
extern NspGtkVRuler *GetGtkVRulerCopy (Stack stack, int i);
extern NspGtkVRuler *GetGtkVRuler (Stack stack, int i);

#endif /* NSP_INC_NspGtkVRuler */ 

#ifdef NspGtkVRuler_Private 
static int init_gtkvruler(NspGtkVRuler *o,NspTypeGtkVRuler *type);
static char *nsp_gtkvruler_type_as_string(void);
static char *nsp_gtkvruler_type_short_string(NspObject *v);
static AttrTab gtkvruler_attrs[];
static NspMethods *gtkvruler_get_methods(void);
/* static int int_gtkvruler_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkVRuler_Private */
