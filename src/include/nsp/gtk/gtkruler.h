/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkRuler
#define NSP_INC_NspGtkRuler

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

/* NspGtkRuler */

#include <nsp/gtk/gtkwidget.h>

/*
 * NspGtkRuler inherits from GtkWidget
 * just change some type attributes 
 */

typedef NspGtkWidget NspGtkRuler ;
typedef NspTypeGtkWidget NspTypeGtkRuler ;

extern int nsp_type_gtkruler_id;
extern NspTypeGtkRuler *nsp_type_gtkruler;

/* type instances for gtkwidget */

NspTypeGtkRuler *new_type_gtkruler(type_mode mode);

/* instance for NspGtkRuler */

NspGtkRuler *new_gtkruler();

/*
 * Object methods redefined for gtkruler 
 */

#define NULLGTKRULER (NspGtkRuler*) 0


/* from NspGtkRulerObj.c */

extern NspGtkRuler *nsp_gtkruler_object (NspObject *O);
extern int IsGtkRulerObj (Stack stack, int i);
extern int IsGtkRuler(NspObject *O);
extern NspGtkRuler *GetGtkRulerCopy (Stack stack, int i);
extern NspGtkRuler *GetGtkRuler (Stack stack, int i);

#endif /* NSP_INC_NspGtkRuler */ 

#ifdef NspGtkRuler_Private 
static int init_gtkruler(NspGtkRuler *o,NspTypeGtkRuler *type);
static char *nsp_gtkruler_type_as_string(void);
static char *nsp_gtkruler_type_short_string(NspObject *v);
static AttrTab gtkruler_attrs[];
static NspMethods *gtkruler_get_methods(void);
/* static int int_gtkruler_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkRuler_Private */
