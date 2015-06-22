/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkApplication
#define NSP_INC_NspGtkApplication

/*
 * Copyright (C) 1998-2015 Jean-Philippe Chancelier Enpc/Cermics
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

/* NspGtkApplication */

#include <nsp/gtk/gtkwindow.h>

/*
 * NspGtkApplication inherits from GtkWindow
 * just change some type attributes 
 */

typedef NspGtkWindow NspGtkApplication ;
typedef NspTypeGtkWindow NspTypeGtkApplication ;

extern int nsp_type_gtkapplication_id;
extern NspTypeGtkApplication *nsp_type_gtkapplication;

/* type instances for gtkwindow */

NspTypeGtkApplication *new_type_gtkapplication(type_mode mode);

/* instance for NspGtkApplication */

NspGtkApplication *new_gtkapplication();

/*
 * Object methods redefined for gtkapplication 
 */

#define NULLGTKAPPLICATION (NspGtkApplication*) 0


/* from NspGtkApplicationObj.c */

extern NspGtkApplication *nsp_gtkapplication_object (NspObject *O);
extern int IsGtkApplicationObj (Stack stack, int i);
extern int IsGtkApplication(NspObject *O);
extern NspGtkApplication *GetGtkApplicationCopy (Stack stack, int i);
extern NspGtkApplication *GetGtkApplication (Stack stack, int i);

#endif /* NSP_INC_NspGtkApplication */ 

#ifdef NspGtkApplication_Private 
static int init_gtkapplication(NspGtkApplication *o,NspTypeGtkApplication *type);
static char *nsp_gtkapplication_type_as_string(void);
static char *nsp_gtkapplication_type_short_string(NspObject *v);
static AttrTab gtkapplication_attrs[];
static NspMethods *gtkapplication_get_methods(void);
/* static int int_gtkapplication_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkApplication_Private */
