/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkProgress
#define NSP_INC_NspGtkProgress

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

/* NspGtkProgress */

#include <nsp/gtk/gtkwidget.h>

/*
 * NspGtkProgress inherits from GtkWidget
 * just change some type attributes 
 */

typedef NspGtkWidget NspGtkProgress ;
typedef NspTypeGtkWidget NspTypeGtkProgress ;

extern int nsp_type_gtkprogress_id;
extern NspTypeGtkProgress *nsp_type_gtkprogress;

/* type instances for gtkwidget */

NspTypeGtkProgress *new_type_gtkprogress(type_mode mode);

/* instance for NspGtkProgress */

NspGtkProgress *new_gtkprogress();

/*
 * Object methods redefined for gtkprogress 
 */

#define NULLGTKPROGRESS (NspGtkProgress*) 0


/* from NspGtkProgressObj.c */

extern NspGtkProgress *nsp_gtkprogress_object (NspObject *O);
extern int IsGtkProgressObj (Stack stack, int i);
extern int IsGtkProgress(NspObject *O);
extern NspGtkProgress *GetGtkProgressCopy (Stack stack, int i);
extern NspGtkProgress *GetGtkProgress (Stack stack, int i);

#endif /* NSP_INC_NspGtkProgress */ 

#ifdef NspGtkProgress_Private 
static int init_gtkprogress(NspGtkProgress *o,NspTypeGtkProgress *type);
static char *nsp_gtkprogress_type_as_string(void);
static char *nsp_gtkprogress_type_short_string(NspObject *v);
static AttrTab gtkprogress_attrs[];
static NspMethods *gtkprogress_get_methods(void);
/* static int int_gtkprogress_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkProgress_Private */
