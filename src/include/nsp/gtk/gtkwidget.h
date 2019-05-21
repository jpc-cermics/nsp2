/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkWidget
#define NSP_INC_NspGtkWidget

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

/* NspGtkWidget */

#include <nsp/gtk/gobject.h>

/*
 * NspGtkWidget inherits from GObject
 * just change some type attributes 
 */

typedef NspGObject NspGtkWidget ;
typedef NspTypeGObject NspTypeGtkWidget ;

extern int nsp_type_gtkwidget_id;
extern NspTypeGtkWidget *nsp_type_gtkwidget;

/* type instances for gobject */

NspTypeGtkWidget *new_type_gtkwidget(type_mode mode);

/* instance for NspGtkWidget */

NspGtkWidget *new_gtkwidget();

/*
 * Object methods redefined for gtkwidget 
 */

#define NULLGTKWIDGET (NspGtkWidget*) 0


/* from NspGtkWidgetObj.c */

extern NspGtkWidget *nsp_gtkwidget_object (NspObject *O);
extern int IsGtkWidgetObj (Stack stack, int i);
extern int IsGtkWidget(NspObject *O);
extern NspGtkWidget *GetGtkWidgetCopy (Stack stack, int i);
extern NspGtkWidget *GetGtkWidget (Stack stack, int i);

#endif /* NSP_INC_NspGtkWidget */ 

#ifdef NspGtkWidget_Private 
static int init_gtkwidget(NspGtkWidget *o,NspTypeGtkWidget *type);
static char *nsp_gtkwidget_type_as_string(void);
static char *nsp_gtkwidget_type_short_string(NspObject *v);
static AttrTab gtkwidget_attrs[];
static NspMethods *gtkwidget_get_methods(void);
/* static int int_gtkwidget_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkWidget_Private */
