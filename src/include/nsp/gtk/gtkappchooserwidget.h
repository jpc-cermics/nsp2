/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkAppChooserWidget
#define NSP_INC_NspGtkAppChooserWidget

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

/* NspGtkAppChooserWidget */

#include <nsp/gtk/gtkbox.h>

/*
 * NspGtkAppChooserWidget inherits from GtkBox
 * just change some type attributes 
 */

typedef NspGtkBox NspGtkAppChooserWidget ;
typedef NspTypeGtkBox NspTypeGtkAppChooserWidget ;

extern int nsp_type_gtkappchooserwidget_id;
extern NspTypeGtkAppChooserWidget *nsp_type_gtkappchooserwidget;

/* type instances for gtkbox */

NspTypeGtkAppChooserWidget *new_type_gtkappchooserwidget(type_mode mode);

/* instance for NspGtkAppChooserWidget */

NspGtkAppChooserWidget *new_gtkappchooserwidget();

/*
 * Object methods redefined for gtkappchooserwidget 
 */

#define NULLGTKAPPCHOOSERWIDGET (NspGtkAppChooserWidget*) 0


/* from NspGtkAppChooserWidgetObj.c */

extern NspGtkAppChooserWidget *nsp_gtkappchooserwidget_object (NspObject *O);
extern int IsGtkAppChooserWidgetObj (Stack stack, int i);
extern int IsGtkAppChooserWidget(NspObject *O);
extern NspGtkAppChooserWidget *GetGtkAppChooserWidgetCopy (Stack stack, int i);
extern NspGtkAppChooserWidget *GetGtkAppChooserWidget (Stack stack, int i);

#endif /* NSP_INC_NspGtkAppChooserWidget */ 

#ifdef NspGtkAppChooserWidget_Private 
static int init_gtkappchooserwidget(NspGtkAppChooserWidget *o,NspTypeGtkAppChooserWidget *type);
static char *nsp_gtkappchooserwidget_type_as_string(void);
static char *nsp_gtkappchooserwidget_type_short_string(NspObject *v);
static AttrTab gtkappchooserwidget_attrs[];
static NspMethods *gtkappchooserwidget_get_methods(void);
/* static int int_gtkappchooserwidget_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkAppChooserWidget_Private */
