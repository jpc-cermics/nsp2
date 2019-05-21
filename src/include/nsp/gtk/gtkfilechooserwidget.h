/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkFileChooserWidget
#define NSP_INC_NspGtkFileChooserWidget

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

/* NspGtkFileChooserWidget */

#include <nsp/gtk/gtkbox.h>

/*
 * NspGtkFileChooserWidget inherits from GtkBox
 * just change some type attributes 
 */

typedef NspGtkBox NspGtkFileChooserWidget ;
typedef NspTypeGtkBox NspTypeGtkFileChooserWidget ;

extern int nsp_type_gtkfilechooserwidget_id;
extern NspTypeGtkFileChooserWidget *nsp_type_gtkfilechooserwidget;

/* type instances for gtkbox */

NspTypeGtkFileChooserWidget *new_type_gtkfilechooserwidget(type_mode mode);

/* instance for NspGtkFileChooserWidget */

NspGtkFileChooserWidget *new_gtkfilechooserwidget();

/*
 * Object methods redefined for gtkfilechooserwidget 
 */

#define NULLGTKFILECHOOSERWIDGET (NspGtkFileChooserWidget*) 0


/* from NspGtkFileChooserWidgetObj.c */

extern NspGtkFileChooserWidget *nsp_gtkfilechooserwidget_object (NspObject *O);
extern int IsGtkFileChooserWidgetObj (Stack stack, int i);
extern int IsGtkFileChooserWidget(NspObject *O);
extern NspGtkFileChooserWidget *GetGtkFileChooserWidgetCopy (Stack stack, int i);
extern NspGtkFileChooserWidget *GetGtkFileChooserWidget (Stack stack, int i);

#endif /* NSP_INC_NspGtkFileChooserWidget */ 

#ifdef NspGtkFileChooserWidget_Private 
static int init_gtkfilechooserwidget(NspGtkFileChooserWidget *o,NspTypeGtkFileChooserWidget *type);
static char *nsp_gtkfilechooserwidget_type_as_string(void);
static char *nsp_gtkfilechooserwidget_type_short_string(NspObject *v);
static AttrTab gtkfilechooserwidget_attrs[];
static NspMethods *gtkfilechooserwidget_get_methods(void);
/* static int int_gtkfilechooserwidget_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkFileChooserWidget_Private */
