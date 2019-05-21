/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkColorChooserWidget
#define NSP_INC_NspGtkColorChooserWidget

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

/* NspGtkColorChooserWidget */

#include <nsp/gtk/gtkbox.h>

/*
 * NspGtkColorChooserWidget inherits from GtkBox
 * just change some type attributes 
 */

typedef NspGtkBox NspGtkColorChooserWidget ;
typedef NspTypeGtkBox NspTypeGtkColorChooserWidget ;

extern int nsp_type_gtkcolorchooserwidget_id;
extern NspTypeGtkColorChooserWidget *nsp_type_gtkcolorchooserwidget;

/* type instances for gtkbox */

NspTypeGtkColorChooserWidget *new_type_gtkcolorchooserwidget(type_mode mode);

/* instance for NspGtkColorChooserWidget */

NspGtkColorChooserWidget *new_gtkcolorchooserwidget();

/*
 * Object methods redefined for gtkcolorchooserwidget 
 */

#define NULLGTKCOLORCHOOSERWIDGET (NspGtkColorChooserWidget*) 0


/* from NspGtkColorChooserWidgetObj.c */

extern NspGtkColorChooserWidget *nsp_gtkcolorchooserwidget_object (NspObject *O);
extern int IsGtkColorChooserWidgetObj (Stack stack, int i);
extern int IsGtkColorChooserWidget(NspObject *O);
extern NspGtkColorChooserWidget *GetGtkColorChooserWidgetCopy (Stack stack, int i);
extern NspGtkColorChooserWidget *GetGtkColorChooserWidget (Stack stack, int i);

#endif /* NSP_INC_NspGtkColorChooserWidget */ 

#ifdef NspGtkColorChooserWidget_Private 
static int init_gtkcolorchooserwidget(NspGtkColorChooserWidget *o,NspTypeGtkColorChooserWidget *type);
static char *nsp_gtkcolorchooserwidget_type_as_string(void);
static char *nsp_gtkcolorchooserwidget_type_short_string(NspObject *v);
static AttrTab gtkcolorchooserwidget_attrs[];
static NspMethods *gtkcolorchooserwidget_get_methods(void);
/* static int int_gtkcolorchooserwidget_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkColorChooserWidget_Private */
