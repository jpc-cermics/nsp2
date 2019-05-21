/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkFontChooserWidget
#define NSP_INC_NspGtkFontChooserWidget

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

/* NspGtkFontChooserWidget */

#include <nsp/gtk/gtkbox.h>

/*
 * NspGtkFontChooserWidget inherits from GtkBox
 * just change some type attributes 
 */

typedef NspGtkBox NspGtkFontChooserWidget ;
typedef NspTypeGtkBox NspTypeGtkFontChooserWidget ;

extern int nsp_type_gtkfontchooserwidget_id;
extern NspTypeGtkFontChooserWidget *nsp_type_gtkfontchooserwidget;

/* type instances for gtkbox */

NspTypeGtkFontChooserWidget *new_type_gtkfontchooserwidget(type_mode mode);

/* instance for NspGtkFontChooserWidget */

NspGtkFontChooserWidget *new_gtkfontchooserwidget();

/*
 * Object methods redefined for gtkfontchooserwidget 
 */

#define NULLGTKFONTCHOOSERWIDGET (NspGtkFontChooserWidget*) 0


/* from NspGtkFontChooserWidgetObj.c */

extern NspGtkFontChooserWidget *nsp_gtkfontchooserwidget_object (NspObject *O);
extern int IsGtkFontChooserWidgetObj (Stack stack, int i);
extern int IsGtkFontChooserWidget(NspObject *O);
extern NspGtkFontChooserWidget *GetGtkFontChooserWidgetCopy (Stack stack, int i);
extern NspGtkFontChooserWidget *GetGtkFontChooserWidget (Stack stack, int i);

#endif /* NSP_INC_NspGtkFontChooserWidget */ 

#ifdef NspGtkFontChooserWidget_Private 
static int init_gtkfontchooserwidget(NspGtkFontChooserWidget *o,NspTypeGtkFontChooserWidget *type);
static char *nsp_gtkfontchooserwidget_type_as_string(void);
static char *nsp_gtkfontchooserwidget_type_short_string(NspObject *v);
static AttrTab gtkfontchooserwidget_attrs[];
static NspMethods *gtkfontchooserwidget_get_methods(void);
/* static int int_gtkfontchooserwidget_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkFontChooserWidget_Private */
