/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkSourceStyleSchemeChooserWidget
#define NSP_INC_NspGtkSourceStyleSchemeChooserWidget

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

/* NspGtkSourceStyleSchemeChooserWidget */

#include <nsp/gtk/gtkbin.h>

/*
 * NspGtkSourceStyleSchemeChooserWidget inherits from GtkBin
 * just change some type attributes 
 */

typedef NspGtkBin NspGtkSourceStyleSchemeChooserWidget ;
typedef NspTypeGtkBin NspTypeGtkSourceStyleSchemeChooserWidget ;

extern int nsp_type_gtksourcestyleschemechooserwidget_id;
extern NspTypeGtkSourceStyleSchemeChooserWidget *nsp_type_gtksourcestyleschemechooserwidget;

/* type instances for gtkbin */

NspTypeGtkSourceStyleSchemeChooserWidget *new_type_gtksourcestyleschemechooserwidget(type_mode mode);

/* instance for NspGtkSourceStyleSchemeChooserWidget */

NspGtkSourceStyleSchemeChooserWidget *new_gtksourcestyleschemechooserwidget();

/*
 * Object methods redefined for gtksourcestyleschemechooserwidget 
 */

#define NULLGTKSOURCESTYLESCHEMECHOOSERWIDGET (NspGtkSourceStyleSchemeChooserWidget*) 0


/* from NspGtkSourceStyleSchemeChooserWidgetObj.c */

extern NspGtkSourceStyleSchemeChooserWidget *nsp_gtksourcestyleschemechooserwidget_object (NspObject *O);
extern int IsGtkSourceStyleSchemeChooserWidgetObj (Stack stack, int i);
extern int IsGtkSourceStyleSchemeChooserWidget(NspObject *O);
extern NspGtkSourceStyleSchemeChooserWidget *GetGtkSourceStyleSchemeChooserWidgetCopy (Stack stack, int i);
extern NspGtkSourceStyleSchemeChooserWidget *GetGtkSourceStyleSchemeChooserWidget (Stack stack, int i);

#endif /* NSP_INC_NspGtkSourceStyleSchemeChooserWidget */ 

#ifdef NspGtkSourceStyleSchemeChooserWidget_Private 
static int init_gtksourcestyleschemechooserwidget(NspGtkSourceStyleSchemeChooserWidget *o,NspTypeGtkSourceStyleSchemeChooserWidget *type);
static char *nsp_gtksourcestyleschemechooserwidget_type_as_string(void);
static char *nsp_gtksourcestyleschemechooserwidget_type_short_string(NspObject *v);
static AttrTab gtksourcestyleschemechooserwidget_attrs[];
static NspMethods *gtksourcestyleschemechooserwidget_get_methods(void);
/* static int int_gtksourcestyleschemechooserwidget_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkSourceStyleSchemeChooserWidget_Private */
