/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkRecentChooserWidget
#define NSP_INC_NspGtkRecentChooserWidget

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

/* NspGtkRecentChooserWidget */

#include <nsp/gtk/gtkbox.h>

/*
 * NspGtkRecentChooserWidget inherits from GtkBox
 * just change some type attributes 
 */

typedef NspGtkBox NspGtkRecentChooserWidget ;
typedef NspTypeGtkBox NspTypeGtkRecentChooserWidget ;

extern int nsp_type_gtkrecentchooserwidget_id;
extern NspTypeGtkRecentChooserWidget *nsp_type_gtkrecentchooserwidget;

/* type instances for gtkbox */

NspTypeGtkRecentChooserWidget *new_type_gtkrecentchooserwidget(type_mode mode);

/* instance for NspGtkRecentChooserWidget */

NspGtkRecentChooserWidget *new_gtkrecentchooserwidget();

/*
 * Object methods redefined for gtkrecentchooserwidget 
 */

#define NULLGTKRECENTCHOOSERWIDGET (NspGtkRecentChooserWidget*) 0


/* from NspGtkRecentChooserWidgetObj.c */

extern NspGtkRecentChooserWidget *nsp_gtkrecentchooserwidget_object (NspObject *O);
extern int IsGtkRecentChooserWidgetObj (Stack stack, int i);
extern int IsGtkRecentChooserWidget(NspObject *O);
extern NspGtkRecentChooserWidget *GetGtkRecentChooserWidgetCopy (Stack stack, int i);
extern NspGtkRecentChooserWidget *GetGtkRecentChooserWidget (Stack stack, int i);

#endif /* NSP_INC_NspGtkRecentChooserWidget */ 

#ifdef NspGtkRecentChooserWidget_Private 
static int init_gtkrecentchooserwidget(NspGtkRecentChooserWidget *o,NspTypeGtkRecentChooserWidget *type);
static char *nsp_gtkrecentchooserwidget_type_as_string(void);
static char *nsp_gtkrecentchooserwidget_type_short_string(NspObject *v);
static AttrTab gtkrecentchooserwidget_attrs[];
static NspMethods *gtkrecentchooserwidget_get_methods(void);
/* static int int_gtkrecentchooserwidget_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkRecentChooserWidget_Private */
