/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkPreview
#define NSP_INC_NspGtkPreview

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

/* NspGtkPreview */

#include <nsp/gtk/gtkwidget.h>

/*
 * NspGtkPreview inherits from GtkWidget
 * just change some type attributes 
 */

typedef NspGtkWidget NspGtkPreview ;
typedef NspTypeGtkWidget NspTypeGtkPreview ;

extern int nsp_type_gtkpreview_id;
extern NspTypeGtkPreview *nsp_type_gtkpreview;

/* type instances for gtkwidget */

NspTypeGtkPreview *new_type_gtkpreview(type_mode mode);

/* instance for NspGtkPreview */

NspGtkPreview *new_gtkpreview();

/*
 * Object methods redefined for gtkpreview 
 */

#define NULLGTKPREVIEW (NspGtkPreview*) 0


/* from NspGtkPreviewObj.c */

extern NspGtkPreview *nsp_gtkpreview_object (NspObject *O);
extern int IsGtkPreviewObj (Stack stack, int i);
extern int IsGtkPreview(NspObject *O);
extern NspGtkPreview *GetGtkPreviewCopy (Stack stack, int i);
extern NspGtkPreview *GetGtkPreview (Stack stack, int i);

#endif /* NSP_INC_NspGtkPreview */ 

#ifdef NspGtkPreview_Private 
static int init_gtkpreview(NspGtkPreview *o,NspTypeGtkPreview *type);
static char *nsp_gtkpreview_type_as_string(void);
static char *nsp_gtkpreview_type_short_string(NspObject *v);
static AttrTab gtkpreview_attrs[];
static NspMethods *gtkpreview_get_methods(void);
/* static int int_gtkpreview_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkPreview_Private */
