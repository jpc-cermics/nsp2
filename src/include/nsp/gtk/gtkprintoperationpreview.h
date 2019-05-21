/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkPrintOperationPreview
#define NSP_INC_NspGtkPrintOperationPreview

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

/* NspGtkPrintOperationPreview */

#include <nsp/gtk/gobject.h>

/*
 * NspGtkPrintOperationPreview inherits from GObject
 * just change some type attributes 
 */

typedef NspGObject NspGtkPrintOperationPreview ;
typedef NspTypeGObject NspTypeGtkPrintOperationPreview ;

extern int nsp_type_gtkprintoperationpreview_id;
extern NspTypeGtkPrintOperationPreview *nsp_type_gtkprintoperationpreview;

/* type instances for gobject */

NspTypeGtkPrintOperationPreview *new_type_gtkprintoperationpreview(type_mode mode);

/* instance for NspGtkPrintOperationPreview */

NspGtkPrintOperationPreview *new_gtkprintoperationpreview();

/*
 * Object methods redefined for gtkprintoperationpreview 
 */

#define NULLGTKPRINTOPERATIONPREVIEW (NspGtkPrintOperationPreview*) 0


/* from NspGtkPrintOperationPreviewObj.c */

extern NspGtkPrintOperationPreview *nsp_gtkprintoperationpreview_object (NspObject *O);
extern int IsGtkPrintOperationPreviewObj (Stack stack, int i);
extern int IsGtkPrintOperationPreview(NspObject *O);
extern NspGtkPrintOperationPreview *GetGtkPrintOperationPreviewCopy (Stack stack, int i);
extern NspGtkPrintOperationPreview *GetGtkPrintOperationPreview (Stack stack, int i);

#endif /* NSP_INC_NspGtkPrintOperationPreview */ 

#ifdef NspGtkPrintOperationPreview_Private 
static int init_gtkprintoperationpreview(NspGtkPrintOperationPreview *o,NspTypeGtkPrintOperationPreview *type);
static char *nsp_gtkprintoperationpreview_type_as_string(void);
static char *nsp_gtkprintoperationpreview_type_short_string(NspObject *v);
static AttrTab gtkprintoperationpreview_attrs[];
static NspMethods *gtkprintoperationpreview_get_methods(void);
/* static int int_gtkprintoperationpreview_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkPrintOperationPreview_Private */
