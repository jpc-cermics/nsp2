/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkHandleBox
#define NSP_INC_NspGtkHandleBox

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

/* NspGtkHandleBox */

#include <nsp/gtk/gtkbin.h>

/*
 * NspGtkHandleBox inherits from GtkBin
 * just change some type attributes 
 */

typedef NspGtkBin NspGtkHandleBox ;
typedef NspTypeGtkBin NspTypeGtkHandleBox ;

extern int nsp_type_gtkhandlebox_id;
extern NspTypeGtkHandleBox *nsp_type_gtkhandlebox;

/* type instances for gtkbin */

NspTypeGtkHandleBox *new_type_gtkhandlebox(type_mode mode);

/* instance for NspGtkHandleBox */

NspGtkHandleBox *new_gtkhandlebox();

/*
 * Object methods redefined for gtkhandlebox 
 */

#define NULLGTKHANDLEBOX (NspGtkHandleBox*) 0


/* from NspGtkHandleBoxObj.c */

extern NspGtkHandleBox *nsp_gtkhandlebox_object (NspObject *O);
extern int IsGtkHandleBoxObj (Stack stack, int i);
extern int IsGtkHandleBox(NspObject *O);
extern NspGtkHandleBox *GetGtkHandleBoxCopy (Stack stack, int i);
extern NspGtkHandleBox *GetGtkHandleBox (Stack stack, int i);

#endif /* NSP_INC_NspGtkHandleBox */ 

#ifdef NspGtkHandleBox_Private 
static int init_gtkhandlebox(NspGtkHandleBox *o,NspTypeGtkHandleBox *type);
static char *nsp_gtkhandlebox_type_as_string(void);
static char *nsp_gtkhandlebox_type_short_string(NspObject *v);
static AttrTab gtkhandlebox_attrs[];
static NspMethods *gtkhandlebox_get_methods(void);
/* static int int_gtkhandlebox_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkHandleBox_Private */
