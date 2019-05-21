/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkFrame
#define NSP_INC_NspGtkFrame

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

/* NspGtkFrame */

#include <nsp/gtk/gtkbin.h>

/*
 * NspGtkFrame inherits from GtkBin
 * just change some type attributes 
 */

typedef NspGtkBin NspGtkFrame ;
typedef NspTypeGtkBin NspTypeGtkFrame ;

extern int nsp_type_gtkframe_id;
extern NspTypeGtkFrame *nsp_type_gtkframe;

/* type instances for gtkbin */

NspTypeGtkFrame *new_type_gtkframe(type_mode mode);

/* instance for NspGtkFrame */

NspGtkFrame *new_gtkframe();

/*
 * Object methods redefined for gtkframe 
 */

#define NULLGTKFRAME (NspGtkFrame*) 0


/* from NspGtkFrameObj.c */

extern NspGtkFrame *nsp_gtkframe_object (NspObject *O);
extern int IsGtkFrameObj (Stack stack, int i);
extern int IsGtkFrame(NspObject *O);
extern NspGtkFrame *GetGtkFrameCopy (Stack stack, int i);
extern NspGtkFrame *GetGtkFrame (Stack stack, int i);

#endif /* NSP_INC_NspGtkFrame */ 

#ifdef NspGtkFrame_Private 
static int init_gtkframe(NspGtkFrame *o,NspTypeGtkFrame *type);
static char *nsp_gtkframe_type_as_string(void);
static char *nsp_gtkframe_type_short_string(NspObject *v);
static AttrTab gtkframe_attrs[];
static NspMethods *gtkframe_get_methods(void);
/* static int int_gtkframe_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkFrame_Private */
