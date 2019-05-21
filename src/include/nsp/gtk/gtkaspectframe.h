/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkAspectFrame
#define NSP_INC_NspGtkAspectFrame

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

/* NspGtkAspectFrame */

#include <nsp/gtk/gtkframe.h>

/*
 * NspGtkAspectFrame inherits from GtkFrame
 * just change some type attributes 
 */

typedef NspGtkFrame NspGtkAspectFrame ;
typedef NspTypeGtkFrame NspTypeGtkAspectFrame ;

extern int nsp_type_gtkaspectframe_id;
extern NspTypeGtkAspectFrame *nsp_type_gtkaspectframe;

/* type instances for gtkframe */

NspTypeGtkAspectFrame *new_type_gtkaspectframe(type_mode mode);

/* instance for NspGtkAspectFrame */

NspGtkAspectFrame *new_gtkaspectframe();

/*
 * Object methods redefined for gtkaspectframe 
 */

#define NULLGTKASPECTFRAME (NspGtkAspectFrame*) 0


/* from NspGtkAspectFrameObj.c */

extern NspGtkAspectFrame *nsp_gtkaspectframe_object (NspObject *O);
extern int IsGtkAspectFrameObj (Stack stack, int i);
extern int IsGtkAspectFrame(NspObject *O);
extern NspGtkAspectFrame *GetGtkAspectFrameCopy (Stack stack, int i);
extern NspGtkAspectFrame *GetGtkAspectFrame (Stack stack, int i);

#endif /* NSP_INC_NspGtkAspectFrame */ 

#ifdef NspGtkAspectFrame_Private 
static int init_gtkaspectframe(NspGtkAspectFrame *o,NspTypeGtkAspectFrame *type);
static char *nsp_gtkaspectframe_type_as_string(void);
static char *nsp_gtkaspectframe_type_short_string(NspObject *v);
static AttrTab gtkaspectframe_attrs[];
static NspMethods *gtkaspectframe_get_methods(void);
/* static int int_gtkaspectframe_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkAspectFrame_Private */
