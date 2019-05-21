/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkProgressBar
#define NSP_INC_NspGtkProgressBar

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

/* NspGtkProgressBar */

#include <nsp/gtk/gtkwidget.h>

/*
 * NspGtkProgressBar inherits from GtkWidget
 * just change some type attributes 
 */

typedef NspGtkWidget NspGtkProgressBar ;
typedef NspTypeGtkWidget NspTypeGtkProgressBar ;

extern int nsp_type_gtkprogressbar_id;
extern NspTypeGtkProgressBar *nsp_type_gtkprogressbar;

/* type instances for gtkwidget */

NspTypeGtkProgressBar *new_type_gtkprogressbar(type_mode mode);

/* instance for NspGtkProgressBar */

NspGtkProgressBar *new_gtkprogressbar();

/*
 * Object methods redefined for gtkprogressbar 
 */

#define NULLGTKPROGRESSBAR (NspGtkProgressBar*) 0


/* from NspGtkProgressBarObj.c */

extern NspGtkProgressBar *nsp_gtkprogressbar_object (NspObject *O);
extern int IsGtkProgressBarObj (Stack stack, int i);
extern int IsGtkProgressBar(NspObject *O);
extern NspGtkProgressBar *GetGtkProgressBarCopy (Stack stack, int i);
extern NspGtkProgressBar *GetGtkProgressBar (Stack stack, int i);

#endif /* NSP_INC_NspGtkProgressBar */ 

#ifdef NspGtkProgressBar_Private 
static int init_gtkprogressbar(NspGtkProgressBar *o,NspTypeGtkProgressBar *type);
static char *nsp_gtkprogressbar_type_as_string(void);
static char *nsp_gtkprogressbar_type_short_string(NspObject *v);
static AttrTab gtkprogressbar_attrs[];
static NspMethods *gtkprogressbar_get_methods(void);
/* static int int_gtkprogressbar_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkProgressBar_Private */
