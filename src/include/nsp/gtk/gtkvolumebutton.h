/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkVolumeButton
#define NSP_INC_NspGtkVolumeButton

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

/* NspGtkVolumeButton */

#include <nsp/gtk/gtkscalebutton.h>

/*
 * NspGtkVolumeButton inherits from GtkScaleButton
 * just change some type attributes 
 */

typedef NspGtkScaleButton NspGtkVolumeButton ;
typedef NspTypeGtkScaleButton NspTypeGtkVolumeButton ;

extern int nsp_type_gtkvolumebutton_id;
extern NspTypeGtkVolumeButton *nsp_type_gtkvolumebutton;

/* type instances for gtkscalebutton */

NspTypeGtkVolumeButton *new_type_gtkvolumebutton(type_mode mode);

/* instance for NspGtkVolumeButton */

NspGtkVolumeButton *new_gtkvolumebutton();

/*
 * Object methods redefined for gtkvolumebutton 
 */

#define NULLGTKVOLUMEBUTTON (NspGtkVolumeButton*) 0


/* from NspGtkVolumeButtonObj.c */

extern NspGtkVolumeButton *nsp_gtkvolumebutton_object (NspObject *O);
extern int IsGtkVolumeButtonObj (Stack stack, int i);
extern int IsGtkVolumeButton(NspObject *O);
extern NspGtkVolumeButton *GetGtkVolumeButtonCopy (Stack stack, int i);
extern NspGtkVolumeButton *GetGtkVolumeButton (Stack stack, int i);

#endif /* NSP_INC_NspGtkVolumeButton */ 

#ifdef NspGtkVolumeButton_Private 
static int init_gtkvolumebutton(NspGtkVolumeButton *o,NspTypeGtkVolumeButton *type);
static char *nsp_gtkvolumebutton_type_as_string(void);
static char *nsp_gtkvolumebutton_type_short_string(NspObject *v);
static AttrTab gtkvolumebutton_attrs[];
static NspMethods *gtkvolumebutton_get_methods(void);
/* static int int_gtkvolumebutton_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkVolumeButton_Private */
