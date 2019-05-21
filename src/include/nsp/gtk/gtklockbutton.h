/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkLockButton
#define NSP_INC_NspGtkLockButton

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

/* NspGtkLockButton */

#include <nsp/gtk/gtkbutton.h>

/*
 * NspGtkLockButton inherits from GtkButton
 * just change some type attributes 
 */

typedef NspGtkButton NspGtkLockButton ;
typedef NspTypeGtkButton NspTypeGtkLockButton ;

extern int nsp_type_gtklockbutton_id;
extern NspTypeGtkLockButton *nsp_type_gtklockbutton;

/* type instances for gtkbutton */

NspTypeGtkLockButton *new_type_gtklockbutton(type_mode mode);

/* instance for NspGtkLockButton */

NspGtkLockButton *new_gtklockbutton();

/*
 * Object methods redefined for gtklockbutton 
 */

#define NULLGTKLOCKBUTTON (NspGtkLockButton*) 0


/* from NspGtkLockButtonObj.c */

extern NspGtkLockButton *nsp_gtklockbutton_object (NspObject *O);
extern int IsGtkLockButtonObj (Stack stack, int i);
extern int IsGtkLockButton(NspObject *O);
extern NspGtkLockButton *GetGtkLockButtonCopy (Stack stack, int i);
extern NspGtkLockButton *GetGtkLockButton (Stack stack, int i);

#endif /* NSP_INC_NspGtkLockButton */ 

#ifdef NspGtkLockButton_Private 
static int init_gtklockbutton(NspGtkLockButton *o,NspTypeGtkLockButton *type);
static char *nsp_gtklockbutton_type_as_string(void);
static char *nsp_gtklockbutton_type_short_string(NspObject *v);
static AttrTab gtklockbutton_attrs[];
static NspMethods *gtklockbutton_get_methods(void);
/* static int int_gtklockbutton_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkLockButton_Private */
