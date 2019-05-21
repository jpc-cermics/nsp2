/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkLinkButton
#define NSP_INC_NspGtkLinkButton

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

/* NspGtkLinkButton */

#include <nsp/gtk/gtkbutton.h>

/*
 * NspGtkLinkButton inherits from GtkButton
 * just change some type attributes 
 */

typedef NspGtkButton NspGtkLinkButton ;
typedef NspTypeGtkButton NspTypeGtkLinkButton ;

extern int nsp_type_gtklinkbutton_id;
extern NspTypeGtkLinkButton *nsp_type_gtklinkbutton;

/* type instances for gtkbutton */

NspTypeGtkLinkButton *new_type_gtklinkbutton(type_mode mode);

/* instance for NspGtkLinkButton */

NspGtkLinkButton *new_gtklinkbutton();

/*
 * Object methods redefined for gtklinkbutton 
 */

#define NULLGTKLINKBUTTON (NspGtkLinkButton*) 0


/* from NspGtkLinkButtonObj.c */

extern NspGtkLinkButton *nsp_gtklinkbutton_object (NspObject *O);
extern int IsGtkLinkButtonObj (Stack stack, int i);
extern int IsGtkLinkButton(NspObject *O);
extern NspGtkLinkButton *GetGtkLinkButtonCopy (Stack stack, int i);
extern NspGtkLinkButton *GetGtkLinkButton (Stack stack, int i);

#endif /* NSP_INC_NspGtkLinkButton */ 

#ifdef NspGtkLinkButton_Private 
static int init_gtklinkbutton(NspGtkLinkButton *o,NspTypeGtkLinkButton *type);
static char *nsp_gtklinkbutton_type_as_string(void);
static char *nsp_gtklinkbutton_type_short_string(NspObject *v);
static AttrTab gtklinkbutton_attrs[];
static NspMethods *gtklinkbutton_get_methods(void);
/* static int int_gtklinkbutton_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkLinkButton_Private */
