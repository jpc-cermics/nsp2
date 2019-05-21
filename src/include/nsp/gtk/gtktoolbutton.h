/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkToolButton
#define NSP_INC_NspGtkToolButton

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

/* NspGtkToolButton */

#include <nsp/gtk/gtktoolitem.h>

/*
 * NspGtkToolButton inherits from GtkToolItem
 * just change some type attributes 
 */

typedef NspGtkToolItem NspGtkToolButton ;
typedef NspTypeGtkToolItem NspTypeGtkToolButton ;

extern int nsp_type_gtktoolbutton_id;
extern NspTypeGtkToolButton *nsp_type_gtktoolbutton;

/* type instances for gtktoolitem */

NspTypeGtkToolButton *new_type_gtktoolbutton(type_mode mode);

/* instance for NspGtkToolButton */

NspGtkToolButton *new_gtktoolbutton();

/*
 * Object methods redefined for gtktoolbutton 
 */

#define NULLGTKTOOLBUTTON (NspGtkToolButton*) 0


/* from NspGtkToolButtonObj.c */

extern NspGtkToolButton *nsp_gtktoolbutton_object (NspObject *O);
extern int IsGtkToolButtonObj (Stack stack, int i);
extern int IsGtkToolButton(NspObject *O);
extern NspGtkToolButton *GetGtkToolButtonCopy (Stack stack, int i);
extern NspGtkToolButton *GetGtkToolButton (Stack stack, int i);

#endif /* NSP_INC_NspGtkToolButton */ 

#ifdef NspGtkToolButton_Private 
static int init_gtktoolbutton(NspGtkToolButton *o,NspTypeGtkToolButton *type);
static char *nsp_gtktoolbutton_type_as_string(void);
static char *nsp_gtktoolbutton_type_short_string(NspObject *v);
static AttrTab gtktoolbutton_attrs[];
static NspMethods *gtktoolbutton_get_methods(void);
/* static int int_gtktoolbutton_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkToolButton_Private */
