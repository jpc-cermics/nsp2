/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkFlowBox
#define NSP_INC_NspGtkFlowBox

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

/* NspGtkFlowBox */

#include <nsp/gtk/gtkcontainer.h>

/*
 * NspGtkFlowBox inherits from GtkContainer
 * just change some type attributes 
 */

typedef NspGtkContainer NspGtkFlowBox ;
typedef NspTypeGtkContainer NspTypeGtkFlowBox ;

extern int nsp_type_gtkflowbox_id;
extern NspTypeGtkFlowBox *nsp_type_gtkflowbox;

/* type instances for gtkcontainer */

NspTypeGtkFlowBox *new_type_gtkflowbox(type_mode mode);

/* instance for NspGtkFlowBox */

NspGtkFlowBox *new_gtkflowbox();

/*
 * Object methods redefined for gtkflowbox 
 */

#define NULLGTKFLOWBOX (NspGtkFlowBox*) 0


/* from NspGtkFlowBoxObj.c */

extern NspGtkFlowBox *nsp_gtkflowbox_object (NspObject *O);
extern int IsGtkFlowBoxObj (Stack stack, int i);
extern int IsGtkFlowBox(NspObject *O);
extern NspGtkFlowBox *GetGtkFlowBoxCopy (Stack stack, int i);
extern NspGtkFlowBox *GetGtkFlowBox (Stack stack, int i);

#endif /* NSP_INC_NspGtkFlowBox */ 

#ifdef NspGtkFlowBox_Private 
static int init_gtkflowbox(NspGtkFlowBox *o,NspTypeGtkFlowBox *type);
static char *nsp_gtkflowbox_type_as_string(void);
static char *nsp_gtkflowbox_type_short_string(NspObject *v);
static AttrTab gtkflowbox_attrs[];
static NspMethods *gtkflowbox_get_methods(void);
/* static int int_gtkflowbox_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkFlowBox_Private */
