/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkRadioAction
#define NSP_INC_NspGtkRadioAction

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

/* NspGtkRadioAction */

#include <nsp/gtk/gtktoggleaction.h>

/*
 * NspGtkRadioAction inherits from GtkToggleAction
 * just change some type attributes 
 */

typedef NspGtkToggleAction NspGtkRadioAction ;
typedef NspTypeGtkToggleAction NspTypeGtkRadioAction ;

extern int nsp_type_gtkradioaction_id;
extern NspTypeGtkRadioAction *nsp_type_gtkradioaction;

/* type instances for gtktoggleaction */

NspTypeGtkRadioAction *new_type_gtkradioaction(type_mode mode);

/* instance for NspGtkRadioAction */

NspGtkRadioAction *new_gtkradioaction();

/*
 * Object methods redefined for gtkradioaction 
 */

#define NULLGTKRADIOACTION (NspGtkRadioAction*) 0


/* from NspGtkRadioActionObj.c */

extern NspGtkRadioAction *nsp_gtkradioaction_object (NspObject *O);
extern int IsGtkRadioActionObj (Stack stack, int i);
extern int IsGtkRadioAction(NspObject *O);
extern NspGtkRadioAction *GetGtkRadioActionCopy (Stack stack, int i);
extern NspGtkRadioAction *GetGtkRadioAction (Stack stack, int i);

#endif /* NSP_INC_NspGtkRadioAction */ 

#ifdef NspGtkRadioAction_Private 
static int init_gtkradioaction(NspGtkRadioAction *o,NspTypeGtkRadioAction *type);
static char *nsp_gtkradioaction_type_as_string(void);
static char *nsp_gtkradioaction_type_short_string(NspObject *v);
static AttrTab gtkradioaction_attrs[];
static NspMethods *gtkradioaction_get_methods(void);
/* static int int_gtkradioaction_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkRadioAction_Private */
