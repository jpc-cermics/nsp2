/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkToggleAction
#define NSP_INC_NspGtkToggleAction

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

/* NspGtkToggleAction */

#include <nsp/gtk/gtkaction.h>

/*
 * NspGtkToggleAction inherits from GtkAction
 * just change some type attributes 
 */

typedef NspGtkAction NspGtkToggleAction ;
typedef NspTypeGtkAction NspTypeGtkToggleAction ;

extern int nsp_type_gtktoggleaction_id;
extern NspTypeGtkToggleAction *nsp_type_gtktoggleaction;

/* type instances for gtkaction */

NspTypeGtkToggleAction *new_type_gtktoggleaction(type_mode mode);

/* instance for NspGtkToggleAction */

NspGtkToggleAction *new_gtktoggleaction();

/*
 * Object methods redefined for gtktoggleaction 
 */

#define NULLGTKTOGGLEACTION (NspGtkToggleAction*) 0


/* from NspGtkToggleActionObj.c */

extern NspGtkToggleAction *nsp_gtktoggleaction_object (NspObject *O);
extern int IsGtkToggleActionObj (Stack stack, int i);
extern int IsGtkToggleAction(NspObject *O);
extern NspGtkToggleAction *GetGtkToggleActionCopy (Stack stack, int i);
extern NspGtkToggleAction *GetGtkToggleAction (Stack stack, int i);

#endif /* NSP_INC_NspGtkToggleAction */ 

#ifdef NspGtkToggleAction_Private 
static int init_gtktoggleaction(NspGtkToggleAction *o,NspTypeGtkToggleAction *type);
static char *nsp_gtktoggleaction_type_as_string(void);
static char *nsp_gtktoggleaction_type_short_string(NspObject *v);
static AttrTab gtktoggleaction_attrs[];
static NspMethods *gtktoggleaction_get_methods(void);
/* static int int_gtktoggleaction_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkToggleAction_Private */
