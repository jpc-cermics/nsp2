/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkHBox
#define NSP_INC_NspGtkHBox

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

/* NspGtkHBox */

#include <nsp/gtk/gtkbox.h>

/*
 * NspGtkHBox inherits from GtkBox
 * just change some type attributes 
 */

typedef NspGtkBox NspGtkHBox ;
typedef NspTypeGtkBox NspTypeGtkHBox ;

extern int nsp_type_gtkhbox_id;
extern NspTypeGtkHBox *nsp_type_gtkhbox;

/* type instances for gtkbox */

NspTypeGtkHBox *new_type_gtkhbox(type_mode mode);

/* instance for NspGtkHBox */

NspGtkHBox *new_gtkhbox();

/*
 * Object methods redefined for gtkhbox 
 */

#define NULLGTKHBOX (NspGtkHBox*) 0


/* from NspGtkHBoxObj.c */

extern NspGtkHBox *nsp_gtkhbox_object (NspObject *O);
extern int IsGtkHBoxObj (Stack stack, int i);
extern int IsGtkHBox(NspObject *O);
extern NspGtkHBox *GetGtkHBoxCopy (Stack stack, int i);
extern NspGtkHBox *GetGtkHBox (Stack stack, int i);

#endif /* NSP_INC_NspGtkHBox */ 

#ifdef NspGtkHBox_Private 
static int init_gtkhbox(NspGtkHBox *o,NspTypeGtkHBox *type);
static char *nsp_gtkhbox_type_as_string(void);
static char *nsp_gtkhbox_type_short_string(NspObject *v);
static AttrTab gtkhbox_attrs[];
static NspMethods *gtkhbox_get_methods(void);
/* static int int_gtkhbox_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkHBox_Private */
