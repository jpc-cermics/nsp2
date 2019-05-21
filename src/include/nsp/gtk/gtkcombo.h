/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkCombo
#define NSP_INC_NspGtkCombo

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

/* NspGtkCombo */

#include <nsp/gtk/gtkhbox.h>

/*
 * NspGtkCombo inherits from GtkHBox
 * just change some type attributes 
 */

typedef NspGtkHBox NspGtkCombo ;
typedef NspTypeGtkHBox NspTypeGtkCombo ;

extern int nsp_type_gtkcombo_id;
extern NspTypeGtkCombo *nsp_type_gtkcombo;

/* type instances for gtkhbox */

NspTypeGtkCombo *new_type_gtkcombo(type_mode mode);

/* instance for NspGtkCombo */

NspGtkCombo *new_gtkcombo();

/*
 * Object methods redefined for gtkcombo 
 */

#define NULLGTKCOMBO (NspGtkCombo*) 0


/* from NspGtkComboObj.c */

extern NspGtkCombo *nsp_gtkcombo_object (NspObject *O);
extern int IsGtkComboObj (Stack stack, int i);
extern int IsGtkCombo(NspObject *O);
extern NspGtkCombo *GetGtkComboCopy (Stack stack, int i);
extern NspGtkCombo *GetGtkCombo (Stack stack, int i);

#endif /* NSP_INC_NspGtkCombo */ 

#ifdef NspGtkCombo_Private 
static int init_gtkcombo(NspGtkCombo *o,NspTypeGtkCombo *type);
static char *nsp_gtkcombo_type_as_string(void);
static char *nsp_gtkcombo_type_short_string(NspObject *v);
static AttrTab gtkcombo_attrs[];
static NspMethods *gtkcombo_get_methods(void);
/* static int int_gtkcombo_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkCombo_Private */
