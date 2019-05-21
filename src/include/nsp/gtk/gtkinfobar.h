/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkInfoBar
#define NSP_INC_NspGtkInfoBar

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

/* NspGtkInfoBar */

#include <nsp/gtk/gtkbox.h>

/*
 * NspGtkInfoBar inherits from GtkBox
 * just change some type attributes 
 */

typedef NspGtkBox NspGtkInfoBar ;
typedef NspTypeGtkBox NspTypeGtkInfoBar ;

extern int nsp_type_gtkinfobar_id;
extern NspTypeGtkInfoBar *nsp_type_gtkinfobar;

/* type instances for gtkbox */

NspTypeGtkInfoBar *new_type_gtkinfobar(type_mode mode);

/* instance for NspGtkInfoBar */

NspGtkInfoBar *new_gtkinfobar();

/*
 * Object methods redefined for gtkinfobar 
 */

#define NULLGTKINFOBAR (NspGtkInfoBar*) 0


/* from NspGtkInfoBarObj.c */

extern NspGtkInfoBar *nsp_gtkinfobar_object (NspObject *O);
extern int IsGtkInfoBarObj (Stack stack, int i);
extern int IsGtkInfoBar(NspObject *O);
extern NspGtkInfoBar *GetGtkInfoBarCopy (Stack stack, int i);
extern NspGtkInfoBar *GetGtkInfoBar (Stack stack, int i);

#endif /* NSP_INC_NspGtkInfoBar */ 

#ifdef NspGtkInfoBar_Private 
static int init_gtkinfobar(NspGtkInfoBar *o,NspTypeGtkInfoBar *type);
static char *nsp_gtkinfobar_type_as_string(void);
static char *nsp_gtkinfobar_type_short_string(NspObject *v);
static AttrTab gtkinfobar_attrs[];
static NspMethods *gtkinfobar_get_methods(void);
/* static int int_gtkinfobar_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkInfoBar_Private */
