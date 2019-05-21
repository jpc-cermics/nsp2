/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkMenuShell
#define NSP_INC_NspGtkMenuShell

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

/* NspGtkMenuShell */

#include <nsp/gtk/gtkcontainer.h>

/*
 * NspGtkMenuShell inherits from GtkContainer
 * just change some type attributes 
 */

typedef NspGtkContainer NspGtkMenuShell ;
typedef NspTypeGtkContainer NspTypeGtkMenuShell ;

extern int nsp_type_gtkmenushell_id;
extern NspTypeGtkMenuShell *nsp_type_gtkmenushell;

/* type instances for gtkcontainer */

NspTypeGtkMenuShell *new_type_gtkmenushell(type_mode mode);

/* instance for NspGtkMenuShell */

NspGtkMenuShell *new_gtkmenushell();

/*
 * Object methods redefined for gtkmenushell 
 */

#define NULLGTKMENUSHELL (NspGtkMenuShell*) 0


/* from NspGtkMenuShellObj.c */

extern NspGtkMenuShell *nsp_gtkmenushell_object (NspObject *O);
extern int IsGtkMenuShellObj (Stack stack, int i);
extern int IsGtkMenuShell(NspObject *O);
extern NspGtkMenuShell *GetGtkMenuShellCopy (Stack stack, int i);
extern NspGtkMenuShell *GetGtkMenuShell (Stack stack, int i);

#endif /* NSP_INC_NspGtkMenuShell */ 

#ifdef NspGtkMenuShell_Private 
static int init_gtkmenushell(NspGtkMenuShell *o,NspTypeGtkMenuShell *type);
static char *nsp_gtkmenushell_type_as_string(void);
static char *nsp_gtkmenushell_type_short_string(NspObject *v);
static AttrTab gtkmenushell_attrs[];
static NspMethods *gtkmenushell_get_methods(void);
/* static int int_gtkmenushell_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkMenuShell_Private */
