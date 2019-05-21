/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkToolShell
#define NSP_INC_NspGtkToolShell

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

/* NspGtkToolShell */

#include <nsp/gtk/gobject.h>

/*
 * NspGtkToolShell inherits from GObject
 * just change some type attributes 
 */

typedef NspGObject NspGtkToolShell ;
typedef NspTypeGObject NspTypeGtkToolShell ;

extern int nsp_type_gtktoolshell_id;
extern NspTypeGtkToolShell *nsp_type_gtktoolshell;

/* type instances for gobject */

NspTypeGtkToolShell *new_type_gtktoolshell(type_mode mode);

/* instance for NspGtkToolShell */

NspGtkToolShell *new_gtktoolshell();

/*
 * Object methods redefined for gtktoolshell 
 */

#define NULLGTKTOOLSHELL (NspGtkToolShell*) 0


/* from NspGtkToolShellObj.c */

extern NspGtkToolShell *nsp_gtktoolshell_object (NspObject *O);
extern int IsGtkToolShellObj (Stack stack, int i);
extern int IsGtkToolShell(NspObject *O);
extern NspGtkToolShell *GetGtkToolShellCopy (Stack stack, int i);
extern NspGtkToolShell *GetGtkToolShell (Stack stack, int i);

#endif /* NSP_INC_NspGtkToolShell */ 

#ifdef NspGtkToolShell_Private 
static int init_gtktoolshell(NspGtkToolShell *o,NspTypeGtkToolShell *type);
static char *nsp_gtktoolshell_type_as_string(void);
static char *nsp_gtktoolshell_type_short_string(NspObject *v);
static AttrTab gtktoolshell_attrs[];
static NspMethods *gtktoolshell_get_methods(void);
/* static int int_gtktoolshell_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkToolShell_Private */
