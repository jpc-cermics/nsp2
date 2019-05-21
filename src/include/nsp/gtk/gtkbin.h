/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkBin
#define NSP_INC_NspGtkBin

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

/* NspGtkBin */

#include <nsp/gtk/gtkcontainer.h>

/*
 * NspGtkBin inherits from GtkContainer
 * just change some type attributes 
 */

typedef NspGtkContainer NspGtkBin ;
typedef NspTypeGtkContainer NspTypeGtkBin ;

extern int nsp_type_gtkbin_id;
extern NspTypeGtkBin *nsp_type_gtkbin;

/* type instances for gtkcontainer */

NspTypeGtkBin *new_type_gtkbin(type_mode mode);

/* instance for NspGtkBin */

NspGtkBin *new_gtkbin();

/*
 * Object methods redefined for gtkbin 
 */

#define NULLGTKBIN (NspGtkBin*) 0


/* from NspGtkBinObj.c */

extern NspGtkBin *nsp_gtkbin_object (NspObject *O);
extern int IsGtkBinObj (Stack stack, int i);
extern int IsGtkBin(NspObject *O);
extern NspGtkBin *GetGtkBinCopy (Stack stack, int i);
extern NspGtkBin *GetGtkBin (Stack stack, int i);

#endif /* NSP_INC_NspGtkBin */ 

#ifdef NspGtkBin_Private 
static int init_gtkbin(NspGtkBin *o,NspTypeGtkBin *type);
static char *nsp_gtkbin_type_as_string(void);
static char *nsp_gtkbin_type_short_string(NspObject *v);
static AttrTab gtkbin_attrs[];
static NspMethods *gtkbin_get_methods(void);
/* static int int_gtkbin_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkBin_Private */
