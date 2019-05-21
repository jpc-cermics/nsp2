/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkCellAreaContext
#define NSP_INC_NspGtkCellAreaContext

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

/* NspGtkCellAreaContext */

#include <nsp/gtk/gobject.h>

/*
 * NspGtkCellAreaContext inherits from GObject
 * just change some type attributes 
 */

typedef NspGObject NspGtkCellAreaContext ;
typedef NspTypeGObject NspTypeGtkCellAreaContext ;

extern int nsp_type_gtkcellareacontext_id;
extern NspTypeGtkCellAreaContext *nsp_type_gtkcellareacontext;

/* type instances for gobject */

NspTypeGtkCellAreaContext *new_type_gtkcellareacontext(type_mode mode);

/* instance for NspGtkCellAreaContext */

NspGtkCellAreaContext *new_gtkcellareacontext();

/*
 * Object methods redefined for gtkcellareacontext 
 */

#define NULLGTKCELLAREACONTEXT (NspGtkCellAreaContext*) 0


/* from NspGtkCellAreaContextObj.c */

extern NspGtkCellAreaContext *nsp_gtkcellareacontext_object (NspObject *O);
extern int IsGtkCellAreaContextObj (Stack stack, int i);
extern int IsGtkCellAreaContext(NspObject *O);
extern NspGtkCellAreaContext *GetGtkCellAreaContextCopy (Stack stack, int i);
extern NspGtkCellAreaContext *GetGtkCellAreaContext (Stack stack, int i);

#endif /* NSP_INC_NspGtkCellAreaContext */ 

#ifdef NspGtkCellAreaContext_Private 
static int init_gtkcellareacontext(NspGtkCellAreaContext *o,NspTypeGtkCellAreaContext *type);
static char *nsp_gtkcellareacontext_type_as_string(void);
static char *nsp_gtkcellareacontext_type_short_string(NspObject *v);
static AttrTab gtkcellareacontext_attrs[];
static NspMethods *gtkcellareacontext_get_methods(void);
/* static int int_gtkcellareacontext_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkCellAreaContext_Private */
