/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkNotebook
#define NSP_INC_NspGtkNotebook

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

/* NspGtkNotebook */

#include <nsp/gtk/gtkcontainer.h>

/*
 * NspGtkNotebook inherits from GtkContainer
 * just change some type attributes 
 */

typedef NspGtkContainer NspGtkNotebook ;
typedef NspTypeGtkContainer NspTypeGtkNotebook ;

extern int nsp_type_gtknotebook_id;
extern NspTypeGtkNotebook *nsp_type_gtknotebook;

/* type instances for gtkcontainer */

NspTypeGtkNotebook *new_type_gtknotebook(type_mode mode);

/* instance for NspGtkNotebook */

NspGtkNotebook *new_gtknotebook();

/*
 * Object methods redefined for gtknotebook 
 */

#define NULLGTKNOTEBOOK (NspGtkNotebook*) 0


/* from NspGtkNotebookObj.c */

extern NspGtkNotebook *nsp_gtknotebook_object (NspObject *O);
extern int IsGtkNotebookObj (Stack stack, int i);
extern int IsGtkNotebook(NspObject *O);
extern NspGtkNotebook *GetGtkNotebookCopy (Stack stack, int i);
extern NspGtkNotebook *GetGtkNotebook (Stack stack, int i);

#endif /* NSP_INC_NspGtkNotebook */ 

#ifdef NspGtkNotebook_Private 
static int init_gtknotebook(NspGtkNotebook *o,NspTypeGtkNotebook *type);
static char *nsp_gtknotebook_type_as_string(void);
static char *nsp_gtknotebook_type_short_string(NspObject *v);
static AttrTab gtknotebook_attrs[];
static NspMethods *gtknotebook_get_methods(void);
/* static int int_gtknotebook_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkNotebook_Private */
