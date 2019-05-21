/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkCellRendererText
#define NSP_INC_NspGtkCellRendererText

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

/* NspGtkCellRendererText */

#include <nsp/gtk/gtkcellrenderer.h>

/*
 * NspGtkCellRendererText inherits from GtkCellRenderer
 * just change some type attributes 
 */

typedef NspGtkCellRenderer NspGtkCellRendererText ;
typedef NspTypeGtkCellRenderer NspTypeGtkCellRendererText ;

extern int nsp_type_gtkcellrenderertext_id;
extern NspTypeGtkCellRendererText *nsp_type_gtkcellrenderertext;

/* type instances for gtkcellrenderer */

NspTypeGtkCellRendererText *new_type_gtkcellrenderertext(type_mode mode);

/* instance for NspGtkCellRendererText */

NspGtkCellRendererText *new_gtkcellrenderertext();

/*
 * Object methods redefined for gtkcellrenderertext 
 */

#define NULLGTKCELLRENDERERTEXT (NspGtkCellRendererText*) 0


/* from NspGtkCellRendererTextObj.c */

extern NspGtkCellRendererText *nsp_gtkcellrenderertext_object (NspObject *O);
extern int IsGtkCellRendererTextObj (Stack stack, int i);
extern int IsGtkCellRendererText(NspObject *O);
extern NspGtkCellRendererText *GetGtkCellRendererTextCopy (Stack stack, int i);
extern NspGtkCellRendererText *GetGtkCellRendererText (Stack stack, int i);

#endif /* NSP_INC_NspGtkCellRendererText */ 

#ifdef NspGtkCellRendererText_Private 
static int init_gtkcellrenderertext(NspGtkCellRendererText *o,NspTypeGtkCellRendererText *type);
static char *nsp_gtkcellrenderertext_type_as_string(void);
static char *nsp_gtkcellrenderertext_type_short_string(NspObject *v);
static AttrTab gtkcellrenderertext_attrs[];
static NspMethods *gtkcellrenderertext_get_methods(void);
/* static int int_gtkcellrenderertext_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkCellRendererText_Private */
