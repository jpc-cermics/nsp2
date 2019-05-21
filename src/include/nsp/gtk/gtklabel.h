/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkLabel
#define NSP_INC_NspGtkLabel

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

/* NspGtkLabel */

#include <nsp/gtk/gtkmisc.h>

/*
 * NspGtkLabel inherits from GtkMisc
 * just change some type attributes 
 */

typedef NspGtkMisc NspGtkLabel ;
typedef NspTypeGtkMisc NspTypeGtkLabel ;

extern int nsp_type_gtklabel_id;
extern NspTypeGtkLabel *nsp_type_gtklabel;

/* type instances for gtkmisc */

NspTypeGtkLabel *new_type_gtklabel(type_mode mode);

/* instance for NspGtkLabel */

NspGtkLabel *new_gtklabel();

/*
 * Object methods redefined for gtklabel 
 */

#define NULLGTKLABEL (NspGtkLabel*) 0


/* from NspGtkLabelObj.c */

extern NspGtkLabel *nsp_gtklabel_object (NspObject *O);
extern int IsGtkLabelObj (Stack stack, int i);
extern int IsGtkLabel(NspObject *O);
extern NspGtkLabel *GetGtkLabelCopy (Stack stack, int i);
extern NspGtkLabel *GetGtkLabel (Stack stack, int i);

#endif /* NSP_INC_NspGtkLabel */ 

#ifdef NspGtkLabel_Private 
static int init_gtklabel(NspGtkLabel *o,NspTypeGtkLabel *type);
static char *nsp_gtklabel_type_as_string(void);
static char *nsp_gtklabel_type_short_string(NspObject *v);
static AttrTab gtklabel_attrs[];
static NspMethods *gtklabel_get_methods(void);
/* static int int_gtklabel_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkLabel_Private */
