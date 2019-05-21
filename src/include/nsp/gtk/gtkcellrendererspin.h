/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkCellRendererSpin
#define NSP_INC_NspGtkCellRendererSpin

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

/* NspGtkCellRendererSpin */

#include <nsp/gtk/gtkcellrenderertext.h>

/*
 * NspGtkCellRendererSpin inherits from GtkCellRendererText
 * just change some type attributes 
 */

typedef NspGtkCellRendererText NspGtkCellRendererSpin ;
typedef NspTypeGtkCellRendererText NspTypeGtkCellRendererSpin ;

extern int nsp_type_gtkcellrendererspin_id;
extern NspTypeGtkCellRendererSpin *nsp_type_gtkcellrendererspin;

/* type instances for gtkcellrenderertext */

NspTypeGtkCellRendererSpin *new_type_gtkcellrendererspin(type_mode mode);

/* instance for NspGtkCellRendererSpin */

NspGtkCellRendererSpin *new_gtkcellrendererspin();

/*
 * Object methods redefined for gtkcellrendererspin 
 */

#define NULLGTKCELLRENDERERSPIN (NspGtkCellRendererSpin*) 0


/* from NspGtkCellRendererSpinObj.c */

extern NspGtkCellRendererSpin *nsp_gtkcellrendererspin_object (NspObject *O);
extern int IsGtkCellRendererSpinObj (Stack stack, int i);
extern int IsGtkCellRendererSpin(NspObject *O);
extern NspGtkCellRendererSpin *GetGtkCellRendererSpinCopy (Stack stack, int i);
extern NspGtkCellRendererSpin *GetGtkCellRendererSpin (Stack stack, int i);

#endif /* NSP_INC_NspGtkCellRendererSpin */ 

#ifdef NspGtkCellRendererSpin_Private 
static int init_gtkcellrendererspin(NspGtkCellRendererSpin *o,NspTypeGtkCellRendererSpin *type);
static char *nsp_gtkcellrendererspin_type_as_string(void);
static char *nsp_gtkcellrendererspin_type_short_string(NspObject *v);
static AttrTab gtkcellrendererspin_attrs[];
static NspMethods *gtkcellrendererspin_get_methods(void);
/* static int int_gtkcellrendererspin_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkCellRendererSpin_Private */
