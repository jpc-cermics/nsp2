/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkCellRendererCombo
#define NSP_INC_NspGtkCellRendererCombo

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

/* NspGtkCellRendererCombo */

#include <nsp/gtk/gtkcellrenderertext.h>

/*
 * NspGtkCellRendererCombo inherits from GtkCellRendererText
 * just change some type attributes 
 */

typedef NspGtkCellRendererText NspGtkCellRendererCombo ;
typedef NspTypeGtkCellRendererText NspTypeGtkCellRendererCombo ;

extern int nsp_type_gtkcellrenderercombo_id;
extern NspTypeGtkCellRendererCombo *nsp_type_gtkcellrenderercombo;

/* type instances for gtkcellrenderertext */

NspTypeGtkCellRendererCombo *new_type_gtkcellrenderercombo(type_mode mode);

/* instance for NspGtkCellRendererCombo */

NspGtkCellRendererCombo *new_gtkcellrenderercombo();

/*
 * Object methods redefined for gtkcellrenderercombo 
 */

#define NULLGTKCELLRENDERERCOMBO (NspGtkCellRendererCombo*) 0


/* from NspGtkCellRendererComboObj.c */

extern NspGtkCellRendererCombo *nsp_gtkcellrenderercombo_object (NspObject *O);
extern int IsGtkCellRendererComboObj (Stack stack, int i);
extern int IsGtkCellRendererCombo(NspObject *O);
extern NspGtkCellRendererCombo *GetGtkCellRendererComboCopy (Stack stack, int i);
extern NspGtkCellRendererCombo *GetGtkCellRendererCombo (Stack stack, int i);

#endif /* NSP_INC_NspGtkCellRendererCombo */ 

#ifdef NspGtkCellRendererCombo_Private 
static int init_gtkcellrenderercombo(NspGtkCellRendererCombo *o,NspTypeGtkCellRendererCombo *type);
static char *nsp_gtkcellrenderercombo_type_as_string(void);
static char *nsp_gtkcellrenderercombo_type_short_string(NspObject *v);
static AttrTab gtkcellrenderercombo_attrs[];
static NspMethods *gtkcellrenderercombo_get_methods(void);
/* static int int_gtkcellrenderercombo_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkCellRendererCombo_Private */
