/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkCssSection
#define NSP_INC_NspGtkCssSection

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

/* NspGtkCssSection */

#include <nsp/gtk/gboxed.h>

/*
 * NspGtkCssSection inherits from GBoxed
 * just change some type attributes 
 */

typedef NspGBoxed NspGtkCssSection ;
typedef NspTypeGBoxed NspTypeGtkCssSection ;

extern int nsp_type_gtkcsssection_id;
extern NspTypeGtkCssSection *nsp_type_gtkcsssection;

/* type instances for gboxed */

NspTypeGtkCssSection *new_type_gtkcsssection(type_mode mode);

/* instance for NspGtkCssSection */

NspGtkCssSection *new_gtkcsssection();

/*
 * Object methods redefined for gtkcsssection 
 */

#define NULLGTKCSSSECTION (NspGtkCssSection*) 0


/* from NspGtkCssSectionObj.c */

extern NspGtkCssSection *nsp_gtkcsssection_object (NspObject *O);
extern int IsGtkCssSectionObj (Stack stack, int i);
extern int IsGtkCssSection(NspObject *O);
extern NspGtkCssSection *GetGtkCssSectionCopy (Stack stack, int i);
extern NspGtkCssSection *GetGtkCssSection (Stack stack, int i);

#endif /* NSP_INC_NspGtkCssSection */ 

#ifdef NspGtkCssSection_Private 
static int init_gtkcsssection(NspGtkCssSection *o,NspTypeGtkCssSection *type);
static char *nsp_gtkcsssection_type_as_string(void);
static char *nsp_gtkcsssection_type_short_string(NspObject *v);
static AttrTab gtkcsssection_attrs[];
static NspMethods *gtkcsssection_get_methods(void);
/* static int int_gtkcsssection_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkCssSection_Private */
