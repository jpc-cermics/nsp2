/* -*- Mode: C -*- */
#ifndef NSP_INC_Nspcairo_matrix_t
#define NSP_INC_Nspcairo_matrix_t

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

/* Nspcairo_matrix_t */

#include <nsp/gtk/gboxed.h>

/*
 * Nspcairo_matrix_t inherits from GBoxed
 * just change some type attributes 
 */

typedef NspGBoxed Nspcairo_matrix_t ;
typedef NspTypeGBoxed NspTypeCairo_matrix_t ;

extern int nsp_type_cairo_matrix_t_id;
extern NspTypeCairo_matrix_t *nsp_type_cairo_matrix_t;

/* type instances for gboxed */

NspTypeCairo_matrix_t *new_type_cairo_matrix_t(type_mode mode);

/* instance for Nspcairo_matrix_t */

Nspcairo_matrix_t *new_cairo_matrix_t();

/*
 * Object methods redefined for cairo_matrix_t 
 */

#define NULLCAIRO_MATRIX_T (Nspcairo_matrix_t*) 0


/* from Nspcairo_matrix_tObj.c */

extern Nspcairo_matrix_t *nsp_cairo_matrix_t_object (NspObject *O);
extern int IsCairo_matrix_tObj (Stack stack, int i);
extern int IsCairo_matrix_t(NspObject *O);
extern Nspcairo_matrix_t *GetCairo_matrix_tCopy (Stack stack, int i);
extern Nspcairo_matrix_t *GetCairo_matrix_t (Stack stack, int i);

#endif /* NSP_INC_Nspcairo_matrix_t */ 

#ifdef Nspcairo_matrix_t_Private 
static int init_cairo_matrix_t(Nspcairo_matrix_t *o,NspTypeCairo_matrix_t *type);
static char *nsp_cairo_matrix_t_type_as_string(void);
static char *nsp_cairo_matrix_t_type_short_string(NspObject *v);
static AttrTab cairo_matrix_t_attrs[];
static NspMethods *cairo_matrix_t_get_methods(void);
/* static int int_cairo_matrix_t_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* Nspcairo_matrix_t_Private */
