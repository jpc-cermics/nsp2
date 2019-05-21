/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkTextIter
#define NSP_INC_NspGtkTextIter

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

/* NspGtkTextIter */

#include <nsp/gtk/gboxed.h>

/*
 * NspGtkTextIter inherits from GBoxed
 * just change some type attributes 
 */

typedef NspGBoxed NspGtkTextIter ;
typedef NspTypeGBoxed NspTypeGtkTextIter ;

extern int nsp_type_gtktextiter_id;
extern NspTypeGtkTextIter *nsp_type_gtktextiter;

/* type instances for gboxed */

NspTypeGtkTextIter *new_type_gtktextiter(type_mode mode);

/* instance for NspGtkTextIter */

NspGtkTextIter *new_gtktextiter();

/*
 * Object methods redefined for gtktextiter 
 */

#define NULLGTKTEXTITER (NspGtkTextIter*) 0


/* from NspGtkTextIterObj.c */

extern NspGtkTextIter *nsp_gtktextiter_object (NspObject *O);
extern int IsGtkTextIterObj (Stack stack, int i);
extern int IsGtkTextIter(NspObject *O);
extern NspGtkTextIter *GetGtkTextIterCopy (Stack stack, int i);
extern NspGtkTextIter *GetGtkTextIter (Stack stack, int i);

#endif /* NSP_INC_NspGtkTextIter */ 

#ifdef NspGtkTextIter_Private 
static int init_gtktextiter(NspGtkTextIter *o,NspTypeGtkTextIter *type);
static char *nsp_gtktextiter_type_as_string(void);
static char *nsp_gtktextiter_type_short_string(NspObject *v);
static AttrTab gtktextiter_attrs[];
static NspMethods *gtktextiter_get_methods(void);
/* static int int_gtktextiter_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkTextIter_Private */
