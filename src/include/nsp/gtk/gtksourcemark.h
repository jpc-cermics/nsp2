/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkSourceMark
#define NSP_INC_NspGtkSourceMark

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

/* NspGtkSourceMark */

#include <nsp/gtk/gtktextmark.h>

/*
 * NspGtkSourceMark inherits from GtkTextMark
 * just change some type attributes 
 */

typedef NspGtkTextMark NspGtkSourceMark ;
typedef NspTypeGtkTextMark NspTypeGtkSourceMark ;

extern int nsp_type_gtksourcemark_id;
extern NspTypeGtkSourceMark *nsp_type_gtksourcemark;

/* type instances for gtktextmark */

NspTypeGtkSourceMark *new_type_gtksourcemark(type_mode mode);

/* instance for NspGtkSourceMark */

NspGtkSourceMark *new_gtksourcemark();

/*
 * Object methods redefined for gtksourcemark 
 */

#define NULLGTKSOURCEMARK (NspGtkSourceMark*) 0


/* from NspGtkSourceMarkObj.c */

extern NspGtkSourceMark *nsp_gtksourcemark_object (NspObject *O);
extern int IsGtkSourceMarkObj (Stack stack, int i);
extern int IsGtkSourceMark(NspObject *O);
extern NspGtkSourceMark *GetGtkSourceMarkCopy (Stack stack, int i);
extern NspGtkSourceMark *GetGtkSourceMark (Stack stack, int i);

#endif /* NSP_INC_NspGtkSourceMark */ 

#ifdef NspGtkSourceMark_Private 
static int init_gtksourcemark(NspGtkSourceMark *o,NspTypeGtkSourceMark *type);
static char *nsp_gtksourcemark_type_as_string(void);
static char *nsp_gtksourcemark_type_short_string(NspObject *v);
static AttrTab gtksourcemark_attrs[];
static NspMethods *gtksourcemark_get_methods(void);
/* static int int_gtksourcemark_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkSourceMark_Private */
