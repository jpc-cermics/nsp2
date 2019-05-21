/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkTextMark
#define NSP_INC_NspGtkTextMark

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

/* NspGtkTextMark */

#include <nsp/gtk/gobject.h>

/*
 * NspGtkTextMark inherits from GObject
 * just change some type attributes 
 */

typedef NspGObject NspGtkTextMark ;
typedef NspTypeGObject NspTypeGtkTextMark ;

extern int nsp_type_gtktextmark_id;
extern NspTypeGtkTextMark *nsp_type_gtktextmark;

/* type instances for gobject */

NspTypeGtkTextMark *new_type_gtktextmark(type_mode mode);

/* instance for NspGtkTextMark */

NspGtkTextMark *new_gtktextmark();

/*
 * Object methods redefined for gtktextmark 
 */

#define NULLGTKTEXTMARK (NspGtkTextMark*) 0


/* from NspGtkTextMarkObj.c */

extern NspGtkTextMark *nsp_gtktextmark_object (NspObject *O);
extern int IsGtkTextMarkObj (Stack stack, int i);
extern int IsGtkTextMark(NspObject *O);
extern NspGtkTextMark *GetGtkTextMarkCopy (Stack stack, int i);
extern NspGtkTextMark *GetGtkTextMark (Stack stack, int i);

#endif /* NSP_INC_NspGtkTextMark */ 

#ifdef NspGtkTextMark_Private 
static int init_gtktextmark(NspGtkTextMark *o,NspTypeGtkTextMark *type);
static char *nsp_gtktextmark_type_as_string(void);
static char *nsp_gtktextmark_type_short_string(NspObject *v);
static AttrTab gtktextmark_attrs[];
static NspMethods *gtktextmark_get_methods(void);
/* static int int_gtktextmark_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkTextMark_Private */
