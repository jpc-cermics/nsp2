/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkTextTag
#define NSP_INC_NspGtkTextTag

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

/* NspGtkTextTag */

#include <nsp/gtk/gobject.h>

/*
 * NspGtkTextTag inherits from GObject
 * just change some type attributes 
 */

typedef NspGObject NspGtkTextTag ;
typedef NspTypeGObject NspTypeGtkTextTag ;

extern int nsp_type_gtktexttag_id;
extern NspTypeGtkTextTag *nsp_type_gtktexttag;

/* type instances for gobject */

NspTypeGtkTextTag *new_type_gtktexttag(type_mode mode);

/* instance for NspGtkTextTag */

NspGtkTextTag *new_gtktexttag();

/*
 * Object methods redefined for gtktexttag 
 */

#define NULLGTKTEXTTAG (NspGtkTextTag*) 0


/* from NspGtkTextTagObj.c */

extern NspGtkTextTag *nsp_gtktexttag_object (NspObject *O);
extern int IsGtkTextTagObj (Stack stack, int i);
extern int IsGtkTextTag(NspObject *O);
extern NspGtkTextTag *GetGtkTextTagCopy (Stack stack, int i);
extern NspGtkTextTag *GetGtkTextTag (Stack stack, int i);

#endif /* NSP_INC_NspGtkTextTag */ 

#ifdef NspGtkTextTag_Private 
static int init_gtktexttag(NspGtkTextTag *o,NspTypeGtkTextTag *type);
static char *nsp_gtktexttag_type_as_string(void);
static char *nsp_gtktexttag_type_short_string(NspObject *v);
static AttrTab gtktexttag_attrs[];
static NspMethods *gtktexttag_get_methods(void);
/* static int int_gtktexttag_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkTextTag_Private */
