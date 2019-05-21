/* -*- Mode: C -*- */
#ifndef NSP_INC_NspAtkText
#define NSP_INC_NspAtkText

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

/* NspAtkText */

#include <nsp/gtk/gobject.h>

/*
 * NspAtkText inherits from GObject
 * just change some type attributes 
 */

typedef NspGObject NspAtkText ;
typedef NspTypeGObject NspTypeAtkText ;

extern int nsp_type_atktext_id;
extern NspTypeAtkText *nsp_type_atktext;

/* type instances for gobject */

NspTypeAtkText *new_type_atktext(type_mode mode);

/* instance for NspAtkText */

NspAtkText *new_atktext();

/*
 * Object methods redefined for atktext 
 */

#define NULLATKTEXT (NspAtkText*) 0


/* from NspAtkTextObj.c */

extern NspAtkText *nsp_atktext_object (NspObject *O);
extern int IsAtkTextObj (Stack stack, int i);
extern int IsAtkText(NspObject *O);
extern NspAtkText *GetAtkTextCopy (Stack stack, int i);
extern NspAtkText *GetAtkText (Stack stack, int i);

#endif /* NSP_INC_NspAtkText */ 

#ifdef NspAtkText_Private 
static int init_atktext(NspAtkText *o,NspTypeAtkText *type);
static char *nsp_atktext_type_as_string(void);
static char *nsp_atktext_type_short_string(NspObject *v);
static AttrTab atktext_attrs[];
static NspMethods *atktext_get_methods(void);
/* static int int_atktext_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspAtkText_Private */
