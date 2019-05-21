/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkSourceStyleScheme
#define NSP_INC_NspGtkSourceStyleScheme

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

/* NspGtkSourceStyleScheme */

#include <nsp/gtk/gobject.h>

/*
 * NspGtkSourceStyleScheme inherits from GObject
 * just change some type attributes 
 */

typedef NspGObject NspGtkSourceStyleScheme ;
typedef NspTypeGObject NspTypeGtkSourceStyleScheme ;

extern int nsp_type_gtksourcestylescheme_id;
extern NspTypeGtkSourceStyleScheme *nsp_type_gtksourcestylescheme;

/* type instances for gobject */

NspTypeGtkSourceStyleScheme *new_type_gtksourcestylescheme(type_mode mode);

/* instance for NspGtkSourceStyleScheme */

NspGtkSourceStyleScheme *new_gtksourcestylescheme();

/*
 * Object methods redefined for gtksourcestylescheme 
 */

#define NULLGTKSOURCESTYLESCHEME (NspGtkSourceStyleScheme*) 0


/* from NspGtkSourceStyleSchemeObj.c */

extern NspGtkSourceStyleScheme *nsp_gtksourcestylescheme_object (NspObject *O);
extern int IsGtkSourceStyleSchemeObj (Stack stack, int i);
extern int IsGtkSourceStyleScheme(NspObject *O);
extern NspGtkSourceStyleScheme *GetGtkSourceStyleSchemeCopy (Stack stack, int i);
extern NspGtkSourceStyleScheme *GetGtkSourceStyleScheme (Stack stack, int i);

#endif /* NSP_INC_NspGtkSourceStyleScheme */ 

#ifdef NspGtkSourceStyleScheme_Private 
static int init_gtksourcestylescheme(NspGtkSourceStyleScheme *o,NspTypeGtkSourceStyleScheme *type);
static char *nsp_gtksourcestylescheme_type_as_string(void);
static char *nsp_gtksourcestylescheme_type_short_string(NspObject *v);
static AttrTab gtksourcestylescheme_attrs[];
static NspMethods *gtksourcestylescheme_get_methods(void);
/* static int int_gtksourcestylescheme_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkSourceStyleScheme_Private */
