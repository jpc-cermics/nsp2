/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGConverter
#define NSP_INC_NspGConverter

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

/* NspGConverter */

#include <nsp/gtk/gobject.h>

/*
 * NspGConverter inherits from GObject
 * just change some type attributes 
 */

typedef NspGObject NspGConverter ;
typedef NspTypeGObject NspTypeGConverter ;

extern int nsp_type_gconverter_id;
extern NspTypeGConverter *nsp_type_gconverter;

/* type instances for gobject */

NspTypeGConverter *new_type_gconverter(type_mode mode);

/* instance for NspGConverter */

NspGConverter *new_gconverter();

/*
 * Object methods redefined for gconverter 
 */

#define NULLGCONVERTER (NspGConverter*) 0


/* from NspGConverterObj.c */

extern NspGConverter *nsp_gconverter_object (NspObject *O);
extern int IsGConverterObj (Stack stack, int i);
extern int IsGConverter(NspObject *O);
extern NspGConverter *GetGConverterCopy (Stack stack, int i);
extern NspGConverter *GetGConverter (Stack stack, int i);

#endif /* NSP_INC_NspGConverter */ 

#ifdef NspGConverter_Private 
static int init_gconverter(NspGConverter *o,NspTypeGConverter *type);
static char *nsp_gconverter_type_as_string(void);
static char *nsp_gconverter_type_short_string(NspObject *v);
static AttrTab gconverter_attrs[];
static NspMethods *gconverter_get_methods(void);
/* static int int_gconverter_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGConverter_Private */
