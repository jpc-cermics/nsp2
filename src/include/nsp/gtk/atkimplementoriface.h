/* -*- Mode: C -*- */
#ifndef NSP_INC_NspAtkImplementorIface
#define NSP_INC_NspAtkImplementorIface

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

/* NspAtkImplementorIface */

#include <nsp/gtk/gobject.h>

/*
 * NspAtkImplementorIface inherits from GObject
 * just change some type attributes 
 */

typedef NspGObject NspAtkImplementorIface ;
typedef NspTypeGObject NspTypeAtkImplementorIface ;

extern int nsp_type_atkimplementoriface_id;
extern NspTypeAtkImplementorIface *nsp_type_atkimplementoriface;

/* type instances for gobject */

NspTypeAtkImplementorIface *new_type_atkimplementoriface(type_mode mode);

/* instance for NspAtkImplementorIface */

NspAtkImplementorIface *new_atkimplementoriface();

/*
 * Object methods redefined for atkimplementoriface 
 */

#define NULLATKIMPLEMENTORIFACE (NspAtkImplementorIface*) 0


/* from NspAtkImplementorIfaceObj.c */

extern NspAtkImplementorIface *nsp_atkimplementoriface_object (NspObject *O);
extern int IsAtkImplementorIfaceObj (Stack stack, int i);
extern int IsAtkImplementorIface(NspObject *O);
extern NspAtkImplementorIface *GetAtkImplementorIfaceCopy (Stack stack, int i);
extern NspAtkImplementorIface *GetAtkImplementorIface (Stack stack, int i);

#endif /* NSP_INC_NspAtkImplementorIface */ 

#ifdef NspAtkImplementorIface_Private 
static int init_atkimplementoriface(NspAtkImplementorIface *o,NspTypeAtkImplementorIface *type);
static char *nsp_atkimplementoriface_type_as_string(void);
static char *nsp_atkimplementoriface_type_short_string(NspObject *v);
static AttrTab atkimplementoriface_attrs[];
static NspMethods *atkimplementoriface_get_methods(void);
/* static int int_atkimplementoriface_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspAtkImplementorIface_Private */
