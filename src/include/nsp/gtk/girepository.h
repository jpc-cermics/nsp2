/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGIRepository
#define NSP_INC_NspGIRepository

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

/* NspGIRepository */

#include <nsp/gtk/gobject.h>

/*
 * NspGIRepository inherits from GObject
 * just change some type attributes 
 */

typedef NspGObject NspGIRepository ;
typedef NspTypeGObject NspTypeGIRepository ;

extern int nsp_type_girepository_id;
extern NspTypeGIRepository *nsp_type_girepository;

/* type instances for gobject */

NspTypeGIRepository *new_type_girepository(type_mode mode);

/* instance for NspGIRepository */

NspGIRepository *new_girepository();

/*
 * Object methods redefined for girepository 
 */

#define NULLGIREPOSITORY (NspGIRepository*) 0


/* from NspGIRepositoryObj.c */

extern NspGIRepository *nsp_girepository_object (NspObject *O);
extern int IsGIRepositoryObj (Stack stack, int i);
extern int IsGIRepository(NspObject *O);
extern NspGIRepository *GetGIRepositoryCopy (Stack stack, int i);
extern NspGIRepository *GetGIRepository (Stack stack, int i);

#endif /* NSP_INC_NspGIRepository */ 

#ifdef NspGIRepository_Private 
static int init_girepository(NspGIRepository *o,NspTypeGIRepository *type);
static char *nsp_girepository_type_as_string(void);
static char *nsp_girepository_type_short_string(NspObject *v);
static AttrTab girepository_attrs[];
static NspMethods *girepository_get_methods(void);
/* static int int_girepository_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGIRepository_Private */
