/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGNetworkService
#define NSP_INC_NspGNetworkService

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

/* NspGNetworkService */

#include <nsp/gtk/gobject.h>

/*
 * NspGNetworkService inherits from GObject
 * just change some type attributes 
 */

typedef NspGObject NspGNetworkService ;
typedef NspTypeGObject NspTypeGNetworkService ;

extern int nsp_type_gnetworkservice_id;
extern NspTypeGNetworkService *nsp_type_gnetworkservice;

/* type instances for gobject */

NspTypeGNetworkService *new_type_gnetworkservice(type_mode mode);

/* instance for NspGNetworkService */

NspGNetworkService *new_gnetworkservice();

/*
 * Object methods redefined for gnetworkservice 
 */

#define NULLGNETWORKSERVICE (NspGNetworkService*) 0


/* from NspGNetworkServiceObj.c */

extern NspGNetworkService *nsp_gnetworkservice_object (NspObject *O);
extern int IsGNetworkServiceObj (Stack stack, int i);
extern int IsGNetworkService(NspObject *O);
extern NspGNetworkService *GetGNetworkServiceCopy (Stack stack, int i);
extern NspGNetworkService *GetGNetworkService (Stack stack, int i);

#endif /* NSP_INC_NspGNetworkService */ 

#ifdef NspGNetworkService_Private 
static int init_gnetworkservice(NspGNetworkService *o,NspTypeGNetworkService *type);
static char *nsp_gnetworkservice_type_as_string(void);
static char *nsp_gnetworkservice_type_short_string(NspObject *v);
static AttrTab gnetworkservice_attrs[];
static NspMethods *gnetworkservice_get_methods(void);
/* static int int_gnetworkservice_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGNetworkService_Private */
