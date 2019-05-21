/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGSocketConnection
#define NSP_INC_NspGSocketConnection

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

/* NspGSocketConnection */

#include <nsp/gtk/giostream.h>

/*
 * NspGSocketConnection inherits from GIOStream
 * just change some type attributes 
 */

typedef NspGIOStream NspGSocketConnection ;
typedef NspTypeGIOStream NspTypeGSocketConnection ;

extern int nsp_type_gsocketconnection_id;
extern NspTypeGSocketConnection *nsp_type_gsocketconnection;

/* type instances for giostream */

NspTypeGSocketConnection *new_type_gsocketconnection(type_mode mode);

/* instance for NspGSocketConnection */

NspGSocketConnection *new_gsocketconnection();

/*
 * Object methods redefined for gsocketconnection 
 */

#define NULLGSOCKETCONNECTION (NspGSocketConnection*) 0


/* from NspGSocketConnectionObj.c */

extern NspGSocketConnection *nsp_gsocketconnection_object (NspObject *O);
extern int IsGSocketConnectionObj (Stack stack, int i);
extern int IsGSocketConnection(NspObject *O);
extern NspGSocketConnection *GetGSocketConnectionCopy (Stack stack, int i);
extern NspGSocketConnection *GetGSocketConnection (Stack stack, int i);

#endif /* NSP_INC_NspGSocketConnection */ 

#ifdef NspGSocketConnection_Private 
static int init_gsocketconnection(NspGSocketConnection *o,NspTypeGSocketConnection *type);
static char *nsp_gsocketconnection_type_as_string(void);
static char *nsp_gsocketconnection_type_short_string(NspObject *v);
static AttrTab gsocketconnection_attrs[];
static NspMethods *gsocketconnection_get_methods(void);
/* static int int_gsocketconnection_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGSocketConnection_Private */
