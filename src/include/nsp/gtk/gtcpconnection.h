/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGTcpConnection
#define NSP_INC_NspGTcpConnection

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

/* NspGTcpConnection */

#include <nsp/gtk/gsocketconnection.h>

/*
 * NspGTcpConnection inherits from GSocketConnection
 * just change some type attributes 
 */

typedef NspGSocketConnection NspGTcpConnection ;
typedef NspTypeGSocketConnection NspTypeGTcpConnection ;

extern int nsp_type_gtcpconnection_id;
extern NspTypeGTcpConnection *nsp_type_gtcpconnection;

/* type instances for gsocketconnection */

NspTypeGTcpConnection *new_type_gtcpconnection(type_mode mode);

/* instance for NspGTcpConnection */

NspGTcpConnection *new_gtcpconnection();

/*
 * Object methods redefined for gtcpconnection 
 */

#define NULLGTCPCONNECTION (NspGTcpConnection*) 0


/* from NspGTcpConnectionObj.c */

extern NspGTcpConnection *nsp_gtcpconnection_object (NspObject *O);
extern int IsGTcpConnectionObj (Stack stack, int i);
extern int IsGTcpConnection(NspObject *O);
extern NspGTcpConnection *GetGTcpConnectionCopy (Stack stack, int i);
extern NspGTcpConnection *GetGTcpConnection (Stack stack, int i);

#endif /* NSP_INC_NspGTcpConnection */ 

#ifdef NspGTcpConnection_Private 
static int init_gtcpconnection(NspGTcpConnection *o,NspTypeGTcpConnection *type);
static char *nsp_gtcpconnection_type_as_string(void);
static char *nsp_gtcpconnection_type_short_string(NspObject *v);
static AttrTab gtcpconnection_attrs[];
static NspMethods *gtcpconnection_get_methods(void);
/* static int int_gtcpconnection_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGTcpConnection_Private */
