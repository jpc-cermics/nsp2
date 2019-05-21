/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGTlsConnection
#define NSP_INC_NspGTlsConnection

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

/* NspGTlsConnection */

#include <nsp/gtk/giostream.h>

/*
 * NspGTlsConnection inherits from GIOStream
 * just change some type attributes 
 */

typedef NspGIOStream NspGTlsConnection ;
typedef NspTypeGIOStream NspTypeGTlsConnection ;

extern int nsp_type_gtlsconnection_id;
extern NspTypeGTlsConnection *nsp_type_gtlsconnection;

/* type instances for giostream */

NspTypeGTlsConnection *new_type_gtlsconnection(type_mode mode);

/* instance for NspGTlsConnection */

NspGTlsConnection *new_gtlsconnection();

/*
 * Object methods redefined for gtlsconnection 
 */

#define NULLGTLSCONNECTION (NspGTlsConnection*) 0


/* from NspGTlsConnectionObj.c */

extern NspGTlsConnection *nsp_gtlsconnection_object (NspObject *O);
extern int IsGTlsConnectionObj (Stack stack, int i);
extern int IsGTlsConnection(NspObject *O);
extern NspGTlsConnection *GetGTlsConnectionCopy (Stack stack, int i);
extern NspGTlsConnection *GetGTlsConnection (Stack stack, int i);

#endif /* NSP_INC_NspGTlsConnection */ 

#ifdef NspGTlsConnection_Private 
static int init_gtlsconnection(NspGTlsConnection *o,NspTypeGTlsConnection *type);
static char *nsp_gtlsconnection_type_as_string(void);
static char *nsp_gtlsconnection_type_short_string(NspObject *v);
static AttrTab gtlsconnection_attrs[];
static NspMethods *gtlsconnection_get_methods(void);
/* static int int_gtlsconnection_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGTlsConnection_Private */
