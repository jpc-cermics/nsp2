/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGSocketService
#define NSP_INC_NspGSocketService

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

/* NspGSocketService */

#include <nsp/gtk/gsocketlistener.h>

/*
 * NspGSocketService inherits from GSocketListener
 * just change some type attributes 
 */

typedef NspGSocketListener NspGSocketService ;
typedef NspTypeGSocketListener NspTypeGSocketService ;

extern int nsp_type_gsocketservice_id;
extern NspTypeGSocketService *nsp_type_gsocketservice;

/* type instances for gsocketlistener */

NspTypeGSocketService *new_type_gsocketservice(type_mode mode);

/* instance for NspGSocketService */

NspGSocketService *new_gsocketservice();

/*
 * Object methods redefined for gsocketservice 
 */

#define NULLGSOCKETSERVICE (NspGSocketService*) 0


/* from NspGSocketServiceObj.c */

extern NspGSocketService *nsp_gsocketservice_object (NspObject *O);
extern int IsGSocketServiceObj (Stack stack, int i);
extern int IsGSocketService(NspObject *O);
extern NspGSocketService *GetGSocketServiceCopy (Stack stack, int i);
extern NspGSocketService *GetGSocketService (Stack stack, int i);

#endif /* NSP_INC_NspGSocketService */ 

#ifdef NspGSocketService_Private 
static int init_gsocketservice(NspGSocketService *o,NspTypeGSocketService *type);
static char *nsp_gsocketservice_type_as_string(void);
static char *nsp_gsocketservice_type_short_string(NspObject *v);
static AttrTab gsocketservice_attrs[];
static NspMethods *gsocketservice_get_methods(void);
/* static int int_gsocketservice_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGSocketService_Private */
