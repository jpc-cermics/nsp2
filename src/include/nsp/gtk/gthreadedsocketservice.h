/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGThreadedSocketService
#define NSP_INC_NspGThreadedSocketService

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

/* NspGThreadedSocketService */

#include <nsp/gtk/gsocketservice.h>

/*
 * NspGThreadedSocketService inherits from GSocketService
 * just change some type attributes 
 */

typedef NspGSocketService NspGThreadedSocketService ;
typedef NspTypeGSocketService NspTypeGThreadedSocketService ;

extern int nsp_type_gthreadedsocketservice_id;
extern NspTypeGThreadedSocketService *nsp_type_gthreadedsocketservice;

/* type instances for gsocketservice */

NspTypeGThreadedSocketService *new_type_gthreadedsocketservice(type_mode mode);

/* instance for NspGThreadedSocketService */

NspGThreadedSocketService *new_gthreadedsocketservice();

/*
 * Object methods redefined for gthreadedsocketservice 
 */

#define NULLGTHREADEDSOCKETSERVICE (NspGThreadedSocketService*) 0


/* from NspGThreadedSocketServiceObj.c */

extern NspGThreadedSocketService *nsp_gthreadedsocketservice_object (NspObject *O);
extern int IsGThreadedSocketServiceObj (Stack stack, int i);
extern int IsGThreadedSocketService(NspObject *O);
extern NspGThreadedSocketService *GetGThreadedSocketServiceCopy (Stack stack, int i);
extern NspGThreadedSocketService *GetGThreadedSocketService (Stack stack, int i);

#endif /* NSP_INC_NspGThreadedSocketService */ 

#ifdef NspGThreadedSocketService_Private 
static int init_gthreadedsocketservice(NspGThreadedSocketService *o,NspTypeGThreadedSocketService *type);
static char *nsp_gthreadedsocketservice_type_as_string(void);
static char *nsp_gthreadedsocketservice_type_short_string(NspObject *v);
static AttrTab gthreadedsocketservice_attrs[];
static NspMethods *gthreadedsocketservice_get_methods(void);
/* static int int_gthreadedsocketservice_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGThreadedSocketService_Private */
