/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGMountOperation
#define NSP_INC_NspGMountOperation

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

/* NspGMountOperation */

#include <nsp/gtk/gobject.h>

/*
 * NspGMountOperation inherits from GObject
 * just change some type attributes 
 */

typedef NspGObject NspGMountOperation ;
typedef NspTypeGObject NspTypeGMountOperation ;

extern int nsp_type_gmountoperation_id;
extern NspTypeGMountOperation *nsp_type_gmountoperation;

/* type instances for gobject */

NspTypeGMountOperation *new_type_gmountoperation(type_mode mode);

/* instance for NspGMountOperation */

NspGMountOperation *new_gmountoperation();

/*
 * Object methods redefined for gmountoperation 
 */

#define NULLGMOUNTOPERATION (NspGMountOperation*) 0


/* from NspGMountOperationObj.c */

extern NspGMountOperation *nsp_gmountoperation_object (NspObject *O);
extern int IsGMountOperationObj (Stack stack, int i);
extern int IsGMountOperation(NspObject *O);
extern NspGMountOperation *GetGMountOperationCopy (Stack stack, int i);
extern NspGMountOperation *GetGMountOperation (Stack stack, int i);

#endif /* NSP_INC_NspGMountOperation */ 

#ifdef NspGMountOperation_Private 
static int init_gmountoperation(NspGMountOperation *o,NspTypeGMountOperation *type);
static char *nsp_gmountoperation_type_as_string(void);
static char *nsp_gmountoperation_type_short_string(NspObject *v);
static AttrTab gmountoperation_attrs[];
static NspMethods *gmountoperation_get_methods(void);
/* static int int_gmountoperation_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGMountOperation_Private */
