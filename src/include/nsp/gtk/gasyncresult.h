/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGAsyncResult
#define NSP_INC_NspGAsyncResult

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

/* NspGAsyncResult */

#include <nsp/gtk/gobject.h>

/*
 * NspGAsyncResult inherits from GObject
 * just change some type attributes 
 */

typedef NspGObject NspGAsyncResult ;
typedef NspTypeGObject NspTypeGAsyncResult ;

extern int nsp_type_gasyncresult_id;
extern NspTypeGAsyncResult *nsp_type_gasyncresult;

/* type instances for gobject */

NspTypeGAsyncResult *new_type_gasyncresult(type_mode mode);

/* instance for NspGAsyncResult */

NspGAsyncResult *new_gasyncresult();

/*
 * Object methods redefined for gasyncresult 
 */

#define NULLGASYNCRESULT (NspGAsyncResult*) 0


/* from NspGAsyncResultObj.c */

extern NspGAsyncResult *nsp_gasyncresult_object (NspObject *O);
extern int IsGAsyncResultObj (Stack stack, int i);
extern int IsGAsyncResult(NspObject *O);
extern NspGAsyncResult *GetGAsyncResultCopy (Stack stack, int i);
extern NspGAsyncResult *GetGAsyncResult (Stack stack, int i);

#endif /* NSP_INC_NspGAsyncResult */ 

#ifdef NspGAsyncResult_Private 
static int init_gasyncresult(NspGAsyncResult *o,NspTypeGAsyncResult *type);
static char *nsp_gasyncresult_type_as_string(void);
static char *nsp_gasyncresult_type_short_string(NspObject *v);
static AttrTab gasyncresult_attrs[];
static NspMethods *gasyncresult_get_methods(void);
/* static int int_gasyncresult_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGAsyncResult_Private */
