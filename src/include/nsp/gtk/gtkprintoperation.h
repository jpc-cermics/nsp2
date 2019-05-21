/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkPrintOperation
#define NSP_INC_NspGtkPrintOperation

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

/* NspGtkPrintOperation */

#include <nsp/gtk/gobject.h>

/*
 * NspGtkPrintOperation inherits from GObject
 * just change some type attributes 
 */

typedef NspGObject NspGtkPrintOperation ;
typedef NspTypeGObject NspTypeGtkPrintOperation ;

extern int nsp_type_gtkprintoperation_id;
extern NspTypeGtkPrintOperation *nsp_type_gtkprintoperation;

/* type instances for gobject */

NspTypeGtkPrintOperation *new_type_gtkprintoperation(type_mode mode);

/* instance for NspGtkPrintOperation */

NspGtkPrintOperation *new_gtkprintoperation();

/*
 * Object methods redefined for gtkprintoperation 
 */

#define NULLGTKPRINTOPERATION (NspGtkPrintOperation*) 0


/* from NspGtkPrintOperationObj.c */

extern NspGtkPrintOperation *nsp_gtkprintoperation_object (NspObject *O);
extern int IsGtkPrintOperationObj (Stack stack, int i);
extern int IsGtkPrintOperation(NspObject *O);
extern NspGtkPrintOperation *GetGtkPrintOperationCopy (Stack stack, int i);
extern NspGtkPrintOperation *GetGtkPrintOperation (Stack stack, int i);

#endif /* NSP_INC_NspGtkPrintOperation */ 

#ifdef NspGtkPrintOperation_Private 
static int init_gtkprintoperation(NspGtkPrintOperation *o,NspTypeGtkPrintOperation *type);
static char *nsp_gtkprintoperation_type_as_string(void);
static char *nsp_gtkprintoperation_type_short_string(NspObject *v);
static AttrTab gtkprintoperation_attrs[];
static NspMethods *gtkprintoperation_get_methods(void);
/* static int int_gtkprintoperation_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkPrintOperation_Private */
