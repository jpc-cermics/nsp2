/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkSelectionData
#define NSP_INC_NspGtkSelectionData

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

/* NspGtkSelectionData */

#include <nsp/gtk/gboxed.h>

/*
 * NspGtkSelectionData inherits from GBoxed
 * just change some type attributes 
 */

typedef NspGBoxed NspGtkSelectionData ;
typedef NspTypeGBoxed NspTypeGtkSelectionData ;

extern int nsp_type_gtkselectiondata_id;
extern NspTypeGtkSelectionData *nsp_type_gtkselectiondata;

/* type instances for gboxed */

NspTypeGtkSelectionData *new_type_gtkselectiondata(type_mode mode);

/* instance for NspGtkSelectionData */

NspGtkSelectionData *new_gtkselectiondata();

/*
 * Object methods redefined for gtkselectiondata 
 */

#define NULLGTKSELECTIONDATA (NspGtkSelectionData*) 0


/* from NspGtkSelectionDataObj.c */

extern NspGtkSelectionData *nsp_gtkselectiondata_object (NspObject *O);
extern int IsGtkSelectionDataObj (Stack stack, int i);
extern int IsGtkSelectionData(NspObject *O);
extern NspGtkSelectionData *GetGtkSelectionDataCopy (Stack stack, int i);
extern NspGtkSelectionData *GetGtkSelectionData (Stack stack, int i);

#endif /* NSP_INC_NspGtkSelectionData */ 

#ifdef NspGtkSelectionData_Private 
static int init_gtkselectiondata(NspGtkSelectionData *o,NspTypeGtkSelectionData *type);
static char *nsp_gtkselectiondata_type_as_string(void);
static char *nsp_gtkselectiondata_type_short_string(NspObject *v);
static AttrTab gtkselectiondata_attrs[];
static NspMethods *gtkselectiondata_get_methods(void);
/* static int int_gtkselectiondata_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkSelectionData_Private */
