/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGMenuModel
#define NSP_INC_NspGMenuModel

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

/* NspGMenuModel */

#include <nsp/gtk/gobject.h>

/*
 * NspGMenuModel inherits from GObject
 * just change some type attributes 
 */

typedef NspGObject NspGMenuModel ;
typedef NspTypeGObject NspTypeGMenuModel ;

extern int nsp_type_gmenumodel_id;
extern NspTypeGMenuModel *nsp_type_gmenumodel;

/* type instances for gobject */

NspTypeGMenuModel *new_type_gmenumodel(type_mode mode);

/* instance for NspGMenuModel */

NspGMenuModel *new_gmenumodel();

/*
 * Object methods redefined for gmenumodel 
 */

#define NULLGMENUMODEL (NspGMenuModel*) 0


/* from NspGMenuModelObj.c */

extern NspGMenuModel *nsp_gmenumodel_object (NspObject *O);
extern int IsGMenuModelObj (Stack stack, int i);
extern int IsGMenuModel(NspObject *O);
extern NspGMenuModel *GetGMenuModelCopy (Stack stack, int i);
extern NspGMenuModel *GetGMenuModel (Stack stack, int i);

#endif /* NSP_INC_NspGMenuModel */ 

#ifdef NspGMenuModel_Private 
static int init_gmenumodel(NspGMenuModel *o,NspTypeGMenuModel *type);
static char *nsp_gmenumodel_type_as_string(void);
static char *nsp_gmenumodel_type_short_string(NspObject *v);
static AttrTab gmenumodel_attrs[];
static NspMethods *gmenumodel_get_methods(void);
/* static int int_gmenumodel_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGMenuModel_Private */
