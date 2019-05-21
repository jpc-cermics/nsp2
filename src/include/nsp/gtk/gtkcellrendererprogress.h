/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkCellRendererProgress
#define NSP_INC_NspGtkCellRendererProgress

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

/* NspGtkCellRendererProgress */

#include <nsp/gtk/gtkcellrenderer.h>

/*
 * NspGtkCellRendererProgress inherits from GtkCellRenderer
 * just change some type attributes 
 */

typedef NspGtkCellRenderer NspGtkCellRendererProgress ;
typedef NspTypeGtkCellRenderer NspTypeGtkCellRendererProgress ;

extern int nsp_type_gtkcellrendererprogress_id;
extern NspTypeGtkCellRendererProgress *nsp_type_gtkcellrendererprogress;

/* type instances for gtkcellrenderer */

NspTypeGtkCellRendererProgress *new_type_gtkcellrendererprogress(type_mode mode);

/* instance for NspGtkCellRendererProgress */

NspGtkCellRendererProgress *new_gtkcellrendererprogress();

/*
 * Object methods redefined for gtkcellrendererprogress 
 */

#define NULLGTKCELLRENDERERPROGRESS (NspGtkCellRendererProgress*) 0


/* from NspGtkCellRendererProgressObj.c */

extern NspGtkCellRendererProgress *nsp_gtkcellrendererprogress_object (NspObject *O);
extern int IsGtkCellRendererProgressObj (Stack stack, int i);
extern int IsGtkCellRendererProgress(NspObject *O);
extern NspGtkCellRendererProgress *GetGtkCellRendererProgressCopy (Stack stack, int i);
extern NspGtkCellRendererProgress *GetGtkCellRendererProgress (Stack stack, int i);

#endif /* NSP_INC_NspGtkCellRendererProgress */ 

#ifdef NspGtkCellRendererProgress_Private 
static int init_gtkcellrendererprogress(NspGtkCellRendererProgress *o,NspTypeGtkCellRendererProgress *type);
static char *nsp_gtkcellrendererprogress_type_as_string(void);
static char *nsp_gtkcellrendererprogress_type_short_string(NspObject *v);
static AttrTab gtkcellrendererprogress_attrs[];
static NspMethods *gtkcellrendererprogress_get_methods(void);
/* static int int_gtkcellrendererprogress_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkCellRendererProgress_Private */
