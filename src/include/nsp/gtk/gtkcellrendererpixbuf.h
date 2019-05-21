/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkCellRendererPixbuf
#define NSP_INC_NspGtkCellRendererPixbuf

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

/* NspGtkCellRendererPixbuf */

#include <nsp/gtk/gtkcellrenderer.h>

/*
 * NspGtkCellRendererPixbuf inherits from GtkCellRenderer
 * just change some type attributes 
 */

typedef NspGtkCellRenderer NspGtkCellRendererPixbuf ;
typedef NspTypeGtkCellRenderer NspTypeGtkCellRendererPixbuf ;

extern int nsp_type_gtkcellrendererpixbuf_id;
extern NspTypeGtkCellRendererPixbuf *nsp_type_gtkcellrendererpixbuf;

/* type instances for gtkcellrenderer */

NspTypeGtkCellRendererPixbuf *new_type_gtkcellrendererpixbuf(type_mode mode);

/* instance for NspGtkCellRendererPixbuf */

NspGtkCellRendererPixbuf *new_gtkcellrendererpixbuf();

/*
 * Object methods redefined for gtkcellrendererpixbuf 
 */

#define NULLGTKCELLRENDERERPIXBUF (NspGtkCellRendererPixbuf*) 0


/* from NspGtkCellRendererPixbufObj.c */

extern NspGtkCellRendererPixbuf *nsp_gtkcellrendererpixbuf_object (NspObject *O);
extern int IsGtkCellRendererPixbufObj (Stack stack, int i);
extern int IsGtkCellRendererPixbuf(NspObject *O);
extern NspGtkCellRendererPixbuf *GetGtkCellRendererPixbufCopy (Stack stack, int i);
extern NspGtkCellRendererPixbuf *GetGtkCellRendererPixbuf (Stack stack, int i);

#endif /* NSP_INC_NspGtkCellRendererPixbuf */ 

#ifdef NspGtkCellRendererPixbuf_Private 
static int init_gtkcellrendererpixbuf(NspGtkCellRendererPixbuf *o,NspTypeGtkCellRendererPixbuf *type);
static char *nsp_gtkcellrendererpixbuf_type_as_string(void);
static char *nsp_gtkcellrendererpixbuf_type_short_string(NspObject *v);
static AttrTab gtkcellrendererpixbuf_attrs[];
static NspMethods *gtkcellrendererpixbuf_get_methods(void);
/* static int int_gtkcellrendererpixbuf_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkCellRendererPixbuf_Private */
