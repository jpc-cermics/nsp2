/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGTlsCertificate
#define NSP_INC_NspGTlsCertificate

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

/* NspGTlsCertificate */

#include <nsp/gtk/gobject.h>

/*
 * NspGTlsCertificate inherits from GObject
 * just change some type attributes 
 */

typedef NspGObject NspGTlsCertificate ;
typedef NspTypeGObject NspTypeGTlsCertificate ;

extern int nsp_type_gtlscertificate_id;
extern NspTypeGTlsCertificate *nsp_type_gtlscertificate;

/* type instances for gobject */

NspTypeGTlsCertificate *new_type_gtlscertificate(type_mode mode);

/* instance for NspGTlsCertificate */

NspGTlsCertificate *new_gtlscertificate();

/*
 * Object methods redefined for gtlscertificate 
 */

#define NULLGTLSCERTIFICATE (NspGTlsCertificate*) 0


/* from NspGTlsCertificateObj.c */

extern NspGTlsCertificate *nsp_gtlscertificate_object (NspObject *O);
extern int IsGTlsCertificateObj (Stack stack, int i);
extern int IsGTlsCertificate(NspObject *O);
extern NspGTlsCertificate *GetGTlsCertificateCopy (Stack stack, int i);
extern NspGTlsCertificate *GetGTlsCertificate (Stack stack, int i);

#endif /* NSP_INC_NspGTlsCertificate */ 

#ifdef NspGTlsCertificate_Private 
static int init_gtlscertificate(NspGTlsCertificate *o,NspTypeGTlsCertificate *type);
static char *nsp_gtlscertificate_type_as_string(void);
static char *nsp_gtlscertificate_type_short_string(NspObject *v);
static AttrTab gtlscertificate_attrs[];
static NspMethods *gtlscertificate_get_methods(void);
/* static int int_gtlscertificate_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGTlsCertificate_Private */
