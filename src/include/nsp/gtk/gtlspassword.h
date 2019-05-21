/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGTlsPassword
#define NSP_INC_NspGTlsPassword

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

/* NspGTlsPassword */

#include <nsp/gtk/gobject.h>

/*
 * NspGTlsPassword inherits from GObject
 * just change some type attributes 
 */

typedef NspGObject NspGTlsPassword ;
typedef NspTypeGObject NspTypeGTlsPassword ;

extern int nsp_type_gtlspassword_id;
extern NspTypeGTlsPassword *nsp_type_gtlspassword;

/* type instances for gobject */

NspTypeGTlsPassword *new_type_gtlspassword(type_mode mode);

/* instance for NspGTlsPassword */

NspGTlsPassword *new_gtlspassword();

/*
 * Object methods redefined for gtlspassword 
 */

#define NULLGTLSPASSWORD (NspGTlsPassword*) 0


/* from NspGTlsPasswordObj.c */

extern NspGTlsPassword *nsp_gtlspassword_object (NspObject *O);
extern int IsGTlsPasswordObj (Stack stack, int i);
extern int IsGTlsPassword(NspObject *O);
extern NspGTlsPassword *GetGTlsPasswordCopy (Stack stack, int i);
extern NspGTlsPassword *GetGTlsPassword (Stack stack, int i);

#endif /* NSP_INC_NspGTlsPassword */ 

#ifdef NspGTlsPassword_Private 
static int init_gtlspassword(NspGTlsPassword *o,NspTypeGTlsPassword *type);
static char *nsp_gtlspassword_type_as_string(void);
static char *nsp_gtlspassword_type_short_string(NspObject *v);
static AttrTab gtlspassword_attrs[];
static NspMethods *gtlspassword_get_methods(void);
/* static int int_gtlspassword_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGTlsPassword_Private */
