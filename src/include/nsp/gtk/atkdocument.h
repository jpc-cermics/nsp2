/* -*- Mode: C -*- */
#ifndef NSP_INC_NspAtkDocument
#define NSP_INC_NspAtkDocument

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

/* NspAtkDocument */

#include <nsp/gtk/gobject.h>

/*
 * NspAtkDocument inherits from GObject
 * just change some type attributes 
 */

typedef NspGObject NspAtkDocument ;
typedef NspTypeGObject NspTypeAtkDocument ;

extern int nsp_type_atkdocument_id;
extern NspTypeAtkDocument *nsp_type_atkdocument;

/* type instances for gobject */

NspTypeAtkDocument *new_type_atkdocument(type_mode mode);

/* instance for NspAtkDocument */

NspAtkDocument *new_atkdocument();

/*
 * Object methods redefined for atkdocument 
 */

#define NULLATKDOCUMENT (NspAtkDocument*) 0


/* from NspAtkDocumentObj.c */

extern NspAtkDocument *nsp_atkdocument_object (NspObject *O);
extern int IsAtkDocumentObj (Stack stack, int i);
extern int IsAtkDocument(NspObject *O);
extern NspAtkDocument *GetAtkDocumentCopy (Stack stack, int i);
extern NspAtkDocument *GetAtkDocument (Stack stack, int i);

#endif /* NSP_INC_NspAtkDocument */ 

#ifdef NspAtkDocument_Private 
static int init_atkdocument(NspAtkDocument *o,NspTypeAtkDocument *type);
static char *nsp_atkdocument_type_as_string(void);
static char *nsp_atkdocument_type_short_string(NspObject *v);
static AttrTab atkdocument_attrs[];
static NspMethods *atkdocument_get_methods(void);
/* static int int_atkdocument_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspAtkDocument_Private */
