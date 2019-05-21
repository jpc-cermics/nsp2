/* -*- Mode: C -*- */
#ifndef NSP_INC_NspPangoLanguage
#define NSP_INC_NspPangoLanguage

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

/* NspPangoLanguage */

#include <nsp/gtk/gboxed.h>

/*
 * NspPangoLanguage inherits from GBoxed
 * just change some type attributes 
 */

typedef NspGBoxed NspPangoLanguage ;
typedef NspTypeGBoxed NspTypePangoLanguage ;

extern int nsp_type_pangolanguage_id;
extern NspTypePangoLanguage *nsp_type_pangolanguage;

/* type instances for gboxed */

NspTypePangoLanguage *new_type_pangolanguage(type_mode mode);

/* instance for NspPangoLanguage */

NspPangoLanguage *new_pangolanguage();

/*
 * Object methods redefined for pangolanguage 
 */

#define NULLPANGOLANGUAGE (NspPangoLanguage*) 0


/* from NspPangoLanguageObj.c */

extern NspPangoLanguage *nsp_pangolanguage_object (NspObject *O);
extern int IsPangoLanguageObj (Stack stack, int i);
extern int IsPangoLanguage(NspObject *O);
extern NspPangoLanguage *GetPangoLanguageCopy (Stack stack, int i);
extern NspPangoLanguage *GetPangoLanguage (Stack stack, int i);

#endif /* NSP_INC_NspPangoLanguage */ 

#ifdef NspPangoLanguage_Private 
static int init_pangolanguage(NspPangoLanguage *o,NspTypePangoLanguage *type);
static char *nsp_pangolanguage_type_as_string(void);
static char *nsp_pangolanguage_type_short_string(NspObject *v);
static AttrTab pangolanguage_attrs[];
static NspMethods *pangolanguage_get_methods(void);
/* static int int_pangolanguage_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspPangoLanguage_Private */
