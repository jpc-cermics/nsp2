/* -*- Mode: C -*- */
#ifndef INC_NSP_PangoLanguage
#define INC_NSP_PangoLanguage

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2003 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/gboxed.h"

/*
* NspPangoLanguage inherits from NspGBoxed
* just change some type attributes 
*/

typedef NspGBoxed NspPangoLanguage ;
typedef NspTypeGBoxed NspTypePangoLanguage ;

extern int nsp_type_pangolanguage_id;
extern NspTypePangoLanguage *nsp_type_pangolanguage;

/* type instances for gboxed */

NspTypePangoLanguage *new_type_pangolanguage(type_mode mode);

/* instance for PangoLanguage */

NspPangoLanguage *new_pangolanguage();

/*
* Object methods redefined for pangolanguage 
*/

#define NULLPANGOLANGUAGE (NspPangoLanguage*) 0

NspPangoLanguage *pangolanguage_create(char *name,NspTypeBase *type);

/* from PangoLanguageObj.c */

extern NspPangoLanguage *pangolanguage_object (NspObject *O); 
extern int IsPangoLanguageObj (Stack stack, int i); 
extern int IsPangoLanguage(NspObject *O);
extern NspPangoLanguage *GetPangoLanguageCopy (Stack stack, int i); 
extern NspPangoLanguage *GetPangoLanguage (Stack stack, int i); 

#endif 

#ifdef PangoLanguage_Private 
static int init_pangolanguage(NspPangoLanguage *o,NspTypePangoLanguage *type);
static char *pangolanguage_type_as_string(void);
static char *pangolanguage_type_short_string(NspObject *v);
static AttrTab pangolanguage_attrs[];
/* static int int_pangolanguage_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *pangolanguage_get_methods(void); 
#endif /* PangoLanguage_Private */
