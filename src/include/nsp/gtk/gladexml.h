/* -*- Mode: C -*- */
#ifndef INC_NSP_GladeXML
#define INC_NSP_GladeXML

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2003 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/gobject.h"

/*
* NspGladeXML inherits from NspGObject
* just change some type attributes 
*/

typedef NspGObject NspGladeXML ;
typedef NspTypeGObject NspTypeGladeXML ;

extern int nsp_type_gladexml_id;
extern NspTypeGladeXML *nsp_type_gladexml;

/* type instances for gobject */

NspTypeGladeXML *new_type_gladexml(type_mode mode);

/* instance for GladeXML */

NspGladeXML *new_gladexml();

/*
* Object methods redefined for gladexml 
*/

#define NULLGLADEXML (NspGladeXML*) 0

NspGladeXML *gladexml_create(char *name,NspTypeBase *type);

/* from GladeXMLObj.c */

extern NspGladeXML *gladexml_object (NspObject *O); 
extern int IsGladeXMLObj (Stack stack, int i); 
extern int IsGladeXML(NspObject *O);
extern NspGladeXML *GetGladeXMLCopy (Stack stack, int i); 
extern NspGladeXML *GetGladeXML (Stack stack, int i); 

#endif 

#ifdef GladeXML_Private 
static int init_gladexml(NspGladeXML *o,NspTypeGladeXML *type);
static char *gladexml_type_as_string(void);
static char *gladexml_type_short_string(void);
static AttrTab gladexml_attrs[];
/* static int int_gladexml_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *gladexml_get_methods(void); 
#endif /* GladeXML_Private */
