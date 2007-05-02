/* -*- Mode: C -*- */
#ifndef INC_NSP_PangoFontMetrics
#define INC_NSP_PangoFontMetrics

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2007 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/gboxed.h"

/*
* NspPangoFontMetrics inherits from NspGBoxed
* just change some type attributes 
*/

typedef NspGBoxed NspPangoFontMetrics ;
typedef NspTypeGBoxed NspTypePangoFontMetrics ;

extern int nsp_type_pangofontmetrics_id;
extern NspTypePangoFontMetrics *nsp_type_pangofontmetrics;

/* type instances for gboxed */

NspTypePangoFontMetrics *new_type_pangofontmetrics(type_mode mode);

/* instance for PangoFontMetrics */

NspPangoFontMetrics *new_pangofontmetrics();

/*
* Object methods redefined for pangofontmetrics 
*/

#define NULLPANGOFONTMETRICS (NspPangoFontMetrics*) 0

NspPangoFontMetrics *pangofontmetrics_create(char *name,NspTypeBase *type);

/* from PangoFontMetricsObj.c */

extern NspPangoFontMetrics *pangofontmetrics_object (NspObject *O); 
extern int IsPangoFontMetricsObj (Stack stack, int i); 
extern int IsPangoFontMetrics(NspObject *O);
extern NspPangoFontMetrics *GetPangoFontMetricsCopy (Stack stack, int i); 
extern NspPangoFontMetrics *GetPangoFontMetrics (Stack stack, int i); 

#endif 

#ifdef PangoFontMetrics_Private 
static int init_pangofontmetrics(NspPangoFontMetrics *o,NspTypePangoFontMetrics *type);
static char *pangofontmetrics_type_as_string(void);
static char *pangofontmetrics_type_short_string(NspObject *v);
static AttrTab pangofontmetrics_attrs[];
/* static int int_pangofontmetrics_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *pangofontmetrics_get_methods(void); 
#endif /* PangoFontMetrics_Private */
