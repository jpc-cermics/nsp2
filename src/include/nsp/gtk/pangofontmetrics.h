/* -*- Mode: C -*- */
#ifndef NSP_INC_NspPangoFontMetrics
#define NSP_INC_NspPangoFontMetrics

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

/* NspPangoFontMetrics */

#include <nsp/gtk/gboxed.h>

/*
 * NspPangoFontMetrics inherits from GBoxed
 * just change some type attributes 
 */

typedef NspGBoxed NspPangoFontMetrics ;
typedef NspTypeGBoxed NspTypePangoFontMetrics ;

extern int nsp_type_pangofontmetrics_id;
extern NspTypePangoFontMetrics *nsp_type_pangofontmetrics;

/* type instances for gboxed */

NspTypePangoFontMetrics *new_type_pangofontmetrics(type_mode mode);

/* instance for NspPangoFontMetrics */

NspPangoFontMetrics *new_pangofontmetrics();

/*
 * Object methods redefined for pangofontmetrics 
 */

#define NULLPANGOFONTMETRICS (NspPangoFontMetrics*) 0


/* from NspPangoFontMetricsObj.c */

extern NspPangoFontMetrics *nsp_pangofontmetrics_object (NspObject *O);
extern int IsPangoFontMetricsObj (Stack stack, int i);
extern int IsPangoFontMetrics(NspObject *O);
extern NspPangoFontMetrics *GetPangoFontMetricsCopy (Stack stack, int i);
extern NspPangoFontMetrics *GetPangoFontMetrics (Stack stack, int i);

#endif /* NSP_INC_NspPangoFontMetrics */ 

#ifdef NspPangoFontMetrics_Private 
static int init_pangofontmetrics(NspPangoFontMetrics *o,NspTypePangoFontMetrics *type);
static char *nsp_pangofontmetrics_type_as_string(void);
static char *nsp_pangofontmetrics_type_short_string(NspObject *v);
static AttrTab pangofontmetrics_attrs[];
static NspMethods *pangofontmetrics_get_methods(void);
/* static int int_pangofontmetrics_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspPangoFontMetrics_Private */
