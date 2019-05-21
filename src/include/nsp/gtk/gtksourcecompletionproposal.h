/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkSourceCompletionProposal
#define NSP_INC_NspGtkSourceCompletionProposal

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

/* NspGtkSourceCompletionProposal */

#include <nsp/gtk/gobject.h>

/*
 * NspGtkSourceCompletionProposal inherits from GObject
 * just change some type attributes 
 */

typedef NspGObject NspGtkSourceCompletionProposal ;
typedef NspTypeGObject NspTypeGtkSourceCompletionProposal ;

extern int nsp_type_gtksourcecompletionproposal_id;
extern NspTypeGtkSourceCompletionProposal *nsp_type_gtksourcecompletionproposal;

/* type instances for gobject */

NspTypeGtkSourceCompletionProposal *new_type_gtksourcecompletionproposal(type_mode mode);

/* instance for NspGtkSourceCompletionProposal */

NspGtkSourceCompletionProposal *new_gtksourcecompletionproposal();

/*
 * Object methods redefined for gtksourcecompletionproposal 
 */

#define NULLGTKSOURCECOMPLETIONPROPOSAL (NspGtkSourceCompletionProposal*) 0


/* from NspGtkSourceCompletionProposalObj.c */

extern NspGtkSourceCompletionProposal *nsp_gtksourcecompletionproposal_object (NspObject *O);
extern int IsGtkSourceCompletionProposalObj (Stack stack, int i);
extern int IsGtkSourceCompletionProposal(NspObject *O);
extern NspGtkSourceCompletionProposal *GetGtkSourceCompletionProposalCopy (Stack stack, int i);
extern NspGtkSourceCompletionProposal *GetGtkSourceCompletionProposal (Stack stack, int i);

#endif /* NSP_INC_NspGtkSourceCompletionProposal */ 

#ifdef NspGtkSourceCompletionProposal_Private 
static int init_gtksourcecompletionproposal(NspGtkSourceCompletionProposal *o,NspTypeGtkSourceCompletionProposal *type);
static char *nsp_gtksourcecompletionproposal_type_as_string(void);
static char *nsp_gtksourcecompletionproposal_type_short_string(NspObject *v);
static AttrTab gtksourcecompletionproposal_attrs[];
static NspMethods *gtksourcecompletionproposal_get_methods(void);
/* static int int_gtksourcecompletionproposal_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkSourceCompletionProposal_Private */
