/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkSourceMarksSequence
#define NSP_INC_NspGtkSourceMarksSequence

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

/* NspGtkSourceMarksSequence */

#include <nsp/gtk/gobject.h>

/*
 * NspGtkSourceMarksSequence inherits from GObject
 * just change some type attributes 
 */

typedef NspGObject NspGtkSourceMarksSequence ;
typedef NspTypeGObject NspTypeGtkSourceMarksSequence ;

extern int nsp_type_gtksourcemarkssequence_id;
extern NspTypeGtkSourceMarksSequence *nsp_type_gtksourcemarkssequence;

/* type instances for gobject */

NspTypeGtkSourceMarksSequence *new_type_gtksourcemarkssequence(type_mode mode);

/* instance for NspGtkSourceMarksSequence */

NspGtkSourceMarksSequence *new_gtksourcemarkssequence();

/*
 * Object methods redefined for gtksourcemarkssequence 
 */

#define NULLGTKSOURCEMARKSSEQUENCE (NspGtkSourceMarksSequence*) 0


/* from NspGtkSourceMarksSequenceObj.c */

extern NspGtkSourceMarksSequence *nsp_gtksourcemarkssequence_object (NspObject *O);
extern int IsGtkSourceMarksSequenceObj (Stack stack, int i);
extern int IsGtkSourceMarksSequence(NspObject *O);
extern NspGtkSourceMarksSequence *GetGtkSourceMarksSequenceCopy (Stack stack, int i);
extern NspGtkSourceMarksSequence *GetGtkSourceMarksSequence (Stack stack, int i);

#endif /* NSP_INC_NspGtkSourceMarksSequence */ 

#ifdef NspGtkSourceMarksSequence_Private 
static int init_gtksourcemarkssequence(NspGtkSourceMarksSequence *o,NspTypeGtkSourceMarksSequence *type);
static char *nsp_gtksourcemarkssequence_type_as_string(void);
static char *nsp_gtksourcemarkssequence_type_short_string(NspObject *v);
static AttrTab gtksourcemarkssequence_attrs[];
static NspMethods *gtksourcemarkssequence_get_methods(void);
/* static int int_gtksourcemarkssequence_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkSourceMarksSequence_Private */
