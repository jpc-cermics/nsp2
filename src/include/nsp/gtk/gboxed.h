/* -*- Mode: C -*- */
#ifndef INC_NSP_GBoxed
#define INC_NSP_GBoxed

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

/* GBoxed */

#include <nsp/object.h>
#include <nsp/gtk/gobject.h>
#include <glib.h>
#include <glib-object.h>

/*
 * NspGBoxed inherits from NspObject
 */

typedef struct _nsp_gboxed NspGBoxed;

typedef int (*gboxed_save) (NspFile  *F, NspGBoxed *M);

typedef struct _nsp_type_GBoxed {
  NSP_TYPE_OBJECT__
  /* rajouts */
} NspTypeGBoxed;

struct _nsp_gboxed {
  NspObject father;
  NspTypeGBoxed *type;
  gpointer boxed;
  GType gtype;
  gboolean free_on_dealloc;
};

extern int nsp_type_gboxed_id;
extern NspTypeGBoxed *nsp_type_gboxed;

/* type instances for classa */

NspTypeGBoxed *new_type_gboxed(type_mode mode);

/* instance for GBoxed */

NspGBoxed *new_gboxed();

/*
 * Object methods redefined for gboxed
 */

#define NULLGBOXED (NspGBoxed*) 0
extern NspGBoxed *gboxed_create(char *name,GType boxed_type, gpointer boxed, gboolean copy_boxed,
				gboolean own_ref, /* (NspTypeBase *) */ void *  type );
extern NspGBoxed *gboxed_copy(NspGBoxed *H);
extern void gboxed_destroy(NspGBoxed *H);
extern int gboxed_info(NspGBoxed *H, int indent,char *name, int rec_level);
extern int gboxed_print(NspGBoxed *H, int indent,char *name, int rec_level);
extern NspGBoxed *gboxed_object (NspObject *O);
extern int IsGBoxedObj (Stack stack, int i);
extern int IsGBoxed(NspObject *O);
extern NspGBoxed *GetGBoxedCopy (Stack stack, int i);
extern NspGBoxed *GetGBoxed (Stack stack, int i);
#define nspg_boxed_get(x, type) ((type *) ((NspGBoxed *) x)->boxed)
#define NSP_GBOXED_GET(x, type) ((type *) ((NspGBoxed *) x)->boxed)
extern int nspg_boxed_check(NspObject *self,GType boxed_type) ;

#endif /* INC_NSP_GBoxed */

#ifdef GBoxed_Private
static int init_gboxed(NspGBoxed *o,NspTypeGBoxed *type);
static char *gboxed_type_as_string(void);
static char *gboxed_type_short_string(NspObject *v);
static int gboxed_eq(NspGBoxed *A, NspObject *B);
static int gboxed_neq(NspGBoxed *A, NspObject *B);
static NspMethods *gboxed_get_methods(NspObject *Obj);
#endif /* GBoxed_Private */

#ifndef INC_NSP_GBoxedNsp
#define INC_NSP_GBoxedNsp

/*
 * NspBoxedNsp inherits from GBoxed
 *
 */

typedef NspGBoxed NspBoxedNsp ;
typedef NspTypeGBoxed NspTypeBoxedNsp ;

extern int nsp_type_gboxednsp_id;
extern NspTypeBoxedNsp *nsp_type_gboxednsp;

/* type instances for gboxed */

NspTypeBoxedNsp *new_type_gboxednsp(type_mode mode);

/* instance for NspBoxedNsp */

NspBoxedNsp *new_gboxednsp();

/*
 * Object methods redefined for gboxednsp
 */

#define NULLBOXEDNSP (NspBoxedNsp*) 0


/* from NspBoxedNspObj.c */

extern NspBoxedNsp *nsp_gboxednsp_object (NspObject *O);
extern int IsBoxedNspObj (Stack stack, int i);
extern int IsBoxedNsp(NspObject *O);
extern NspBoxedNsp *GetBoxedNspCopy (Stack stack, int i);
extern NspBoxedNsp *GetBoxedNsp (Stack stack, int i);

#endif /* INC_NSP_GBoxedNsp */

#ifdef NspBoxedNsp_Private
static int init_gboxednsp(NspBoxedNsp *o,NspTypeBoxedNsp *type);
static char *nsp_gboxednsp_type_as_string(void);
static char *nsp_gboxednsp_type_short_string(NspObject *v);
static AttrTab gboxednsp_attrs[];
static NspMethods *gboxednsp_get_methods(void);
#endif /* NspBoxedNsp_Private */
