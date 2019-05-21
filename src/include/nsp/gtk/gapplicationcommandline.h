/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGApplicationCommandLine
#define NSP_INC_NspGApplicationCommandLine

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

/* NspGApplicationCommandLine */

#include <nsp/gtk/gobject.h>

/*
 * NspGApplicationCommandLine inherits from GObject
 * just change some type attributes 
 */

typedef NspGObject NspGApplicationCommandLine ;
typedef NspTypeGObject NspTypeGApplicationCommandLine ;

extern int nsp_type_gapplicationcommandline_id;
extern NspTypeGApplicationCommandLine *nsp_type_gapplicationcommandline;

/* type instances for gobject */

NspTypeGApplicationCommandLine *new_type_gapplicationcommandline(type_mode mode);

/* instance for NspGApplicationCommandLine */

NspGApplicationCommandLine *new_gapplicationcommandline();

/*
 * Object methods redefined for gapplicationcommandline 
 */

#define NULLGAPPLICATIONCOMMANDLINE (NspGApplicationCommandLine*) 0


/* from NspGApplicationCommandLineObj.c */

extern NspGApplicationCommandLine *nsp_gapplicationcommandline_object (NspObject *O);
extern int IsGApplicationCommandLineObj (Stack stack, int i);
extern int IsGApplicationCommandLine(NspObject *O);
extern NspGApplicationCommandLine *GetGApplicationCommandLineCopy (Stack stack, int i);
extern NspGApplicationCommandLine *GetGApplicationCommandLine (Stack stack, int i);

#endif /* NSP_INC_NspGApplicationCommandLine */ 

#ifdef NspGApplicationCommandLine_Private 
static int init_gapplicationcommandline(NspGApplicationCommandLine *o,NspTypeGApplicationCommandLine *type);
static char *nsp_gapplicationcommandline_type_as_string(void);
static char *nsp_gapplicationcommandline_type_short_string(NspObject *v);
static AttrTab gapplicationcommandline_attrs[];
static NspMethods *gapplicationcommandline_get_methods(void);
/* static int int_gapplicationcommandline_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGApplicationCommandLine_Private */
