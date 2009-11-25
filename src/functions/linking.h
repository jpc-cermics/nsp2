#ifndef NSP_LINKING
#define NSP_LINKING

#include <nsp/string.h>
#include <nsp/object.h> /* for Hash */
/* Nsp
 * Copyright (C) 1998-2009 Jean-Philippe Chancelier Enpc/Cermics
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

extern int SearchInDynLinks(nsp_const_string op, int (**realop) ());
extern void  ShowDynLinks (void);
extern int nsp_is_linked(nsp_const_string name,int *ilib);
extern void nsp_link_library(int iflag, int *rhs,int *ilib,nsp_const_string shared_path, char **en_names, char strf);
extern void nsp_dynamic_load(nsp_const_string shared_path,char **en_names,char strf, int *ilib, int iflag, int *rhs);
extern NspHash *nsp_get_dlsymbols();
extern void nsp_unlink_shared(int ilib);
extern void nsp_remove_interface(int Nshared);
extern void nsp_link_initialize(void);
extern void nsp_delete_interface_functions(int Int);

#endif 
