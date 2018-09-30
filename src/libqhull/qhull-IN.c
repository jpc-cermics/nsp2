/* Nsp 
 * Copyright (C) 2017-2017 Jean-Philippe Chancelier
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

#include <nsp/machine.h>
#include <nsp/interf.h>

extern function int_voronoi;
extern function int_delaunay;
extern function int_convhulln;

static OpWrapTab libqhull_func[] = {
  {"voronoi_internal", int_voronoi,NULL},
  {"delaunayn__", int_delaunay,NULL},
  {"convhulln", int_convhulln,NULL},
  {(char *) 0, NULL, NULL},
};
int libqhull_Interf (int i, Stack stack, int rhs, int opt, int lhs)
{
  if ( libqhull_func[i].wrapper == NULL)
     return (*(libqhull_func[i].fonc)) (stack, rhs, opt, lhs);
  else 
     return (*(libqhull_func[i].wrapper)) (stack, rhs, opt, lhs,libqhull_func[i].fonc);
}
void libqhull_Interf_Info (int i, char **fname, function (**f))
{
 *fname = libqhull_func[i].name;
 *f = libqhull_func[i].fonc;
}
