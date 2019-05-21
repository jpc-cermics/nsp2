/* Nsp
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

#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "nsp/object.h"
#include "nsp/stack.h"
#include "nsp/datas.h"

/*
 * Emulation of evaluation 
 */

int main()
{
  NspMatrix *M;
  NspObject *OM,*OM1;
  nsp_new_frame("datas");
  if ((M =nsp_mat_rand(2,2)) == NULLMAT) {exit(1);}
  nsp_object_set_name(M,"pipo");
  nsp_frame_replace_object(M);
  OM1=nsp_object_copy(M);
  nsp_object_set_name(OM1,"foo");
  nsp_frame_replace_object(OM1);
  nsp_matrix_print(M,0);
  return 0;
}



