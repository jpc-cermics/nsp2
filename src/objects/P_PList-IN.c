/* Nsp
 * Copyright (C) 1998-2005 Jean-Philippe Chancelier Enpc/Cermics
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

#include "nsp/machine.h"
#include "nsp/object.h"
#include "nsp/stack.h"
#include "nsp/p_plist-in.h" 

/*
 * NspPListPrint : writes parsed expression
 */

int int_plprint(Stack stack, int rhs, int opt, int lhs)
{
  NspPList *PL;
  CheckRhs(1,1);
  CheckLhs(1,1);
  if ((PL = NspPListObj(NthObj(1))) == NULLP_PLIST) return RET_BUG;
  NspPListPrint(PL,0);
  return 0;
}


/*
 * NspPListPrInt : writes parsed expression in info mode
 */

int int_plinfo(Stack stack, int rhs, int opt, int lhs)
{
  NspPList *PL;
  CheckRhs(1,1);
  CheckLhs(1,1);
  if ((PL = NspPListObj(NthObj(1))) == NULLP_PLIST) return RET_BUG;
  NspPListInfo(PL,0);
  return 0;
}

/*
 * NspPList2SMatrix 
 */

int int_pl2s(Stack stack, int rhs, int opt, int lhs)
{
  NspSMatrix *S;
  NspPList *PL;
  CheckRhs(1,1);
  CheckLhs(1,1);
  if ((PL = NspPListObj(NthObj(1))) == NULLP_PLIST) return RET_BUG;
  if ((S= NspPList2SMatrix(PL,0))== NULL) return RET_BUG;
  MoveObj(stack,1,NSP_OBJECT(S));
  return 1;
}

/*
 * NspPListSave : writes parsed expression in save mode
 */

int int_plsave(Stack stack, int rhs, int opt, int lhs)
{
  NspPList *PL;
  CheckRhs(1,1);
  CheckLhs(1,1);
  if ((PL = NspPListObj(NthObj(1))) == NULLP_PLIST) return RET_BUG ;
  if ( NspPListSave(PL) == FAIL ) return RET_BUG;
  return 0;
}

/*
 * The Interface for parsed lists
 */

static OpTab NspPList_func[]={
  {"print_pl", int_plprint},
  {"info_pl", int_plinfo },
  {"save_pl", int_plsave },
  {"pl2s", int_pl2s},
  {(char *) 0, NULL}
};

int NspPList_Interf(int i, Stack stack, int rhs, int opt, int lhs)
{
  return (*(NspPList_func[i].fonc))(stack,rhs,opt,lhs);
}


/* used to walk through the interface table 
    (for adding or removing functions) **/

void NspPList_Interf_Info(int i, char **fname, function (**f))
{
  *fname = NspPList_func[i].name;
  *f = NspPList_func[i].fonc;
}

