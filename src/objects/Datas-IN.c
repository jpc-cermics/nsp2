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
#include "nsp/datas-in.h"

/*
 * Now the interfaced function for frame operations 
 */

/*
 * resume(A,B,C=rand(4,4),......) 
 * move copies of object A to the next frame 
 * (calling frame)
 * XXX: resume is not as in Scilab i.e 
 *      it does not imply a return 
 * the syntax 
 * resume(A=56,B=V,....) is also accepted 
 */

static int int_dataresume(Stack stack, int rhs, int opt, int lhs)
{
  NspObject *O;
  int i;
  CheckRhs(1,1000);
  CheckLhs(1,1);
  for ( i = 1 ; i <= rhs ; i++) 
    {
      if ( Ocheckname(NthObj(i),NVOID) ) 
	{
	  Scierror("Error: Cannot resume an unnamed value\n");
	  Scierror("\t%s of function %s\n",ArgPosition(rhs),stack.fname);
	  return RET_BUG;
	}
      /* A copy of object is added in the hash table **/
      /* GetObj takes care of Hobj pointers **/
      if (( O =nsp_object_copy(nsp_get_object(stack,i))) == NULLOBJ ) return RET_BUG;
      if (nsp_object_set_name(O,nsp_object_get_name(NthObj(i))) == FAIL) return RET_BUG;
      if (nsp_frame_move_up_object(O) == FAIL) return RET_BUG;
      /* A copy of object is added in the hash table **/
    }
  return 0;
}

/*
 * global('A','B',.....) : set a b etc... as global 
 *   variables 
 */

static int int_global(Stack stack, int rhs, int opt, int lhs)
{
  int i;
  char *str;
  CheckRhs(1,1000);
  CheckLhs(1,1);
  for ( i= 1; i <= rhs ; i++)
    {
      if ((str = GetString(stack,i)) == (char*)0) return RET_BUG;
      if (nsp_declare_global(str)== FAIL) return RET_BUG;
    }
  return 0;
}


static int int_clear(Stack stack, int rhs, int opt, int lhs)
{
  int i;
  char *str;
  CheckRhs(1,1000);
  CheckLhs(1,1);
  for ( i= 1; i <= rhs ; i++)
    {
      if ((str = GetString(stack,i)) == (char*)0) return RET_BUG;
      nsp_frame_remove_object(str);
    }
  return 0;
}


static int int_clearglobal(Stack stack, int rhs, int opt, int lhs)
{
  int i;
  char *str;
  CheckRhs(1,1000);
  CheckLhs(1,1);
  for ( i= 1; i <= rhs ; i++)
    {
      if ((str = GetString(stack,i)) == (char*)0) return RET_BUG;
      nsp_global_frame_remove_object(str);
    }
  return 0;
}


/*
 * Interface for exists 
 * XXX shoul be changed in order to load a macro 
 * in the current env if this macros is in the search list 
 */

static char *exists_list[] = {"all", "local", "global", "function", NULL};

static int int_exists(Stack stack, int rhs, int opt, int lhs)
{
  NspObject *O;
  int rep=0, irep=0;
  char *Name=0;
  CheckRhs(1,2);
  CheckLhs(1,1);
  if ((Name = GetString(stack,1)) == (char*)0) return RET_BUG;
  if (rhs == 2) { 
    if ((rep= GetStringInArray(stack,2,exists_list,1)) == -1) return RET_BUG; 
  }
  switch (rep) {
  case 0: 
    if (nsp_frames_search_object(Name) != NULLOBJ) irep=1;
    break;
  case 1:
    if (nsp_frame_search_object(Name) != NULLOBJ) irep=1;
    break;
  case 2:    
    if (nsp_global_frame_search_object(Name) != NULLOBJ) irep=1;
    break;
  case 3: 
    if (nsp_global_frame_search_object(Name) != NULLOBJ) irep=1;
    break;
  } 
  /* XXX should be changed 
   * if ((O = nsp_create_boolean_object(NVOID,irep)) == NULLOBJ) return RET_BUG;
   */
  if (( O =nsp_create_object_from_double(NVOID,irep))== NULLOBJ ) return RET_BUG;
  MoveObj(stack,1,O);
  return 1;
}


/*
 *
 */

static int int_who(Stack stack, int rhs, int opt, int lhs)
{
  NspHash *H;
  CheckRhs(-1,0);
  CheckLhs(1,1);
  /* get current frame and return it as a hash table */
  if ( Datas == NULLLIST ) 
    {
      return RET_BUG;
    }
  if ((H = nsp_hcreate_from_list(NVOID,-1,(NspList *) Datas->first->O))== NULLHASH) return RET_BUG;
  MoveObj(stack,1,(NspObject *) H);
  return 1;
}



/*
 * insert_env 
 */

static int int_insert_env(Stack stack, int rhs, int opt, int lhs)
{
  NspHash *H;
  CheckRhs(1,1);
  CheckLhs(0,0);
  if ( (H= GetHash(stack,1)) == NULLHASH ) return RET_BUG;
  if (  nsp_frame_insert_hash_contents(H) == FAIL) return RET_BUG;
  return 0;
}





/*
 * The Interface for basic datas operations 
 */

static OpTab Datas_func[]={
  {"resume",int_dataresume},
  {"global",int_global},
  {"exists",int_exists},
  {"clear",int_clear},
  {"clearglobal",int_clearglobal},
  {"insert_env",int_insert_env},
  {"who",int_who},
  {(char *) 0, NULL}
};

int Datas_Interf(int i, Stack stack, int rhs, int opt, int lhs)
{
  return (*(Datas_func[i].fonc))(stack,rhs,opt,lhs);
}

/* used to walk through the interface table 
    (for adding or removing functions) **/

void Datas_Interf_Info(int i, char **fname, function (**f))
{
  *fname = Datas_func[i].name;
  *f = Datas_func[i].fonc;
}






