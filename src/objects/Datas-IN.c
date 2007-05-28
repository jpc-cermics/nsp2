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
#include "nsp/interf.h"
#include "nsp/datas.h"
#include "frame.h"

extern NspObject *Reserved;


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
	  Scierror("\t%s of function %s\n",ArgPosition(rhs),NspFname(stack));
	  return RET_BUG;
	}
      /* A copy of object moved in the calling frame 
       * we must take care here of the fact that when we move an 
       * object in an upper frame this can lead to the destruction 
       * of an object which is in the calling stack.
       */
      /* GetObj takes care of Hobj pointers */
      if (( O =nsp_object_copy(nsp_get_object(stack,i))) == NULLOBJ ) return RET_BUG;
      if (nsp_object_set_name(O,nsp_object_get_name(NthObj(i))) == FAIL) return RET_BUG;
      /* tricky  */
      if( IsHobj(NthObj(i))== FALSE ) 
	{
	  /* here NthObj(i) can be an object of the calling frame : which can 
	   * be destroyed by nsp_frame_move_up_object so we do not want to 
	   * look at this object again in reorder_stack
	   * Ex:     a=5;function f();resume(a);endfunction
	   *         f() 
	   */
	  NthObj(i) = Reserved;
	} 
      else 
	{
	  /* as in the first branch but with a pointer */
	  NspHobj *hobj =(NspHobj *) NthObj(i) ;
	  if ( Ocheckname(hobj->O,NVOID) == FALSE ) 
	    hobj->O = Reserved;
	
	}
      /* A copy of object is added in the upper env **/
      if (nsp_frame_move_up_object(O) == FAIL) return RET_BUG;

    }
  return 0;
}

/* return a copy in the local frame of 
 * object from calling frames 
 */

static int int_nsp_acquire(Stack stack, int rhs, int opt, int lhs)
{
  char *name;
  NspObject *Obj;
  CheckRhs(1,1);
  CheckLhs(1,1);
  if ((name = GetString (stack, 1)) == (char *) 0) return RET_BUG;
  if (( Obj =nsp_frames_search_object(name)) == NULLOBJ) 
    {
      Scierror("Error: object %s not found in callers environemnt\n",name);
      return RET_BUG;
    }
  if (( Obj = nsp_object_copy(Obj)) == NULLOBJ) 
    return RET_BUG;
  MoveObj(stack,1,Obj);
  return Max(lhs,1);
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
      if (nsp_declare_global(str,-1)== FAIL) return RET_BUG;
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
  if ((O = nsp_create_boolean_object(NVOID,irep)) == NULLOBJ) return RET_BUG;
  /* if (( O =nsp_create_object_from_double(NVOID,irep))== NULLOBJ ) return RET_BUG;*/
  MoveObj(stack,1,O);
  return 1;
}


/*
 *
 */

extern NspFrame  *GlobalFrame;
extern NspFrame  *ConstantFrame;

static int int_who(Stack stack, int rhs, int opt, int lhs)
{
  Cell *C;
  int rep = 0;
  static char *exists_list[] = {"local", "global", "caller","constants", NULL};
  NspHash *H;
  CheckRhs(0,1);
  CheckLhs(1,1);
  if ( rhs == 1 )
    {
      if ((rep= GetStringInArray(stack,1,exists_list,1)) == -1) return RET_BUG; 
    }
  switch (rep) 
    {
    case 0: 
      /* get current frame and return it as a hash table */
      if ( Datas == NULLLIST ) return RET_BUG;
      if ((H= nsp_eframe_to_hash((NspFrame *) Datas->first->O)) == NULLHASH) return RET_BUG;
      break;
    case 1:
      if ( GlobalFrame == NULLFRAME ) return RET_BUG;
      if ((H= nsp_eframe_to_hash(GlobalFrame)) == NULLHASH) return RET_BUG;
      break;
    case 2:
      if ( Datas == NULLLIST ) return RET_BUG;
      C = Datas->first->next;
      if (  C == NULLCELL)  return RET_BUG;
      if ( ((NspFrame *) C->O) == ConstantFrame) 
	{
	  Scierror("Error: caller frame does not exist\n");
	  return RET_BUG;
	}
      if ((H= nsp_eframe_to_hash((NspFrame *) C->O)) == NULLHASH) return RET_BUG;
      break;
    case 3: 
      if ( ConstantFrame == NULLFRAME
 ) return RET_BUG;
      if ((H= nsp_eframe_to_hash(ConstantFrame)) == NULLHASH) return RET_BUG;
      break;
    }
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

/* FIXME: temporary here 
 * just to test the frames search inhibit 
 */

int frames_search_inhibit = FALSE ;

static int int_frames_flag(Stack stack, int rhs, int opt, int lhs)
{
  CheckRhs(1,1);
  CheckLhs(0,0);
  if ( GetScalarBool (stack,1,&frames_search_inhibit) == FAIL) return RET_BUG;
  return 0;
}



static int int_frame_to_hash(Stack stack, int rhs, int opt, int lhs)
{
  NspHash *H;
  CheckRhs(0,0);
  CheckLhs(0,1);
  if ((H=nsp_current_frame_to_hash()) == NULLHASH) return RET_BUG;
  MoveObj(stack,1,NSP_OBJECT(H));
  return 1;
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
  {"frames_inhibit_search",int_frames_flag},
  {"frame_to_hash",int_frame_to_hash},
  {"acquire", int_nsp_acquire}, 
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






