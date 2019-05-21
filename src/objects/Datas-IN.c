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

#include <nsp/nsp.h>
#include <nsp/object.h> 
#include <nsp/type.h> 
#include <nsp/hobj.h> 
#include <nsp/list.h> 
#include <nsp/smatrix.h> 
#include <nsp/bmatrix.h> 
#include <nsp/hash.h> 
#include <nsp/interf.h>
#include <nsp/datas.h>
#include <nsp/frame.h>
#include <nsp/libstab.h>
#include <nsp/funtab.h>
#include <nsp/nspthreads.h>
#include <nsp/nspdatas.h>

static const char *exists_list[] = {"all","caller", "callers", "local", "global", "function", "nsp-function", "callable", NULL};
typedef enum { in_all, in_caller, in_callers, in_local, in_global, in_function, in_macro, in_callable} _exist_tag;
static int nsp_exists(const char *Name, _exist_tag type,NspObject **ret);

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
  nsp_datas *data = nsp_get_datas();
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
	   * look at this object again in nsp_reorder_stack
	   * Ex:     a=5;function f();resume(a);endfunction
	   *         f() 
	   */
	  NthObj(i) = data->Reserved;
	} 
      else 
	{
	  /* as in the first branch but with a pointer */
	  NspHobj *hobj =(NspHobj *) NthObj(i) ;
	  if ( Ocheckname(hobj->O,NVOID) == FALSE ) 
	    hobj->O = data->Reserved;
	
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
  NspObject *Def=NULL;
  NspObject *Obj = NULLOBJ;
  char *name;
  int rep=in_callers; 
  nsp_option opts[] ={{"args",list,NULLOBJ,-1},
		      {"def",objcopy,NULLOBJ,-1},
		      { NULL,t_end,NULLOBJ,-1}};
  NspList *args = NULL;
  CheckStdRhs(1,2);
  CheckLhs(1,1);
  if ((name = GetString (stack, 1)) == (char *) 0) return RET_BUG;
  if (rhs -opt  == 2) { 
    if ((rep= GetStringInArray(stack,2,exists_list,1)) == -1) return RET_BUG; 
  }
  if ( get_optional_args(stack, rhs, opt, opts, &args,&Def) == FAIL )
    return RET_BUG;
  if ( nsp_exists(name,rep,&Obj)== FALSE ) 
    {
      if ( Def == NULL ) 
	{
	  Scierror("Error: object %s not found in callers environemnt\n",name);
	  return RET_BUG;
	}
      else 
	{
	  MoveObj(stack,1,Def);
	  return Max(lhs,1);
	}
    }
  /* Follow pointer */
  HOBJ_GET_OBJECT(Obj,RET_BUG);
  if (( Obj = nsp_object_copy(Obj)) == NULLOBJ) 
    return RET_BUG;
  MoveObj(stack,1,Obj);
  return Max(lhs,1);
}

/*
 * global('A','B',....,'Z',X=value,Y=value,....) : 
 * global(A=rand(4,5)): set A as a global variable and set it's value 
 *     to rand(4,5) if A was not already a global variable.
 */

static int int_global(Stack stack, int rhs, int opt, int lhs)
{
  NspObject *Def=NULL;
  char *str ;
  int i;
  for ( i = 1 ; i <= rhs - opt ; i++) 
    {
      if ((str = GetString(stack,i)) == (char*)0) return RET_BUG;
      if (nsp_declare_global(str,-1, Def)== FAIL) return RET_BUG;
    }
  for ( i = rhs -opt+1 ; i <= rhs ; i++)
    {
      if (( Def = nsp_get_object(stack,i)) == NULLOBJ ) return RET_BUG;
      if (nsp_declare_global(nsp_object_get_name(NthObj(i)),-1,Def) == FAIL) return RET_BUG;
    }
  return 0;
} 

static int int_persistent(Stack stack, int rhs, int opt, int lhs)
{
  NspObject *Obj=NULL;
  int i, used= FALSE;
  CheckStdRhs(0,0);
  for ( i = rhs -opt+1 ; i <= rhs ; i++)
    {
      /* GetObj takes care of Hobj pointers */
      if (( Obj =nsp_object_copy(nsp_get_object(stack,i))) == NULLOBJ ) return RET_BUG;
      if (nsp_object_set_name(Obj,nsp_object_get_name(NthObj(i))) == FAIL) return RET_BUG;
      if ( nsp_frame_set_persistent_value(Obj,&used) == FAIL)
	{
	  const char *str =  nsp_object_get_name(Obj);
	  Scierror("Error: failed to set persistent value for variable %s\n",str);
	  nsp_object_destroy(&Obj); /* Obj was not used */
	  return RET_BUG;
	}
      else
	{
	  if ( used == FALSE) nsp_object_destroy(&Obj);
	}
    }
  return 0;
} 

static int int_clear(Stack stack, int rhs, int opt, int lhs)
{
  int i;
  char *str;
  CheckLhs(0,1);
  if ( rhs == 0) 
    {
      nsp_frame_remove_all_objects();
    }
  else 
    {
      for ( i= 1; i <= rhs ; i++)
	{
	  if ((str = GetString(stack,i)) == (char*)0) return RET_BUG;
	  nsp_frame_remove_object(str);
	}
    }
  return 0;
}


static int int_clearglobal(Stack stack, int rhs, int opt, int lhs)
{
  int i;
  char *str;
  CheckLhs(1,1);
  if ( rhs == 0)
    {
      nsp_global_frame_remove_all_objects();
    }
  else 
    {
      for ( i= 1; i <= rhs ; i++)
	{
	  if ((str = GetString(stack,i)) == (char*)0) return RET_BUG;
	  nsp_global_frame_remove_object(str);
	}
    }
  return 0;
}


/*
 * Interface for exists 
 */


static int nsp_exists(const char *Name, _exist_tag type,NspObject **ret)
{
  NspObject *O=NULLOBJ;
  int irep=FALSE, vi=0,vn=0;
  switch (type) {
  case in_all: 
    if ((O=nsp_frames_search_object(Name)) != NULLOBJ) irep=TRUE;
    break;
  case in_local:
    if ((O=nsp_frame_search_object(Name)) != NULLOBJ)  irep=TRUE;
    break;
  case in_global:    
    if ((O=nsp_global_frame_search_object(Name)) != NULLOBJ) irep=TRUE;
    break;
  case in_function: 
    /* to be improved using the args optional argument if given */
    if ( nsp_find_function(Name,&vi,&vn) == OK) irep=TRUE;
    break;
  case in_callers: 
    if ((O= nsp_frames_search_local_in_calling(Name,FALSE)) != NULLOBJ) irep=TRUE;
    break;
  case in_caller:
    if ((O= nsp_frames_search_local_in_calling(Name,TRUE)) != NULLOBJ) irep=TRUE;
    break;
  case in_macro :
    if ((O= nsp_find_macro(Name)) != NULLOBJ) irep=TRUE;
    break;
  case in_callable:
    /* to be improved using the args optional argument if given */
    if ( nsp_find_function(Name,&vi,&vn) == OK 
	 || (O= nsp_find_macro(Name)) != NULLOBJ) 
      irep=TRUE;
    break;
  } 
  if ( irep == TRUE && O != NULLOBJ  )
    {
      /* take care that we can have in a local frame a pointer 
       * to a non existant global variable 
       */
      if ( IsHobj(O) && ((NspHobj *) O)->htype == 'g' )
	{
	  if ((O=nsp_global_frame_search_object(Name)) == NULLOBJ)  irep=FALSE;
	}
    }
  if ( ret != NULL) *ret = O;
  return irep;
}

/* returns the names of calling environments  */

static NspSMatrix *nsp_calling_tree()
{
  nsp_datas *data = nsp_get_datas();
  NspSMatrix *S;
  int i;
  int count=0;
  if ( data->L != NULLLIST ) 
    {
      Cell *C= data->L->first;
      while ( C != NULLCELL) 
	{
	  count++;
	  C = C->next ;
	}
    }
  if (( S = nsp_smatrix_create(NVOID,count,1,NULL,0))== NULL ) 
    return NULL;
  count=0;
  if ( data->L != NULLLIST ) 
    {
      Cell *C= data->L->first;
      while ( C != NULLCELL) 
	{
	  const char *name = C->O->name;
	  if (( S->S[count]= nsp_new_string(name,-1))==NULL )  goto err;
	  count++;
	  C = C->next ;
	}
    }
  return S;
 err:
  for ( i= count;  i < S->mn; i++ )  S->S[i]=NULL;
  nsp_smatrix_destroy(S);
  return NULL;
}

/* checks if variables exists 
 *
 */

static int int_exists(Stack stack, int rhs, int opt, int lhs)
{
  nsp_option opts[] ={{"args",list,NULLOBJ,-1},
		      { NULL,t_end,NULLOBJ,-1}};
  NspList *args = NULL;
  NspSMatrix *Names;
  NspBMatrix *B;
  int rep=0,i;
  CheckStdRhs(0,2);
  CheckLhs(1,1);
  if ( rhs -opt == 0 ) 
    {
      NspSMatrix *S=nsp_calling_tree();
      if ( S == NULL) return RET_BUG;
      MoveObj(stack,1,NSP_OBJECT(S));
      return 1;
    }
  if ((Names = GetSMat(stack,1)) == NULLSMAT)  return RET_BUG;
  if (rhs == 2) { 
    if ((rep= GetStringInArray(stack,2,exists_list,1)) == -1) return RET_BUG; 
  }
  if ( get_optional_args(stack, rhs, opt, opts, &args) == FAIL )
    return RET_BUG;
  if (( B = nsp_bmatrix_create(NVOID,Names->m,Names->n)) ==NULL) return FAIL;
  for ( i= 0 ; i < Names->mn ; i++)
    {
      B->B[i] = nsp_exists(Names->S[i],rep,NULL);
    }
  MoveObj(stack,1,NSP_OBJECT(B));
  return 1;
}

/* interface for who 
 *
 */

NspObject *nsp_who(Stack *stack,const char *frame, int as_hash, int print_only, int *error) 
{
  nsp_datas *data = nsp_get_datas();
  const char *frame_list[] = {"local", "global", "caller","constants", NULL};
  static nsp_frame_tag frame_tags[]={nsp_frame_local, nsp_frame_global, 
				     nsp_frame_caller, nsp_frame_constants};
  int rep = 0;
  Cell *C;
  NspObject *Res=NULL;
  NspFrame *F = NULL;
  if ( error != NULL) *error = FALSE;
  if ( print_only == TRUE ) as_hash = FALSE;
  if ( frame != NULL) 
    {
      rep = is_string_in_array(frame,frame_list,1);
      if ( rep < 0 ) 
	{
	  if ( error != NULL) *error = TRUE;
	  string_not_in_array(*stack,frame,frame_list,"optional argument frame");
	  return NULL;
	}
    }

  switch (frame_tags[rep]) 
    {
    case nsp_frame_local : 
      /* get current frame and return it as a hash table */
      if ( data->L == NULLLIST ) return NULL;
      F = (NspFrame *) data->L->first->O;
      break;
    case nsp_frame_global:
      /* get global frame and return it as a hash table */
      if ( (F= data->GlobalFrame) == NULLFRAME ) return NULL;
      break;
    case  nsp_frame_caller: 
      /* get caller frame and return it as a hash table */
      if ( data->L == NULLLIST ) return NULL;
      C = data->L->first->next;
      if (  C == NULLCELL)  return NULL;
      if ( ((NspFrame *) C->O) == data->ConstantFrame) 
	{
	  Scierror("Error: caller frame does not exist\n");
	  return NULL;
	}
      F = (NspFrame *) C->O;
      break;
    case nsp_frame_constants: 
      /* get constants frame and return it as a hash table */
      if ((F= data->ConstantFrame) == NULLFRAME ) return NULL;
      break;
    }
  
  Res= ( as_hash == TRUE) ? (NspObject *) nsp_eframe_to_hash(F)
    : (NspObject *) nsp_eframe_to_smat(F);
  if ( Res == NULL ) return NULL;
  if ( print_only == TRUE ) 
    {
      nsp_smatrix_print_multicols((NspSMatrix *) Res,0,"who",0);
      nsp_object_destroy(&Res);
      return NULL;
    }
  return Res;
}


static int nsp_int_who(Stack stack, int rhs, int opt, int lhs)
{
  char *frame = "local";
  int as_hash=FALSE, print_only=TRUE, error;
  int_types T0[] = {new_opts, t_end} ;
  int_types T1[] = {string, new_opts, t_end} ;

  nsp_option opts[] ={{"hash",s_bool,NULLOBJ,-1},
		      {"print_only",s_bool,NULLOBJ,-1},
		      { NULL,t_end,NULLOBJ,-1}};
  NspObject *Rep;
  CheckStdRhs(0,1);
  CheckLhs(0,1);

  if (rhs - opt == 0) 
    {
      if ( GetArgs(stack,rhs,opt,T0,&opts,&as_hash,&print_only) == FAIL)
	return RET_BUG;
    }
  else 
    {
      if ( GetArgs(stack,rhs,opt,T1,&frame,&opts,&as_hash,&print_only) == FAIL)
	return RET_BUG;
    }
  
  if ( lhs == 1 ) print_only = FALSE;
  Rep = nsp_who(&stack,frame,as_hash,print_only,&error);
  if ( error == TRUE ) return RET_BUG;
  if ( print_only == TRUE ) return 0;
  if ( Rep ==NULL && lhs >= 1) 
    {
      Scierror("Error: function who failed\n");
      return RET_BUG;
    }
  MoveObj(stack,1, Rep);
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
  {"who",nsp_int_who},
  {"frames_inhibit_search",int_frames_flag},
  {"frame_to_hash",int_frame_to_hash},
  {"acquire", int_nsp_acquire}, 
  {"persistent",int_persistent},
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






