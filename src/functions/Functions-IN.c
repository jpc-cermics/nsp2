/* Nsp
 * Copyright (C) 1998-2008 Jean-Philippe Chancelier Enpc/Cermics
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
 *
 *    link 
 *    addinter 
 *    call or fort 
 */

#include <math.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h> /* isxxxx */

#include "nsp/interf.h"
#include "nsp/stack.h"
#include "../system/files.h"
#include "callfunc.h"
#include "linking.h"
#include "addinter.h"

static void link_bug (int i);

/**
 * int_link:
 * @stack: 
 * @rhs: 
 * @opt: 
 * @lhs: 
 * 
 * interface for link function 
 * 
 * Return value: 
 **/

static int int_link(Stack stack, int rhs, int opt, int lhs)
{
  char shared_lib_expanded[FSIZE+1];
  char *Str,**enames=NULL,*shared_lib=NULL;
  NspSMatrix *Enames;  
  NspObject*OHMat;
  int ilib =0, iflag=1;
  CheckRhs(0,3);
  CheckLhs(0,1);

  if ( rhs == 0)
    {
      /* returns all the entry points in a hash table 
       */
      NspHash *obj = nsp_get_dlsymbols();
      if ( obj == NULLHASH) return RET_BUG;
      MoveObj(stack,1,NSP_OBJECT(obj));
      return 1;
    }
  /* first argument can be a number (ref to a previously linked library)
   * or a string giving a path to a new library to be linked.
   */
  if ( IsMatObj(stack,1 ) ) 
    {
      if (GetScalarInt(stack,1,&ilib) == FAIL) return RET_BUG;
    }
  else
    {
      if ((shared_lib = GetString(stack,1)) == NULLSTRING) return RET_BUG;
      iflag = 0;
    }
  if ( rhs > 1 ) 
    {
      /* list of entry names to be added */
      if ((Enames = GetSMat(stack,2)) == NULLSMAT) return RET_BUG;
      enames = Enames->S;
    }
  if ( rhs > 2 ) 
    {
      /* entries type: c or fortran */
      if ((Str = GetString(stack,3)) == (char*)0) return RET_BUG;
    }
  else
    { 
      Str = "c";
    }
  /* expand keys in path name result in buf */
  if ( shared_lib != NULL )
    {
      if  ( strcmp(shared_lib,"nsp") != 0 || strcmp(shared_lib,"scilab") != 0 )
	nsp_expand_file_with_exec_dir(&stack,shared_lib,shared_lib_expanded);
      else
	strcpy(shared_lib_expanded,shared_lib);
    }

  nsp_dynamic_load(shared_lib_expanded,enames,Str[0],&ilib,iflag,&rhs);
  if ( ilib < 0) 
    {
      link_bug(ilib); 
      return RET_BUG;
    }
  if ( (OHMat =nsp_create_object_from_int(NVOID,ilib)) == NULLOBJ) return RET_BUG;
  MoveObj(stack,1,OHMat);
  return 1;
}

static void  link_bug(int i)
{
  switch (i)
    {
    case -1 :Scierror("Error: the shared archive was not loaded\n");break;
    case -2 :Scierror("Error: cannot link more functions, maxentry reached\n");break;
    case -3 :Scierror("Error: first argument cannot be a number\n");break;
    case -4 :Scierror("Error: only one entry point is allowed\n\ton this operating system\n");break;
    case -5 :break;
    case -6: Scierror("link: problem with one of the entry point\n");break;
    default: Scierror("Error in function link\n");break;
    }
}

/*
 * interface for ulink function 
 */

static int int_ulink(Stack stack, int rhs, int opt, int lhs)
{
  int ilib;
  CheckRhs(1,1);
  CheckLhs(0,1);
  if (GetScalarInt(stack,1,&ilib) == FAIL) return RET_BUG;
  nsp_unlink_shared(ilib);
  return 0;
}

/*
 * interface for c_link function 
 *    [%t|%false,number]=c_link(name [,ilib]) 
 */

static int int_c_link(Stack stack, int rhs, int opt, int lhs)
{
  char *name;
  NspObject *O1,*O2=NULL;
  int ilib=-1,rep,irep;
  CheckRhs(1,2);
  CheckLhs(0,2);
  if ((name = GetString(stack,1)) == (char*)0) return RET_BUG;  
  if (rhs == 2 ) {
    if (GetScalarInt(stack,2,&ilib) == FAIL) return RET_BUG;
  }
  rep= nsp_is_linked(name,&ilib);
  irep = ( rep == -1) ? FALSE : TRUE;
  if ((O1 = nsp_create_boolean_object(NVOID,irep)) == NULLOBJ) return RET_BUG;
  MoveObj(stack,1,O1);
  if ( lhs == 2 ) 
    {
      if ( irep == FALSE ) ilib = -1;
      if (( O2 =nsp_create_object_from_int(NVOID,ilib)) == NULLOBJ) return RET_BUG;
      MoveObj(stack,2,O2);
    }
  return Max(lhs,1);
}


/*
 * addinter function 
 */

static int int_addinter(Stack stack, int rhs, int opt, int lhs)
{
  char file_expanded[FSIZE+1];
  int ilib=0;
  char *Str,*file=NULL;
  CheckRhs(2,2);
  CheckLhs(0,1);
  if ( IsMatObj(stack,1)) 
    {
      /* trying to find an interface in a preloaded 
       * shared library given by its id
       */
      if (GetScalarInt(stack,1,&ilib) == FAIL) return RET_BUG;
      if ((Str = GetString(stack,2)) ==  NULLSTRING) return RET_BUG;
      ilib = nsp_dynamic_interface(NULL,Str,ilib);
    }
  else
    {
      /* trying to load a shared library using a 
       * path-name
       */
      if ((file = GetString(stack,1)) == NULLSTRING) return RET_BUG;
      /* expand keys in path name result in buf */
      nsp_expand_file_with_exec_dir(&stack,file,file_expanded);
      if ((Str = GetString(stack,2)) ==  NULLSTRING) return RET_BUG;
      ilib = nsp_dynamic_interface(file_expanded,Str,ilib);
    }

  if ( ilib < 0 ) 
    {
      link_bug(ilib);
      return RET_BUG;
    }
  if ( nsp_move_double(stack,1,(double) ilib)== FAIL) return RET_BUG;
  return 1;
}

/*
 * call function, can be used to call a dynamically linked function 
 * we only emulate here the long scilab form.
 */

#define MAXPAR 31 

typedef int (f1) (void *);
typedef int (f2) (void *,void *);
typedef int (f3) (void *,void *,void *);
typedef int (f4) (void *,void *,void *,void *);
typedef int (f5) (void *,void *,void *,void *,void *);


static int int_call(Stack stack, int rhs, int opt, int lhs)
{
  function *f;
  /* posi[i]=j if i-th argument of function fname 
    is then j-th argument on the stack **/
  int posi[MAXPAR]={0}; 
  /* outpos[i]=j if i-th returned argument is the one 
    which is at position j on the stack **/
  int outpos[MAXPAR];
  /* flag to check that returned arguments are returned only once **/
  int checkout[MAXPAR]={0};
  /* ref[i] stores pointer for relevant data for i-th fname argument **/
  void *ref[MAXPAR];
  int inpos,i,newout=1,outarg,ismat,count_ref;
  char *Fname,*Type;
  char *Str;
  NspMatrix *M=NULLMAT;
  void *Data;
  CheckRhs(1,1000);
  CheckLhs(0,1000);
  /* first argument is the function name **/
  if ((Fname = GetString(stack,1)) == NULL) return RET_BUG;
  if ( SearchInDynLinks(Fname,&f) == -1 )
    {
      Scierror("Error: entry point %s not found\n",Fname);
      return RET_BUG;
    }

  /* checking input arguments arg,position,type 
   */
  i=2;
  while ( i <= rhs )
    {
      if ( IsSMatObj(stack,i) ) 
	{
	  if ((Str = GetString(stack,i)) == NULL) return RET_BUG;
	  Data = (void *) Str;
	  ismat = FAIL;
	  if ( strcmp("out",Str)==0)
	    {
	      /* we have reached keywork out **/
	      i++; break ;
	    }
	}
      else 
	{
	  /* Argument is a Matrix **/
	  if ( (M=GetRealMatCopy(stack,i))== NULLMAT) return RET_BUG;	  
	  Data = (void *) M->R;
	  ismat = OK;
	}
      if ( i + 2 > rhs ) 
	{
	  Scierror("Error: Not enough arguments to describe input variable %d\n",
		   (i-2)/3 +1 );
	  return RET_BUG;
	}
      if (GetScalarInt(stack,i+1,&inpos) == FAIL) return RET_BUG;
      if ((Type = GetString(stack,i+2))== NULL) return RET_BUG;
      /* Change data **/
      if ( ismat == OK ) 
	switch (Type[0]) 
	  {
	  case 'd' : break;
	  case 'r' :
	  case 'f' : Mat2float(M);break;
	  case 'i' : Mat2int(M);break;
	  default :
	    Scierror("Error: bad type conversion %c for variable %d\n",Type[0],
		   (i-2)/3 +1 );
	    return RET_BUG;
	  }
      if ( inpos <= 0 || inpos > MAXPAR) 
	{
	  Scierror("Error: input position %d for variable %d\n",inpos,
		   (i-2)/3 +1 );
	  Scierror("\tis out of range ]0,%d[\n",MAXPAR);
	  return RET_BUG;
	}
      if ( posi[inpos] != 0) 
	{
	  Scierror("Error: we have more than one variable (%d and %d) \n",posi[inpos],i);
	  Scierror("\tfor input variable %d\n",inpos);
	  return RET_BUG;
	}
      ref[inpos] = Data;
      posi[inpos] = i;
      i += 3;
    }
  /*  Checking output arguments                 
   */

  outarg = 0; /* counts output arguments **/
  while ( i <= rhs )
    {
      int pos1;
      if ((M=GetRealMatCopy(stack,i))== NULLMAT) return RET_BUG;
      if ( M->mn == 1) 
	{
	  /* output argument is only specified by its position in the input list **/
	  if (GetScalarInt(stack,i,&pos1) == FAIL) return RET_BUG;
	  if ( pos1 <= 0 || pos1 > MAXPAR) 
	    {
	      Scierror("Error: Output variable %d is given by its input position (%d)\n",
		       outarg+1,pos1);
	      Scierror("\twhich is out of range ]0,%d[\n",MAXPAR);
	      return RET_BUG;
	    }
	  if (posi[pos1] == 0 )
	    {
	      Scierror("Error: Output variable %d is supposed to be input variable %d\n",
		       outarg+1,pos1);
	      Scierror("\t but input variable %d does not exists\n",pos1);
	      return RET_BUG;
	    }
	  if ( checkout[pos1] > 0) 
	    {
	      Scierror("Error: an input variable (here %d) can appear only once in output list\n",
		       pos1);
	      return RET_BUG;
	    }
	  outpos[outarg]= posi[pos1];
	  checkout[pos1]= 1;
	  i++;
	}
      else if ( M->mn != 2) 
	{
	  Scierror("Error: Incorrect dimensions (%dx%d) for output variable %d\n",
		   M->m,M->n,outarg+1);
	  return RET_BUG;
	}
      else 
	{
	  /* [m,n],pos,type **/
	  if ( i+2 > rhs ) 
	    {
	      Scierror("Error: Not enough arguments to describe output variable %d\n",
		       outarg+1);
	      return RET_BUG;
	    }
	  /* M gives output size **/
	  if (GetScalarInt(stack,i+1,&pos1) == FAIL) return RET_BUG;
	  if ((Type = GetString(stack,i+2))== NULL) return RET_BUG;
	  if ( pos1 <= 0 || pos1 > MAXPAR) 
	    {
	      Scierror("Error: Output variable %d is given by a position (%d)\n",
		       outarg+1,pos1);
	      Scierror("\twhich is out of range ]0,%d[\n",MAXPAR);
	      return RET_BUG;
	    }
	  if ( posi[pos1] != 0 ) 
	    {
	      /* output variable is an input variable **/
	      /* Check size compatibility XXXXXXXXXX **/
	      if ( checkout[pos1] > 0) 
		{
		  Scierror("Error: an input variable (here %d) can appear only once in output list\n",
			   pos1);
		  return RET_BUG;
		}
	      outpos[outarg] = posi[pos1];
	      checkout[pos1]= 1;
	    }
	  else 
	    {
	      /* Create new variable **/
	      NspSMatrix *S;
	      NspMatrix *Loc;
	      char *lstr;
	      switch (Type[0])
		{
		case 'c' :
		  /* A revoir pour faire plus court  XXXXX **/
		  if ((S=nsp_smatrix_create_with_length(NVOID,1,1,M->mn)) == NULLSMAT ) return RET_BUG;
		  NthObj(rhs+newout) = (NspObject *) S;
		  if ((lstr =new_nsp_string_n(M->mn)) == (nsp_string) 0) 
		    return RET_BUG;
		nsp_string_destroy(&(S->S[0] ));
		  Data = (void *) S->S[0];
		  break;
		default :
		  if ((Loc = nsp_matrix_create(NVOID,'r',M->R[0],M->R[1]))==  NULLMAT)  return RET_BUG;
		  NthObj(rhs+newout)= (NspObject *) Loc;
		  Data = (void *) Loc->R;
		  break;
		}
	      posi[pos1]=i;
	      ref[pos1] = Data;
	      outpos[outarg] = rhs+newout ;
	      checkout[pos1]= 1;
	      newout++;
	    }
	  i += 3;
	}
      outarg++;      
    }
  /* Calling the interfaced routine **/
  count_ref = 0;
  for ( i= 1 ; i <= rhs ; i++) if (posi[i] != 0) count_ref++;
  switch (count_ref) 
    {
    case 1: ((f1 *) f)(ref[1]); break;
    case 2: ((f2 *) f)(ref[1],ref[2]);break;
    case 3: ((f3 *) f)(ref[1],ref[2],ref[3]);break;
    case 4: ((f4 *) f)(ref[1],ref[2],ref[3],ref[4]);break;
    case 5: ((f5 *) f)(ref[1],ref[2],ref[3],ref[4],ref[5]);break;
    default: 
      Scierror("Error: no more that 5 transmited arguments in call\n");
      
      return RET_BUG;
    }
  /* put output arguments on the stack : all the outpos[i] are differents 
    and outpos is changed after the call to PutLhsObj **/
  PutLhsObj(stack,Min(lhs,outarg),outpos);
  /* Check if we need to change Data i->d or r->d **/
  for ( i=1 ; i <= Min(lhs,outarg) ; i++) 
    ObjConvert(NthObj(i));
  /* Check if we need to change output dimensions **/
  return Min(lhs,outarg);
}


/*************************************************************
 * The Interface for basic function operation 
 *************************************************************/

extern function  int_swigvarlink_create;

static OpTab Functions_func[]={
  {"link",int_link},
  {"ulink",int_ulink},
  {"c_link",int_c_link},
  {"addinter",int_addinter},
  {"call",int_call},
  {"swigvarlink_create", int_swigvarlink_create},
  {(char *) 0, NULL}
};

int Functions_Interf(int i, Stack stack, int rhs, int opt, int lhs)
{
  return (*(Functions_func[i].fonc))(stack,rhs,opt,lhs);
}

/*
 * used to walk through the interface table 
 * (for adding or removing functions) 
 */

void Functions_Interf_Info(int i, char **fname, function (**f))
{
  *fname = Functions_func[i].name;
  *f = Functions_func[i].fonc;
}

