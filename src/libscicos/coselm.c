/*------------------------------------------------------------------
 * evaluate scicos utility functions
 * rewriten for nsp 
 * Copyright Jean-Philippe Chancelier 
 *------------------------------------------------------------------*/

#include <math.h>
#include <stdio.h>
#include <string.h> 

#include "nsp/machine.h"
#include "nsp/matrix-in.h"
#include "nsp/bmatrix-in.h"


struct
{
  int kfun;
} C2F(curblk);



int int_var2vec(Stack stack, int rhs, int opt, int lhs) 
{
  Scierror("var2vec !!!!! \n");
  return RET_BUG;
}

int int_vec2var(Stack stack, int rhs, int opt, int lhs) 
{
  Scierror("vec2var !!!!\n");
  return RET_BUG;
}


int int_curblk(Stack stack, int rhs, int opt, int lhs) 
{
  NspMatrix *M;
  CheckRhs(-1,0) ;
  if ((M=nsp_matrix_create(NVOID,'r',1,1))==NULLMAT) return RET_BUG;
  M->R[0]= C2F(curblk).kfun;
  NSP_OBJECT(M)->ret_pos = 1;
  StackStore(stack,(NspObject *)M,1);
  return 1;
}


static char *var_names[]={ "inplnk","inpptr","ipar", "ipptr", "lnkptr", "outlnk",
			   "outptr", "outtb",  "rpar", "rpptr", 
			   "x", "xptr","z", "zptr",NULL};

const int reps[]={12,10,7,8,14,13,11,9,5,6,1,2,3,4};

int int_getscicosvars(Stack stack, int rhs, int opt, int lhs) 
{
  double *ptr; 
  int ierr,nv,type,i;
  NspMatrix *M;
  int rep;
  CheckRhs(1,1);
  CheckLhs(1,1);
  if ((rep= GetStringInArray(stack,1,var_names,1)) == -1) return RET_BUG; 
  ierr= scicos_getscicosvars(reps[rep],&ptr,&nv,&type);
  if ( ierr != 0 ) 
    {
      Scierror("scicosim is not running\n");
      return RET_BUG;
    }
  if ((M = nsp_matrix_create(NVOID,'r',nv,1)) == NULLMAT) return RET_BUG; 
  if ( type == 0 ) 
    for ( i = 0 ; i < M->mn; i++) M->R[i]= ((int *) ptr)[i];
  else 
    for ( i = 0 ; i < M->mn; i++) M->R[i]= ptr[i];
  MoveObj(stack,1,(NspObject *)M );
  return 1;
}

int int_setscicosvars(Stack stack, int rhs, int opt, int lhs) 
{
  double *ptr; 
  int ierr,nv,type,i, rep;
  NspMatrix *x1;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ((x1 = GetRealMatCopy(stack,1)) == NULLMAT) return RET_BUG;
  if ((rep= GetStringInArray(stack,2,var_names,1)) == -1) return RET_BUG; 
  ierr= scicos_getscicosvars(reps[rep],&ptr,&nv,&type);
  if ( ierr != 0 ) 
    {
      Scierror("scicosim is not running\n");
      return RET_BUG;
    }
  CheckLength(stack.fname,1,x1,nv) ;
  if( type == 0 )
    for ( i = 0 ; i < nv ; i++) ((int *) ptr)[i]= (int) x1->R[i];
  else 
    for ( i = 0 ; i < nv ; i++) ptr[i]=  x1->R[i];
  return 0;
}

int int_getlabel(Stack stack, int rhs, int opt, int lhs) 
{
  double kf;
  char *label=NULL;
  NspObject *Ob;
  CheckRhs(0,1);
  CheckLhs(1,1);
  /*  checking variable scale */
  if ( rhs == 1 ) 
    {
      if (GetScalarDouble(stack,1,&kf) == FAIL) return RET_BUG;
    }
  else
    {
      kf = C2F(curblk).kfun;
    }
  if ( scicos_getscilabel(&kf,&label)== FAIL) 
    {
      Scierror("scicosim is not running\n");
      return RET_BUG;
    }
  if (( Ob =(NspObject *) nsp_new_string_obj(NVOID,label,-1)) == NULL) return RET_BUG;
  MoveObj(stack,1,Ob);
  return 1;
}
