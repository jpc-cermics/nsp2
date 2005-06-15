#include <string.h>
#include <stdio.h>
#include <math.h>
#include "nsp/machine.h"
#include "nsp/interf.h"
#include "intcscicos.h"
#include "scicos_block.h"

/*
typedef int (*des_interf) (char *fname,unsigned long l);

typedef struct table_struct {
  des_interf f;   
  char *name;     
} intcscicosTable;

static intcscicosTable Tab[]={
  {inttimescicos,"scicos_time"},
  {intduplicate,"duplicate"},
  {intdiffobjs,"diffobjs"},
  {intxproperty,"pointer_xproperty"},
  {intphasesim,"phase_simulation"},
  {intsetxproperty,"set_xproperty"},
  {intsetblockerror,"set_blockerror"},
};

int C2F(intcscicos)()
{  
  Rhs = Max(0, Rhs);
  (*(Tab[Fin-1].f)) (Tab[Fin-1].name,strlen(Tab[Fin-1].name));
  C2F(putlhsvar)();
  return 0;
}

*/

/* fonction pour recuperer le nombre du champs a partir de son nom */
int MlistGetFieldNumber(int *ptr, const char *string)
{
  int nf, longueur, istart, k, ilocal, retval;
  int *headerstr;
  static char str[24];
 
  headerstr = listentry(ptr,1);
  nf=headerstr[1]*headerstr[2]-1;  /* number of fields */
  retval=-1;
  for (k=0; k<nf; k++) {
    longueur=Min(headerstr[6+k]-headerstr[5+k],24);  /* size of kth fieldname */
    istart=5+nf+headerstr[5+k];    /* start of kth fieldname code */
    /*    istart=8+headerstr[4+nf+k]; */
    C2F(cvstr)(&longueur, &headerstr[istart], str, (ilocal=1, &ilocal),longueur);
    str[longueur]='\0';
    if (strcmp(string, str) == 0) {
      retval=k+2;
      break;}
  }
  return retval;
}


static int int_time_scicos(Stack stack, int rhs, int opt, int lhs) 
{ 
  CheckRhs(-1,0);
  CheckLhs(1,1);
  if ( nsp_move_double(stack,1,(double) get_scicos_time() )== FAIL) return RET_BUG;
  return 1;
}

/* v=duplicate(u,count) 
 * returns v=[u(1)*ones(count(1),1);
 *            u(2)*ones(count(2),1);
 *            ...
 */



static int duplicata(int n,const double *v,const double *w,double *ww)
{
  int i,j,k;
  k=0;
  for ( i=0 ; i< n ; i++) 
    {
      for (j=0 ; j<(int) w[i] ; j++) 
	{
	  ww[k]=v[i];
	  k=k+1;
	}
    }
  return k;
}

static int comp_size(const double *v,int n)
{  
  int i;
  int nw=0;
  for (i=0;i<n;i++) {
    if (v[i]>0) nw += (int) v[i];
  }
  return nw;
}

static int int_duplicate(Stack stack, int rhs, int opt, int lhs) 
{
  int nres;
  NspMatrix *A,*B,*Res;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ((A = GetRealMat(stack,1)) == NULLMAT) return RET_BUG;
  if ((B = GetRealMat(stack,2)) == NULLMAT) return RET_BUG;
  if( A->mn == 0) 
    {
      if (( Res =nsp_matrix_create(NVOID,'r',0,0)) == NULLMAT) return  RET_BUG;
      MoveObj(stack,1,NSP_OBJECT(Res));
      return 1;
    }
  CheckSameDims (stack.fname, 1, 2, A, B);
  nres = comp_size(B->R,A->mn);
  if (( Res =nsp_matrix_create(NVOID,'r',nres,1)) == NULLMAT) return  RET_BUG;
  nres= duplicata(A->mn,A->R,B->R,Res->R);
  MoveObj(stack,1,NSP_OBJECT(Res));
  return 1;
}


/*   diffobjs(A,B) returns 0 if A==B and 1 if A and B differ */

static int int_diffobjs(Stack stack, int rhs, int opt, int lhs) 
{
  return 0;
}

/* renvoi le type d'equation get_pointer_xproperty() 
 *	(-1: algebriques, +1 differentielles) 
 */

static int int_xproperty(Stack stack, int rhs, int opt, int lhs) 
{
  /* 
  int un;
  extern int* pointer_xproperty;
  extern int n_pointer_xproperty;
  CheckRhs(-1,0);
  CheckLhs(1,1);
  CreateVarFromPtr(1,"i",&n_pointer_xproperty,(un=1,&un),&pointer_xproperty);
  LhsVar(1)=1;
  */
  return 0;
}

/* renvoi la phase de simulation phase=get_phase_simulation() */
 
static int int_phasesim(Stack stack, int rhs, int opt, int lhs) 
{ 
  CheckRhs(-1,0);
  CheckLhs(1,1);
  if ( nsp_move_double(stack,1,(double) get_phase_simulation() )== FAIL) return RET_BUG;
  return 1;
}

/* renvoi le type d'equation get_pointer_xproperty() 
 *	(-1: algebriques, +1 differentielles) 
 */
 
static int int_setxproperty(Stack stack, int rhs, int opt, int lhs) 
{
  int m1;
  CheckRhs(1,1);
  if ( GetScalarInt(stack,1,&m1) == FAIL) return RET_BUG;
  set_pointer_xproperty(&m1);
  return 0;
}


static int intsetblockerror(Stack stack, int rhs, int opt, int lhs) 
{
  int m1;
  CheckRhs(1,1);
  if ( GetScalarInt(stack,1,&m1) == FAIL) return RET_BUG;
  set_block_error(m1);
  return 0;
}


