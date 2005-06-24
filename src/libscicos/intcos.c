/*------------------------------------------------------------------
 * Copyright ENPC 2004
 * Jean-Philippe Chancelier Enpc/Cermics
 * jpc@cermics.enpc.fr 
 *------------------------------------------------------------------*/

#include <math.h>
#include <stdio.h>
#include <string.h> 

#include "nsp/machine.h"
#include "nsp/matrix-in.h"
#include "nsp/bmatrix-in.h"
#include "scicos.h"

struct
{
  int kfun;
} C2F(curblk);

/* 
 * [state,t]=scicosim(state,tcur,tf,sim,'start' ,tol) 
 * 
 * sim=tlist(['scs','funs','xptr','zptr','zcptr','inpptr',..
 *           'outptr','inplnk','outlnk','lnkptr','rpar',..
 *	     'rpptr','ipar','ipptr','clkptr','ordptr',..
 *	     'execlk','ordclk','cord','oord','zord',..
 *	     'critev','nb','ztyp','nblk','ndcblk',..
 *	     'subscr','funtyp','iord','labels','modptr'],..
 *
 * state=tlist(['xcs','x','z','iz','tevts','evtspt','pointi','outtb'],..
 *               x,z,iz,tevts,evtspt,pointi,outtb)
 */

static int int_scicos_sim(Stack stack, int rhs, int opt, int lhs) 
{
  double tcur,tf;
  int i,nout,rep,flag,pointi,ierr=0,idb,nblk,count;
  static char *action_name[]={ "finish","linear", "run", "start", NULL };
  const int nstate = 7, nsim = 30;
  NspHash *State, *Sim;
  NspMatrix * State_elts[nstate], * Sim_elts[nsim];
  NspMatrix *Msimpar;
  NspBMatrix *funflag;
  NspList *funs;
  Cell *cloc;

  void **funptr;

  const char *sim[]={"funs","xptr","zptr","zcptr","inpptr",
		     "outptr","inplnk","outlnk","lnkptr","rpar",
		     "rpptr","ipar","ipptr","clkptr", "ordptr",
		     "execlk","ordclk","cord","oord","zord",
		     "critev","nb","ztyp","nblk","ndcblk", 
		     "subscr","funtyp","iord","labels","modptr"};

  char *state[] = {"x","z","iz","tevts","evtspt","pointi","outtb"};
  double simpar[7];
  CheckRhs(6,6);
  CheckLhs(1,2);
  /* first variable : the state */
  if ((State = GetHashCopy(stack,1)) == NULLHASH) return RET_BUG;
  for ( i = 0 ; i < nstate ; i++ ) 
    {
      NspObject *obj;
      if ( nsp_hash_find(State,state[i],&obj) == FAIL) return RET_BUG;
      State_elts[i]= (NspMatrix *) obj;
      if ( IsMat((NspObject *) State_elts[i]) == FALSE ) return RET_BUG;
      if ( ((NspMatrix *)State_elts[i])->rc_type != 'r') 
	{
	  Scierror("Elements are supposed to be real matrix \n");
	  return RET_BUG;
	}
    }
  nout = ((NspMatrix *) State_elts[6])->m;
  State_elts[4]=  Mat2int((NspMatrix *) State_elts[4]);
  pointi = State_elts[5]->R[0];
  /* next variables */
  if (GetScalarDouble(stack,2,&tcur) == FAIL) return RET_BUG;
  if (GetScalarDouble(stack,3,&tf) == FAIL) return RET_BUG;
  if ((Sim = GetHashCopy(stack,4)) == NULLHASH) return RET_BUG;
  for ( i = 0 ; i < nsim ; i++ ) 
    {
      NspObject *obj;
      if (nsp_hash_find(Sim,sim[i],&obj) == FAIL) return RET_BUG;
      Sim_elts[i]= (NspMatrix *) obj;
    }
  if ((rep= GetStringInArray(stack,5,action_name,1)) == -1) return RET_BUG; 
  switch (rep ) 
    {
    case 0: flag=3;break;
    case 1: flag=4;break;
    case 2: flag=2;break;
    case 3: flag=1;break;
    }
  /*      [atol  rtol ttol, deltat, scale, impl, hmax] */
  if ((Msimpar = GetRealMat(stack,6)) == NULLMAT) return RET_BUG;
  for ( i=Min(Msimpar->mn,7) ; i < 7 ; i++) simpar[i]= 0.0;
  for ( i=0 ; i < Min(Msimpar->mn,7) ; i++ ) simpar[i]= Msimpar->R[i];

  /* taille de funptr */
  nblk =Sim_elts[23]->R[0];

  if (( funflag = nsp_bmatrix_create(NVOID,nblk,1))== NULLBMAT) return RET_BUG;
  if (( funptr = malloc(nblk*sizeof(void *)))== NULL) return RET_BUG;
  /*
   * Sim_elts is a list of chars or functions 
   */
  funs = (NspList *) Sim_elts[0];
  cloc = funs->first ;
  count = 0;
  while ( cloc != NULLCELL) 
    {
      if ( cloc->O != NULLOBJ ) 
	{
	  if ( count >= nblk ) 
	    {
	      Scierror("Error: funs lenght should be %d\n",nblk);
	      return RET_BUG;
	    }
	  if ( IsString(cloc->O)) 
	    {
	      funflag->B[count]=FALSE;
	      funptr[count]=get_function(((NspSMatrix *) cloc->O)->S[0]);
	    }
	  else if ( IsNspPList(cloc->O) )
	    {
	      funflag->B[count]=TRUE;
	      funptr[count]=cloc->O;
	    }
	  else 
	    {
	      Scierror("Error: funs should contain strings or macros\n");
	      return RET_BUG;
	    }
	}
      count++;
      cloc = cloc->next;
    }
  
  Sim_elts[1]= Mat2int((NspMatrix *) Sim_elts[1]);   /* xptr */
  Sim_elts[2]= Mat2int((NspMatrix *) Sim_elts[2]);   /* zptr */
  Sim_elts[3]= Mat2int((NspMatrix *) Sim_elts[3]);   /* zcptr */
  Sim_elts[4]= Mat2int((NspMatrix *) Sim_elts[4]);   /* inpptr */
  Sim_elts[5]= Mat2int((NspMatrix *) Sim_elts[5]);   /* outptr */
  Sim_elts[6]= Mat2int((NspMatrix *) Sim_elts[6]);   /* inplnk */
  Sim_elts[7]= Mat2int((NspMatrix *) Sim_elts[7]);   /* outlnk */
  Sim_elts[8]= Mat2int((NspMatrix *) Sim_elts[8]);   /* lnkptr */
  Sim_elts[10]= Mat2int((NspMatrix *) Sim_elts[10]); /* rpptr  */
  Sim_elts[11]= Mat2int((NspMatrix *) Sim_elts[11]); /* ipar  */
  Sim_elts[12]= Mat2int((NspMatrix *) Sim_elts[12]); /* ipptr  */
  Sim_elts[13]= Mat2int((NspMatrix *) Sim_elts[13]); /* clkptr  */
  Sim_elts[14]= Mat2int((NspMatrix *) Sim_elts[14]); /* ordptr  */
  Sim_elts[16]= Mat2int((NspMatrix *) Sim_elts[16]); /* ordclk */
  Sim_elts[17]= Mat2int((NspMatrix *) Sim_elts[17]); /* cord */
  Sim_elts[18]= Mat2int((NspMatrix *) Sim_elts[18]); /* oord */
  Sim_elts[19]= Mat2int((NspMatrix *) Sim_elts[19]); /* zord */
  Sim_elts[20]= Mat2int((NspMatrix *) Sim_elts[20]); /* critev */
  Sim_elts[22]= Mat2int((NspMatrix *) Sim_elts[22]); /* ztyp */
  Sim_elts[25]= Mat2int((NspMatrix *) Sim_elts[25]); /* subscr */
  Sim_elts[26]= Mat2int((NspMatrix *) Sim_elts[26]); /* funtyp */
  Sim_elts[27]= Mat2int((NspMatrix *) Sim_elts[27]); /* iord */
  Sim_elts[29]= Mat2int((NspMatrix *) Sim_elts[29]); /* modptr */
  
  scicos_main(State_elts[0]->R, /* state x */
	      Sim_elts[1]->I, /* xptr */
	      State_elts[1]->R, /* state z0 */
	      State_elts[2]->R, /* state iz used to store pointers */
	      Sim_elts[2]->I, /* zptr */
	      Sim_elts[29]->I, /* modptr */
	      ((NspSMatrix *) Sim_elts[28])->S, /* labels */
	      &tcur,
	      &tf,	 
	      State_elts[3]->R, /* tevts */
	      State_elts[4]->I,&State_elts[4]->m,/* evtspt */
	      &pointi, 
	      State_elts[6]->R,&State_elts[6]->m, /* outtb */
	      funflag->B,
	      funptr,
	      Sim_elts[26]->I, /* funtyp */
	      Sim_elts[4]->I, /* inpptr */
	      Sim_elts[5]->I, /* outptr*/
	      Sim_elts[6]->I, 
	      Sim_elts[7]->I,
	      Sim_elts[8]->I,&Sim_elts[8]->m,
	      Sim_elts[9]->R,
	      Sim_elts[10]->I /* rpptr  */,
	      Sim_elts[11]->I /* ipar  */,
	      Sim_elts[12]->I /* ipptr  */,
	      Sim_elts[13]->I /* clkptr  */,
	      Sim_elts[14]->I,&Sim_elts[14]->mn /* ordptr  */,
	      Sim_elts[16]->I, /* ordclk */
	      Sim_elts[17]->I,&Sim_elts[17]->m /* cord */,
	      Sim_elts[27]->I,&Sim_elts[27]->m /* iord */,
	      Sim_elts[18]->I,&Sim_elts[18]->m /* oord */,
	      Sim_elts[19]->I,&Sim_elts[19]->m /* zord */,
	      Sim_elts[20]->I,&nblk, /* critev */
	      Sim_elts[22]->I /* ztyp */,
	      Sim_elts[3]->I /* zcptr */,
	      Sim_elts[25]->I,&Sim_elts[25]->m /* subscr */,
	      simpar,&flag,&ierr);
  idb=0;
  if (ierr > 0 )
    {
      switch (ierr) 
	{
	case 1 :
	  Scierror("Error: scheduling problem\n");
	  return RET_BUG;
	case 2:
	  Scierror("Error: input to zero-crossing stuck on zero\n");
	  return RET_BUG;
	case 6: 
	  Scierror("Error: a block has been called with input out of its domain\n");
	  return RET_BUG;
	case 7: 
	  Scierror("Error: singularity in a block\n");
	  return RET_BUG;
	case 8: 
	  Scierror("Error: block produces an internal error\n");
	  return RET_BUG;
	case 3: 
	  Scierror("Error: event conflict\n");
	  return RET_BUG;
	case 20: 
	  Scierror("Error: initial conditions not converging\n");
	  return RET_BUG;
	case 4: 
	  Scierror("Error: algrebraic loop detected\n");
	  return RET_BUG;
	case 5: 
	  Scierror("Error: cannot allocate memory\n");
	  return RET_BUG;
	case 21: 
	  Scierror("Error: cannot allocate memory in block="",i5)\n") ;
	  return RET_BUG;
	case 33 : 
	  Scierror("Error: sliding mode condition, cannot integrate");
	  return RET_BUG;
	default: 
	  if( ierr >= 1000) 
	    {
	      Scierror("Error: unknown or erroneous block\n");
	    }	      
	  else if(ierr >= 100) 
	    {
	      int istate=-(ierr-100);
	      Scierror("Error: integration problem istate=\"%d\"\n",istate);
	    }
	  else
	    {
	      Scierror("Error: scicos unexpected error,please report...\n"); 
	    }
	  return RET_BUG;
	}
    }
  /* 
   * back convert the state 
   */
  State_elts[4]=  Mat2double((NspMatrix *) State_elts[4]);
  State_elts[5]->R[0]=pointi;
  NthObj(1)->ret_pos = 1;
  if ( lhs == 2 ) 
    {
      if ( nsp_move_double(stack,2,tcur) == FAIL) return RET_BUG;
    }
  return Max(lhs,1);
}


static int int_sctree(Stack stack, int rhs, int opt, int lhs) 
{
  int iok,nord,nb,i;
  NspMatrix *M[5],*ilord,*ok,*work;
  /* [ord,ok]=sctree(vec,in,depu,outptr,cmatp); */
  /* taille nb et entier lv */
  CheckRhs(5,5);
  CheckLhs(1,1);
  for ( i = 0 ; i < 5 ; i++) 
    {
      if ((M[i] = GetRealMat(stack,i+1)) == NULLMAT) return RET_BUG;
      M[i]= Mat2int(M[i]);
    }
  nb = M[0]->mn;
  if ((ilord = nsp_matrix_create(NVOID,'r',1,nb)) == NULLMAT) return RET_BUG;
  if ((ok = nsp_matrix_create(NVOID,'r',1,1)) == NULLMAT) return RET_BUG;
  /* which size ? FIXME */
  if ((work = nsp_matrix_create(NVOID,'r',1,nb)) == NULLMAT) return RET_BUG;
  scicos_sctree(&nb,(int *)M[0]->R,(int *)M[1]->R,(int *)M[2]->R,(int *)M[3]->R,(int *)M[4]->R,
		(int *)ilord->R,&nord,&iok,(int *)work->R);
  /* renvoyer un tableau de taille nord copie de ilord */
  ilord->convert= 'i';
  ilord = Mat2double(ilord);
  if ( nsp_matrix_resize(ilord,nord,1) == FAIL) return RET_BUG;
  ok->R[0]=iok;
  MoveObj(stack,1,(NspObject *)ilord);
  if ( lhs == 2) MoveObj(stack,2,(NspObject *)ok);
  return Max(lhs,1);
}


static int int_tree2(Stack stack, int rhs, int opt, int lhs) 
{
  int nord,nmvec,iok,i;
  NspMatrix *M[4],*ipord,*ok;
  CheckRhs(4,4);
  CheckLhs(2,2);
  for ( i = 0 ; i < 4 ; i++) 
    {
      if ((M[i] = GetRealMat(stack,i+1)) == NULLMAT) return RET_BUG;
      M[i]= Mat2int(M[i]);
    }
  nmvec = M[0]->mn;
  if ((ipord = nsp_matrix_create(NVOID,'r',1,nmvec)) == NULLMAT) return RET_BUG;
  if ((ok = nsp_matrix_create(NVOID,'r',1,1)) == NULLMAT) return RET_BUG;
  /*
      if(.not.getrhsvar(1,'i',nvec,mvec,ipvec)) return
      if(.not.getrhsvar(2,'i',noin,moin,ipoin)) return
      if(.not.getrhsvar(3,'i',noinr,moinr,ipoinr)) return
      if(.not.getrhsvar(4,'i',ndep,mdep,ipdep)) return
      if(.not.createvar(5,'i',nvec*mvec,1,ipord)) return
      if(.not.createvar(6,'i',1,1,ipok)) return
  */

  scicos_ftree2(M[0]->I,&nmvec,M[3]->I,M[1]->I,M[2]->I,ipord->I,&nord,&iok);
  ipord->convert= 'i';
  ipord = Mat2double(ipord);
  if ( nsp_matrix_resize(ipord,nord,1) == FAIL) return RET_BUG;
  ok->R[0]=iok;
  MoveObj(stack,1,(NspObject *)ipord);
  if ( lhs == 2) MoveObj(stack,2,(NspObject *)ok);
  return Max(lhs,1);
}

static int int_tree3(Stack stack, int rhs, int opt, int lhs) 
{
  NspMatrix *M[7],*ipord,*ok,*ipkk;
  int i,iok,nord,nb;
  CheckRhs(7,7);
  CheckLhs(2,2);
  for ( i = 0 ; i < 7 ; i++) 
    {
      if ((M[i] = GetRealMat(stack,i+1)) == NULLMAT) return RET_BUG;
      M[i]= Mat2int(M[i]);
    }
  nb = M[0]->mn;
  if ((ipord = nsp_matrix_create(NVOID,'r',1,nb)) == NULLMAT) return RET_BUG;
  if ((ok = nsp_matrix_create(NVOID,'r',1,1)) == NULLMAT) return RET_BUG;
  if ((ipkk = nsp_matrix_create(NVOID,'r',1,nb)) == NULLMAT) return RET_BUG;

  scicos_ftree3(M[0]->I,&M[0]->mn,M[1]->I,M[2]->I,M[3]->I,M[4]->I,M[5]->I,M[6]->I,ipkk->I,
		ipord->I,&nord,&iok);
  ipord->convert= 'i';
  ipord = Mat2double(ipord);
  if ( nsp_matrix_resize(ipord,nord,1) == FAIL) return RET_BUG;
  ok->R[0]=iok;
  MoveObj(stack,1,(NspObject *)ipord);
  if ( lhs == 2) MoveObj(stack,2,(NspObject *)ok);
  return Max(lhs,1);
}

static int int_scicos_ftree4(Stack stack, int rhs, int opt, int lhs) 
{
  NspMatrix *M[5],*ipr1,*ipr2;
  int i,nmd,nr;
  CheckRhs(5,5);
  CheckLhs(2,2);
  for ( i = 0 ; i < 5 ; i++) 
    {
      if ((M[i] = GetRealMatCopy(stack,i+1)) == NULLMAT) return RET_BUG;
      M[i]= Mat2int(M[i]);
    }
  nmd = M[3]->mn;
  if ((ipr1 = nsp_matrix_create(NVOID,'r',1,nmd)) == NULLMAT) return RET_BUG;
  if ((ipr2 = nsp_matrix_create(NVOID,'r',1,nmd)) == NULLMAT) return RET_BUG;

  scicos_ftree4(M[0]->I,&M[0]->mn,M[3]->I,&M[3]->n,
		M[4]->I,M[1]->I,M[2]->I,ipr1->I,ipr2->I,&nr);
  ipr1->convert= 'i';
  ipr1 = Mat2double(ipr1);
  if ( nsp_matrix_resize(ipr1,nr,1) == FAIL) return RET_BUG;
  ipr2->convert= 'i';
  ipr2 = Mat2double(ipr2);
  if ( nsp_matrix_resize(ipr2,nr,1) == FAIL) return RET_BUG;
  MoveObj(stack,1,(NspObject *) ipr1);
  if ( lhs == 2) MoveObj(stack,2,(NspObject *)ipr2);
  return Max(lhs,1);
}


static int int_scicos_debug(Stack stack, int rhs, int opt, int lhs) 
{
  /*
    FIXME 
      subroutine  scicosdebug(fname)
      common /cosdebug/ cosd
      logical getrhsvar
      int cosd
      character*(*) fname
      if(.not.getrhsvar(1,'i',n,m,i)) return
      cosd=istk(i)
      lhsvar(1)=0
      end
  */
  return 0;
}


int scicos_connection(int* path_out,int* path_in) 
{
  /* FIXME : call the routine 
   * under_connection 
   * function ninnout=under_connection(path_out,path_in)
   */
  return 0;
}

int scicos_badconnection(int* path_out,int prt_out, int nout,int* path_in,int prt_in,int nin) 
{
  /* FXME : call the routine 
   * bad_connection(path_out,prt_out,nout,path_in,prt_in,nin)
   */
  return 0;
}

int scicos_Message(char* code) 
{
  /* FIXME call x_message 
   */
  return 0;
}

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

int int_curblock(Stack stack, int rhs, int opt, int lhs) 
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

int int_getblocklabel(Stack stack, int rhs, int opt, int lhs) 
{
  int kf;
  char *label=NULL;
  NspObject *Ob;
  CheckRhs(0,1);
  CheckLhs(1,1);
  /*  checking variable scale */
  if ( rhs == 1 ) 
    {
      if (GetScalarInt(stack,1,&kf) == FAIL) return RET_BUG;
    }
  else
    {
      kf = C2F(curblk).kfun;
    }
  if ( scicos_getscilabel(kf,&label)== FAIL) 
    {
      Scierror("Error: scicosim is not running\n");
      return RET_BUG;
    }
  if (( Ob =(NspObject *) nsp_new_string_obj(NVOID,label,-1)) == NULL) return RET_BUG;
  MoveObj(stack,1,Ob);
  return 1;
}



static OpTab Scicos_func[]={
  {"sci_tree4",int_scicos_ftree4},
  {"sci_sctree",int_sctree},
  {"sci_tree2",int_tree2},
  {"sci_tree3",int_tree3},
  {"sci_scicos_debug",int_scicos_debug},
  {"scicosim",int_scicos_sim},
  {"curblock", int_curblock},
  {(char *) 0, NULL}
};

int Scicos_Interf(int i, Stack stack, int rhs, int opt, int lhs)
{
  return (*(Scicos_func[i].fonc))(stack,rhs,opt,lhs);
}

/** used to walk through the interface table 
    (for adding or removing functions) **/

void Scicos_Interf_Info(int i, char **fname, function (**f))
{
  *fname = Scicos_func[i].name;
  *f = Scicos_func[i].fonc;
}

