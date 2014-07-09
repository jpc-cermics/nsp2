/* Nsp
 * Copyright (C) 2013-2014 J.-Ph. Chancelier Cermics/ENPC 
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
 */

#include <nsp/nsp.h>
#include <nsp/objects.h>
#include <nsp/imatrix.h>
#include <nsp/spcolmatrix.h>
#include <nsp/interf.h>

#include "clp_cpp.h"
#include "coinmp_cpp.h"

static int nsp_spcolmatrix_to_sparse_triplet(NspSpColMatrix *A1,NspSpColMatrix *A2, NspIMatrix **Matbeg,NspIMatrix **Matind, NspMatrix **Matval);
static int nsp_matrix_to_sparse_triplet(NspMatrix *A1,NspMatrix *A2, NspIMatrix **Matbeg,NspIMatrix **Matind, NspMatrix **Matval);

static int get_clp_options(Stack stack, NspHash *solver_options, nsp_clp_params *clp_params);
  
int int_clp_solve(Stack stack, int rhs, int opt, int lhs)
{
  const double coin_dbl_max= nsp_coin_dbl_max();
  char *sense_str = "min";
  NspMatrix *X,*Lambda, *Retcode, *RetCost;
  NspIMatrix *Cmatbeg=NULL,*Cmatind=NULL;
  NspMatrix *Cmatval=NULL;
  NspIMatrix *Qmatbeg=NULL,*Qmatind=NULL;
  NspMatrix *Qmatval=NULL;
  NspObject *ObjA,*ObjAe, *ObjQ=NULL;
  NspMatrix *Objective, *Rhs, *Rhse, *B, *Lhs, *lb=NULL, *ub=NULL;
  NspHash *Options = NULLHASH;
  NspSMatrix *var_type = NULLSMAT;
  nsp_clp_params options;
  int neq, ncols,nrows,i, sense=0;

  /* Copy Rhs since Rhs will be concatenated with Rhse */
  int_types T[] = {realmat, obj , realmat, obj , realmat, new_opts, t_end} ;
  
  nsp_option opts[] ={
    {"Q", obj,NULLOBJ,-1},
    {"lb",realmatcopy,NULLOBJ,-1},
    {"ub",realmatcopy,NULLOBJ,-1},
    {"sense",string,NULLOBJ,-1},
    {"var_type",smat,NULLOBJ,-1},
    {"options", hash, NULLOBJ,-1},
    { NULL,t_end,NULLOBJ,-1}};
  
  if ( GetArgs(stack,rhs,opt,T,&Objective, &ObjA, &Rhs, &ObjAe, &Rhse, &opts,&ObjQ,&lb,&ub,&sense_str,&var_type,&Options) == FAIL) 
    return RET_BUG;

  if ( get_clp_options(stack, Options, &options) == FAIL )
    return RET_BUG;
  
  if ( strcmp(sense_str,"min") == 0 )
    sense = 0;
  else if ( strcmp(sense_str,"max") == 0 )
    sense = 1;
  else
    {
      Scierror("Error: sense should be 'min' or 'max'\n");
      return RET_BUG;
    }
  
  if ( IsMat(ObjA) &&  IsMat(ObjAe))
    {
      NspMatrix *Ae;
      if ( ((NspMatrix *) ObjA)->m != Rhs->mn || ((NspMatrix *) ObjAe)->m != Rhse->mn )
	{
	  Scierror("Error: incompatible dimensions between matrices and Rhs\n",NspFname(stack));
	  return RET_BUG;
	}
      neq = ((NspMatrix *) ObjAe)->m;
      Ae = (neq == 0) ? NULL : (NspMatrix *) ObjAe;
      if ( nsp_matrix_to_sparse_triplet((NspMatrix *)ObjA,Ae, &Cmatbeg,&Cmatind,&Cmatval) == FAIL)
	return RET_BUG;
    }
  else if ( IsSpColMat(ObjA) && IsSpColMat(ObjAe) )
    {
      NspSpColMatrix *Ae;
      if ( ((NspSpColMatrix *) ObjA)->m != Rhs->mn || ((NspSpColMatrix *) ObjAe)->m != Rhse->mn )
	{
	  Scierror("Error: incompatible dimensions between matrices and Rhs\n",NspFname(stack));
	  return RET_BUG;
	}
      neq = ((NspSpColMatrix *) ObjAe)->m;
      Ae = (neq == 0) ? NULL : (NspSpColMatrix *) ObjAe;
      if ( nsp_spcolmatrix_to_sparse_triplet((NspSpColMatrix *)ObjA,Ae, &Cmatbeg,&Cmatind,&Cmatval) == FAIL)
	return RET_BUG;
    }
  else
    {
      Scierror("Error: first and second argument of function %s should be a real full or sparse matrix\n",NspFname(stack));
      return RET_BUG;
    }

  ncols = Objective->mn; /* Length of c == number of columns*/
  nrows = Rhs->mn + Rhse->mn ; /* length of b == number of rows*/

  
  if ( var_type != NULL )
    {
      if ( var_type->mn != ncols ) 
	{
	  Scierror("Error: var_type should be of size %d\n", ncols ); 
	  return RET_BUG;
 	}
    }
  
  /* Check that ObjA and ObjAe are compatible with Rhs and Rhse */

  /* agregates the Rhs */
  if ( ( B = nsp_matrix_create(NVOID,'r',1,nrows)) == NULLMAT) return RET_BUG;
  memcpy(B->R,Rhse->R,Rhse->mn*sizeof(double));
  memcpy(B->R+Rhse->mn,Rhs->R,Rhs->mn*sizeof(double));
  
  if ( lb == NULL ) 
    {
      /* Create lower bounds if not available with value 0 */
      if (( lb =  nsp_matrix_create(NVOID,'r',1,ncols)) == NULL)
	return RET_BUG;
      for (i = 0; i < ncols; i++){
	lb->R[i] = 0; /* to fit with glpk - nsp_coin_dbl_max(); */
      }
    }
  else
    {
      if ( lb->mn != ncols) 
	{
	  Scierror("Error: lb should be of size %d\n",ncols);
	  return RET_BUG;
	}
      for (i = 0; i < lb->mn ; i++)
	{
	  if ( isinf(lb->R[i]) != 0 ) lb->R[i] = - coin_dbl_max;
	}
    }
  
  if ( ub == NULL ) 
    {
      if (( ub =  nsp_matrix_create(NVOID,'r',1,ncols)) == NULL)
	return RET_BUG;
      for (i = 0; i < ncols; i++)
	{
	  ub->R[i] = coin_dbl_max;
	}        
    }
  else
    {
      if ( ub->mn != ncols) 
	{
	  Scierror("Error: ub should be of size %d\n",ncols);
	  return RET_BUG;
	}
      for (i = 0; i < ub->mn; i++)
	{
	  if ( isinf(ub->R[i]) != 0 ) ub->R[i] = coin_dbl_max;
	}
    }
  
  if (( Lhs =  nsp_matrix_create(NVOID,'r',nrows,1)) == NULL)
    return RET_BUG;
  
  for (i = 0; i < neq; i++)
    {
      Lhs->R[i] = B->R[i];
    }
  for (i = neq; i < nrows; i++)
    {
      Lhs->R[i] = -coin_dbl_max;
    }
  
  /* do we have a quadratic cost */
  
  if ( ObjQ != NULL )
    {
      if ( IsMat(ObjQ) ) 
	{
	  if ( ((NspMatrix *) ObjQ)->m != ncols && ((NspMatrix *) ObjQ)->n != ncols )
	    {
	      Scierror("Error: optional argument Q of function %s should be of size %dx%d\n",NspFname(stack),ncols,ncols);
	      return RET_BUG;
	    }
	  if ( nsp_matrix_to_sparse_triplet((NspMatrix *)ObjQ,NULL, &Qmatbeg,&Qmatind,&Qmatval) == FAIL)
	    return RET_BUG;
	}
      else if ( IsSpColMat(ObjQ) ) 
	{
	  if ( ((NspSpColMatrix *) ObjQ)->m != ncols && ((NspSpColMatrix *) ObjQ)->n != ncols )
	    {
	      Scierror("Error: optional argument Q of function %s should be of size %dx%d\n",NspFname(stack),ncols,ncols);
	      return RET_BUG;
	    }
	  if ( nsp_spcolmatrix_to_sparse_triplet((NspSpColMatrix *)ObjQ,NULL, &Qmatbeg,&Qmatind,&Qmatval) == FAIL)
	    return RET_BUG;
	}
      else
	{
	  Scierror("Error: optional argument Q of function %s should be a real full or sparse matrix\n",NspFname(stack));
	  return RET_BUG;
	}
    }
  
  if (( X= nsp_matrix_create(NVOID,'r', ncols,1)) == NULL ) 
    return RET_BUG;
  if (( Lambda= nsp_matrix_create(NVOID,'r', nrows,1)) == NULL ) 
    return RET_BUG;
  if (( Retcode= nsp_matrix_create(NVOID,'r', 1,1)) == NULL ) 
    return RET_BUG;
  if (( RetCost= nsp_matrix_create(NVOID,'r', 1,1)) == NULL ) 
    return RET_BUG;
  
  nsp_clp_solve(&options,sense, ncols,nrows,neq,
		Cmatbeg,Cmatind, Cmatval, lb,ub, Objective,
		Qmatbeg,Qmatind, Qmatval,
		B, Lhs, (var_type == NULL) ? NULL: var_type->S, X, Lambda,RetCost, Retcode);

  /* destroy allocated */

  if ( Cmatbeg !=NULL) nsp_imatrix_destroy(Cmatbeg);
  if ( Cmatind !=NULL) nsp_imatrix_destroy(Cmatind);
  if ( Cmatval !=NULL) nsp_matrix_destroy(Cmatval);
  if ( Qmatbeg !=NULL) nsp_imatrix_destroy(Qmatbeg);
  if ( Qmatind !=NULL) nsp_imatrix_destroy(Qmatind);
  if ( Qmatval !=NULL) nsp_matrix_destroy(Qmatval);

  nsp_matrix_destroy(B);

  /* XXXX */

  MoveObj(stack,1,NSP_OBJECT(X));
  MoveObj(stack,2,NSP_OBJECT(RetCost));
  MoveObj(stack,3,NSP_OBJECT(Retcode));
  MoveObj(stack,4,NSP_OBJECT(Lambda)); 

  return 4;
}

/* Using CoinMP interface to call clp or cbc */

int int_coinmp_solve(Stack stack, int rhs, int opt, int lhs)
{
  const double coin_dbl_max= nsp_coin_dbl_max();
  char *sense_str = "min";
  NspMatrix *X,*Lambda, *Retcode, *RetCost;
  NspIMatrix *Cmatbeg=NULL,*Cmatind=NULL,*Cmatcount=NULL;
  NspMatrix *Cmatval=NULL;
  NspObject *ObjA,*ObjAe;
  NspMatrix *Objective, *Rhs, *Rhse, *B, *Lhs, *lb=NULL, *ub=NULL;
  NspMatrix *SemiCont = NULL;
  NspHash *Options = NULLHASH;
  NspSMatrix *var_type = NULLSMAT;
  int neq, ncols,nrows,i, sense=0;
  nsp_string columnType= NULL, rowType= NULL;

  /* Copy Rhs since Rhs will be concatenated with Rhse */
  int_types T[] = {realmat, obj , realmat, obj , realmat, new_opts, t_end} ;
  
  nsp_option opts[] ={
    {"lb",realmatcopy,NULLOBJ,-1},
    {"ub",realmatcopy,NULLOBJ,-1},
    {"sense",string,NULLOBJ,-1},
    {"var_type",smat,NULLOBJ,-1},
    {"options", hash, NULLOBJ,-1},
    {"semi_cont",realmatcopy,NULLOBJ,-1},
    { NULL,t_end,NULLOBJ,-1}};
  
  if ( GetArgs(stack,rhs,opt,T,&Objective, &ObjA, &Rhs, &ObjAe, &Rhse, &opts,
	       &lb,&ub,&sense_str,&var_type,&Options,&SemiCont) == FAIL) 
    return RET_BUG;
  /* 
  if ( get_solver_options(stack, Options, &options) == FAIL )
    return RET_BUG;
  */

  if ( strcmp(sense_str,"min") == 0 )
    sense = 0;
  else if ( strcmp(sense_str,"max") == 0 )
    sense = 1;
  else
    {
      Scierror("Error: sense should be 'min' or 'max'\n");
      return RET_BUG;
    }
  
  if ( IsMat(ObjA) &&  IsMat(ObjAe))
    {
      NspMatrix *Ae;
      if ( ((NspMatrix *) ObjA)->m != Rhs->mn || ((NspMatrix *) ObjAe)->m != Rhse->mn )
	{
	  Scierror("Error: incompatible dimensions between matrices and Rhs\n",NspFname(stack));
	  return RET_BUG;
	}
      neq = ((NspMatrix *) ObjAe)->m;
      Ae = (neq == 0) ? NULL : (NspMatrix *) ObjAe;
      if ( nsp_matrix_to_sparse_triplet((NspMatrix *)ObjA,Ae, &Cmatbeg,&Cmatind,&Cmatval) == FAIL)
	return RET_BUG;
    }
  else if ( IsSpColMat(ObjA) && IsSpColMat(ObjAe) )
    {
      NspSpColMatrix *Ae;
      if ( ((NspSpColMatrix *) ObjA)->m != Rhs->mn || ((NspSpColMatrix *) ObjAe)->m != Rhse->mn )
	{
	  Scierror("Error: incompatible dimensions between matrices and Rhs\n",NspFname(stack));
	  return RET_BUG;
	}
      neq = ((NspSpColMatrix *) ObjAe)->m;
      Ae = (neq == 0) ? NULL : (NspSpColMatrix *) ObjAe;
      if ( nsp_spcolmatrix_to_sparse_triplet((NspSpColMatrix *)ObjA,Ae, &Cmatbeg,&Cmatind,&Cmatval) == FAIL)
	return RET_BUG;
    }
  else
    {
      Scierror("Error: first and second argument of function %s should be a real full or sparse matrix\n",NspFname(stack));
      return RET_BUG;
    }

  ncols = Objective->mn; /* Length of c == number of columns*/
  nrows = Rhs->mn + Rhse->mn ; /* length of b == number of rows*/

  if ( SemiCont != NULL) 
    {
      int *Sc = (int *) SemiCont->R;
      for ( i= 0 ; i < SemiCont->mn ; i++)
	{
	  if ( SemiCont->R[i] < 1 || SemiCont->R[i] > ncols) 
	    {
	      Scierror("Error: semi-cont index %d is not in the range [1,%d]\n",i,ncols);
	      return RET_BUG;
	    }
	}
      for ( i= 0 ; i < SemiCont->mn ; i++) Sc[i]= SemiCont->R[i] -1 ;
    }
  
  /* extra matrix requested by coinmp 
   * which counts number of non-null elements in each column 
   */

  if ( ( Cmatcount = nsp_imatrix_create(NVOID,1,ncols,nsp_gint32)) == NULLIMAT )
    {
      Scierror("Error: running out of memory\n");
      return RET_BUG;
    }

  for (i = 0; i < ncols ; i++) 
    {
      Cmatcount->Gint[i] = Cmatbeg->Gint[i+1] - Cmatbeg->Gint[i];
    }

  if ( var_type != NULL )
    {
      if ( var_type->mn != ncols ) 
	{
	  Scierror("Error: var_type should be of size %d\n", ncols ); 
	  return RET_BUG;
 	}
      if (( columnType =new_nsp_string_n(ncols+1)) == (nsp_string) 0)
	{
	  Scierror("Error: running out of memory\n");
	  return RET_BUG;
	}
      for (i = 0 ; i < ncols ; i++)
	{
	  if ( strlen(var_type->S[i]) > 0 ) 
	    columnType[i]= var_type->S[i][0];
	  else
	    columnType[i]= 'C';
	}
      columnType[ncols]='\0';
    }

  /* rowType should be of size rowcount and should contain
   * 'L', 'E', 'G', 'R', 'N' 
   * L: (Ax)_i <= b_i 
   * G: (Ax)_i >= b_i
   * E: (Ax)_i == b_i  
   * R: b_i -Abs(range_i) <= (Ax)_i <= b_i 
   * N: no constraint 
   */

  if (( rowType =new_nsp_string_n(nrows+1)) == (nsp_string) 0)
    {
      Scierror("Error: running out of memory\n");
      return RET_BUG;
    }
  for (i = 0 ; i < nrows ; i++)
    {
      if ( i < Rhse->mn) 
	rowType[i]= 'E';
      else
	rowType[i]= 'L';
    }
  rowType[nrows]='\0';

  /* Check that ObjA and ObjAe are compatible with Rhs and Rhse */

  /* agregates the Rhs */
  if ( ( B = nsp_matrix_create(NVOID,'r',1,nrows)) == NULLMAT) return RET_BUG;
  memcpy(B->R,Rhse->R,Rhse->mn*sizeof(double));
  memcpy(B->R+Rhse->mn,Rhs->R,Rhs->mn*sizeof(double));
  
  /* Create lower bounds if not available */
  
  if ( lb == NULL ) 
    {
      if (( lb =  nsp_matrix_create(NVOID,'r',1,ncols)) == NULL)
	return RET_BUG;
      for (i = 0; i < ncols; i++){
	lb->R[i] = 0; /* to fit with glpk - nsp_coin_dbl_max(); */
      }
    }
  else
    {
      if ( lb->mn != ncols) 
	{
	  Scierror("Error: lb should be of size %d\n",ncols);
	  return RET_BUG;
	}
      for (i = 0; i < lb->mn ; i++)
	{
	  if ( isinf(lb->R[i]) != 0 ) lb->R[i] = - coin_dbl_max; 
	}
    }
  
  if ( ub == NULL ) 
    {
      if (( ub =  nsp_matrix_create(NVOID,'r',1,ncols)) == NULL)
	return RET_BUG;
      for (i = 0; i < ncols; i++)
	{
	  ub->R[i] = coin_dbl_max;
	}        
    }
  else
    {
      if ( ub->mn != ncols) 
	{
	  Scierror("Error: ub should be of size %d\n",ncols);
	  return RET_BUG;
	}
      for (i = 0; i < ub->mn; i++)
	{
	  if ( isinf(ub->R[i]) != 0 ) ub->R[i] = coin_dbl_max;
	}
    }
    
  if (( Lhs =  nsp_matrix_create(NVOID,'r',nrows,1)) == NULL)
    return RET_BUG;
  
  for (i = 0; i < neq; i++)
    {
      Lhs->R[i] = B->R[i];
    }
  for (i = neq; i < nrows; i++)
    {
      Lhs->R[i] = -coin_dbl_max;
    }

    
  if (( X= nsp_matrix_create(NVOID,'r', ncols,1)) == NULL ) 
    return RET_BUG;
  if (( Lambda= nsp_matrix_create(NVOID,'r', nrows,1)) == NULL ) 
    return RET_BUG;
  if (( Retcode= nsp_matrix_create(NVOID,'r', 1,1)) == NULL ) 
    return RET_BUG;
  if (( RetCost= nsp_matrix_create(NVOID,'r', 1,1)) == NULL ) 
    return RET_BUG;

  nsp_coinmp_solve("Pb", sense, ncols, nrows,Cmatbeg,Cmatcount,Cmatind, Cmatval,
		   lb,ub,Objective, B, columnType,  X, Lambda,RetCost, Retcode,rowType, 
		   (SemiCont != NULL) ? SemiCont->mn : 0, 
		   (SemiCont != NULL) ? (int *) SemiCont->R: NULL, Options);

  /* destroy allocated */

  if ( Cmatbeg !=NULL) nsp_imatrix_destroy(Cmatbeg);
  if ( Cmatind !=NULL) nsp_imatrix_destroy(Cmatind);
  if ( Cmatval !=NULL) nsp_matrix_destroy(Cmatval);
  nsp_matrix_destroy(B);
  if ( columnType != NULL) nsp_string_destroy(&columnType);
  if ( rowType != NULL) nsp_string_destroy(&columnType);

  MoveObj(stack,1,NSP_OBJECT(X));
  MoveObj(stack,2,NSP_OBJECT(RetCost));
  MoveObj(stack,3,NSP_OBJECT(Retcode));
  MoveObj(stack,4,NSP_OBJECT(Lambda)); 

  return 4;
}

int int_coinmp_options(Stack stack, int rhs, int opt, int lhs)
{
  NspHash *H;
  CheckStdRhs(0,0);
  CheckLhs(0,1);
  H = nsp_coinmp_get_options();
  if ( H == NULL) 
    {
      Scierror("Error: failed to get coimp options\n");
      return RET_BUG;
    }
  MoveObj(stack,1,NSP_OBJECT(H));
  return 1;
}

static int get_clp_options(Stack stack, NspHash *solver_options, nsp_clp_params *clp_params)
{
  int solverchoice = 1, maxnumiterations = 99999999,loglevel = 0,primalpivot = 1,dualpivot = 1;
  double maxnumseconds = 3600.0,primaltolerance = 1e-7,dualtolerance = 1e-7;
  nsp_option opts[] ={
    { "solverchoice"   , s_int ,NULLOBJ,-1},
    { "maxnumiterations", s_int ,NULLOBJ,-1},
    { "loglevel" , s_int ,NULLOBJ,-1},
    { "primalpivot", s_int, NULLOBJ,-1},
    { "dualpivot",s_int,NULLOBJ,-1},
    { "maxnumseconds",s_double,NULLOBJ,-1},
    { "primaltolerance", s_double,NULLOBJ,-1},
    { "dualtolerance", s_double,NULLOBJ,-1},
    { NULL,      t_end ,NULLOBJ,-1}
  };
  if ( solver_options != NULL) 
    {
      if ( get_optional_args_from_hash(stack,solver_options, opts,
				       &solverchoice ,
				       &maxnumiterations ,
				       &loglevel ,
				       &primalpivot ,
				       &dualpivot ,
				       &maxnumseconds ,
				       &primaltolerance ,
				       &dualtolerance ) == FAIL )
	return FAIL;
    }
  clp_params->solverchoice = Min(Max(solverchoice,1),3);
  clp_params->maxnumiterations = Max(maxnumiterations,1);
  clp_params->loglevel = loglevel ;
  clp_params->primalpivot = primalpivot ;
  clp_params->dualpivot = dualpivot ;
  clp_params->maxnumseconds = maxnumseconds ;
  clp_params->primaltolerance = primaltolerance ;
  clp_params->dualtolerance = dualtolerance ;
  return OK;
}


/* very similar to the previous one + Q */

extern double nsp_cplex_dbl_max();

int int_cplex_solve(Stack stack, int rhs, int opt, int lhs)
{
  int rep;
  int loglevel = 0;
  const double coin_dbl_max= nsp_cplex_dbl_max();
  char *sense_str = "min";
  NspMatrix *X,*Lambda, *Retcode, *RetCost;
  NspIMatrix *Cmatbeg=NULL,*Cmatind=NULL,*Cmatcount=NULL;
  NspMatrix *Cmatval=NULL;
  NspObject *ObjA,*ObjAe;
  NspMatrix *Objective, *Rhs, *Rhse, *B, *Lhs, *lb=NULL, *ub=NULL;
  NspMatrix *SemiCont = NULL;

  NspIMatrix *Qmatbeg=NULL,*Qmatcnt=NULL,*Qmatind=NULL;
  NspMatrix *Qmatval=NULL;
  NspObject *ObjQ=NULL;

  NspHash *Options = NULLHASH;
  NspSMatrix *var_type = NULLSMAT;
  int neq, ncols,nrows,i, sense=0;
  nsp_string columnType= NULL, rowType= NULL;

  int_types T[] = {realmat, obj , realmat, obj , realmat, new_opts, t_end} ;
  
  nsp_option opts[] ={
    {"Q", obj,NULLOBJ,-1},
    {"lb",realmatcopy,NULLOBJ,-1},
    {"ub",realmatcopy,NULLOBJ,-1},
    {"sense",string,NULLOBJ,-1},
    {"var_type",smat,NULLOBJ,-1},
    {"options", hash, NULLOBJ,-1},
    {"semi_cont",realmatcopy,NULLOBJ,-1},
    {"loglevel", s_int,NULLOBJ, -1},
    { NULL,t_end,NULLOBJ,-1}};
  
  if ( GetArgs(stack,rhs,opt,T,&Objective, &ObjA, &Rhs, &ObjAe, &Rhse, &opts,&ObjQ,
	       &lb,&ub,&sense_str,&var_type,&Options,&SemiCont,&loglevel) == FAIL) 
    return RET_BUG;
  /* 
  if ( get_solver_options(stack, Options, &options) == FAIL )
    return RET_BUG;
  */

  if ( strcmp(sense_str,"min") == 0 )
    sense = 0;
  else if ( strcmp(sense_str,"max") == 0 )
    sense = 1;
  else
    {
      Scierror("Error: sense should be 'min' or 'max'\n");
      return RET_BUG;
    }
  
  if ( IsMat(ObjA) &&  IsMat(ObjAe))
    {
      NspMatrix *Ae;
      if ( ((NspMatrix *) ObjA)->m != Rhs->mn || ((NspMatrix *) ObjAe)->m != Rhse->mn )
	{
	  Scierror("Error: incompatible dimensions between matrices and Rhs\n",NspFname(stack));
	  return RET_BUG;
	}
      neq = ((NspMatrix *) ObjAe)->m;
      Ae = (neq == 0) ? NULL : (NspMatrix *) ObjAe;
      if ( nsp_matrix_to_sparse_triplet((NspMatrix *)ObjA,Ae, &Cmatbeg,&Cmatind,&Cmatval) == FAIL)
	return RET_BUG;
    }
  else if ( IsSpColMat(ObjA) && IsSpColMat(ObjAe) )
    {
      NspSpColMatrix *Ae;
      if ( ((NspSpColMatrix *) ObjA)->m != Rhs->mn || ((NspSpColMatrix *) ObjAe)->m != Rhse->mn )
	{
	  Scierror("Error: incompatible dimensions between matrices and Rhs\n",NspFname(stack));
	  return RET_BUG;
	}
      neq = ((NspSpColMatrix *) ObjAe)->m;
      Ae = (neq == 0) ? NULL : (NspSpColMatrix *) ObjAe;
      if ( nsp_spcolmatrix_to_sparse_triplet((NspSpColMatrix *)ObjA,Ae, &Cmatbeg,&Cmatind,&Cmatval) == FAIL)
	return RET_BUG;
    }
  else
    {
      Scierror("Error: first and second argument of function %s should be a real full or sparse matrix\n",
	       NspFname(stack));
      return RET_BUG;
    }

  ncols = Objective->mn; /* Length of c == number of columns*/
  nrows = Rhs->mn + Rhse->mn ; /* length of b == number of rows*/

  if ( SemiCont != NULL) 
    {
      Scierror("Error: not yet implemented in cplex interface \n");
      return RET_BUG;
      int *Sc = (int *) SemiCont->R;
      for ( i= 0 ; i < SemiCont->mn ; i++)
	{
	  if ( SemiCont->R[i] < 1 || SemiCont->R[i] > ncols) 
	    {
	      Scierror("Error: semi-cont index %d is not in the range [1,%d]\n",i,ncols);
	      return RET_BUG;
	    }
	}
      for ( i= 0 ; i < SemiCont->mn ; i++) Sc[i]= SemiCont->R[i] -1 ;
    }
  
  /* extra matrix requested by coinmp 
   * which counts number of non-null elements in each column 
   */

  if ( ( Cmatcount = nsp_imatrix_create(NVOID,1,ncols,nsp_gint32)) == NULLIMAT )
    {
      Scierror("Error: running out of memory\n");
      return RET_BUG;
    }

  for (i = 0; i < ncols ; i++) 
    {
      Cmatcount->Gint[i] = Cmatbeg->Gint[i+1] - Cmatbeg->Gint[i];
    }

  if ( var_type != NULL )
    {
      if ( var_type->mn != ncols ) 
	{
	  Scierror("Error: var_type should be of size %d\n", ncols ); 
	  return RET_BUG;
 	}
      if (( columnType =new_nsp_string_n(ncols+1)) == (nsp_string) 0)
	{
	  Scierror("Error: running out of memory\n");
	  return RET_BUG;
	}
      for (i = 0 ; i < ncols ; i++)
	{
	  if ( strlen(var_type->S[i]) > 0 ) 
	    columnType[i]= var_type->S[i][0];
	  else
	    columnType[i]= 'C';
	}
      columnType[ncols]='\0';
    }

  /* rowType should be of size rowcount and should contain
   * 'L', 'E', 'G', 'R', 'N' 
   * L: (Ax)_i <= b_i 
   * G: (Ax)_i >= b_i
   * E: (Ax)_i == b_i  
   * R: b_i -Abs(range_i) <= (Ax)_i <= b_i 
   * N: no constraint 
   */

  if (( rowType =new_nsp_string_n(nrows+1)) == (nsp_string) 0)
    {
      Scierror("Error: running out of memory\n");
      return RET_BUG;
    }
  for (i = 0 ; i < nrows ; i++)
    {
      if ( i < Rhse->mn) 
	rowType[i]= 'E';
      else
	rowType[i]= 'L';
    }
  rowType[nrows]='\0';

  /* Check that ObjA and ObjAe are compatible with Rhs and Rhse */

  /* agregates the Rhs */
  if ( ( B = nsp_matrix_create(NVOID,'r',1,nrows)) == NULLMAT) return RET_BUG;
  memcpy(B->R,Rhse->R,Rhse->mn*sizeof(double));
  memcpy(B->R+Rhse->mn,Rhs->R,Rhs->mn*sizeof(double));
  
  /* Create lower bounds if not available */
  
  if ( lb == NULL ) 
    {
      if (( lb =  nsp_matrix_create(NVOID,'r',1,ncols)) == NULL)
	return RET_BUG;
      for (i = 0; i < ncols; i++){
	lb->R[i] = 0; /* to fit with glpk - nsp_coin_dbl_max(); */
      }
    }
  else
    {
      if ( lb->mn != ncols) 
	{
	  Scierror("Error: lb should be of size %d\n",ncols);
	  return RET_BUG;
	}
      for (i = 0; i < lb->mn ; i++)
	{
	  if ( isinf(lb->R[i]) != 0 ) lb->R[i] = - coin_dbl_max; 
	}
    }
  
  if ( ub == NULL ) 
    {
      if (( ub =  nsp_matrix_create(NVOID,'r',1,ncols)) == NULL)
	return RET_BUG;
      for (i = 0; i < ncols; i++){
	ub->R[i] = coin_dbl_max;
     }        
    }
  else
    {
      if ( ub->mn != ncols) 
	{
	  Scierror("Error: ub should be of size %d\n",ncols);
	  return RET_BUG;
	}
      for (i = 0; i < ub->mn; i++)
	{
	  if ( isinf(ub->R[i]) != 0 ) ub->R[i] = coin_dbl_max;
	}
    }

  if (( Lhs =  nsp_matrix_create(NVOID,'r',nrows,1)) == NULL)
    return RET_BUG;
  
  for (i = 0; i < neq; i++)
    {
      Lhs->R[i] = B->R[i];
    }
  for (i = neq; i < nrows; i++)
    {
      Lhs->R[i] = -coin_dbl_max;
    }

  /* do we have a quadratic cost */
  
  if ( ObjQ != NULL )
    {
      if ( IsMat(ObjQ) ) 
	{
	  if ( ((NspMatrix *) ObjQ)->m != ncols && ((NspMatrix *) ObjQ)->n != ncols )
	    {
	      Scierror("Error: optional argument Q of function %s should be of size %dx%d\n",
		       NspFname(stack),ncols,ncols);
	      return RET_BUG;
	    }
	  if ( nsp_matrix_to_sparse_triplet((NspMatrix *)ObjQ,NULL, &Qmatbeg,&Qmatind,&Qmatval) == FAIL)
	    return RET_BUG;
	}
      else if ( IsSpColMat(ObjQ) ) 
	{
	  if ( ((NspSpColMatrix *) ObjQ)->m != ncols && ((NspSpColMatrix *) ObjQ)->n != ncols )
	    {
	      Scierror("Error: optional argument Q of function %s should be of size %dx%d\n",
		       NspFname(stack),ncols,ncols);
	      return RET_BUG;
	    }
	  if ( nsp_spcolmatrix_to_sparse_triplet((NspSpColMatrix *)ObjQ,NULL, &Qmatbeg,&Qmatind,&Qmatval) == FAIL)
	    return RET_BUG;
	}
      else
	{
	  Scierror("Error: optional argument Q of function %s should be a real full or sparse matrix\n",
		   NspFname(stack));
	  return RET_BUG;
	}

      /* extra matrix */
      if ( ( Qmatcnt = nsp_imatrix_create(NVOID,1,ncols,nsp_gint32)) == NULLIMAT )
	{
	  Scierror("Error: running out of memory\n");
	  return RET_BUG;
	}
      for (i = 0; i < ncols ; i++) 
	{
	  Qmatcnt->Gint[i] = Qmatbeg->Gint[i+1] - Qmatbeg->Gint[i];
	}
    }
  
  if (( X= nsp_matrix_create(NVOID,'r', ncols,1)) == NULL ) 
    return RET_BUG;
  if (( Lambda= nsp_matrix_create(NVOID,'r', nrows,1)) == NULL ) 
    return RET_BUG;
  if (( Retcode= nsp_matrix_create(NVOID,'r', 1,1)) == NULL ) 
    return RET_BUG;
  if (( RetCost= nsp_matrix_create(NVOID,'r', 1,1)) == NULL ) 
    return RET_BUG;

  rep = nsp_cplex_solve("Pb", sense, ncols, nrows,Cmatbeg,Cmatcount,Cmatind, Cmatval,
			lb,ub,Objective, Qmatbeg, Qmatcnt, Qmatind, Qmatval,
			B, columnType,  X, Lambda,RetCost, Retcode,rowType, 
			(SemiCont != NULL) ? SemiCont->mn : 0, 
			(SemiCont != NULL) ? (int *) SemiCont->R: NULL, Options,loglevel);

  /* destroy allocated */

  if ( Cmatbeg !=NULL) nsp_imatrix_destroy(Cmatbeg);
  if ( Cmatind !=NULL) nsp_imatrix_destroy(Cmatind);
  if ( Cmatval !=NULL) nsp_matrix_destroy(Cmatval);

  if ( Qmatbeg !=NULL) nsp_imatrix_destroy(Qmatbeg);
  if ( Qmatcnt !=NULL) nsp_imatrix_destroy(Qmatcnt);
  if ( Qmatind !=NULL) nsp_imatrix_destroy(Qmatind);
  if ( Qmatval !=NULL) nsp_matrix_destroy(Qmatval);

  nsp_matrix_destroy(B);
  if ( columnType != NULL) nsp_string_destroy(&columnType);
  if ( rowType != NULL) nsp_string_destroy(&columnType);

  if ( rep == FAIL ) 
    {
      nsp_matrix_destroy(X);
      nsp_matrix_destroy(RetCost);
      nsp_matrix_destroy(Retcode);
      nsp_matrix_destroy(Lambda);
      return RET_BUG;
    }
  else
    {
      MoveObj(stack,1,NSP_OBJECT(X));
      if ( lhs >= 2) 
	MoveObj(stack,2,NSP_OBJECT(RetCost));
      else
	nsp_matrix_destroy(RetCost);
      if ( lhs >= 3)
	MoveObj(stack,3,NSP_OBJECT(Retcode));
      else
	nsp_matrix_destroy(Retcode);
      if ( lhs >= 4)
	MoveObj(stack,4,NSP_OBJECT(Lambda)); 
      else
	nsp_matrix_destroy(Lambda);
    }
  return Max(lhs,1);
}



/* utility to test clpsparse */

int int_clp_sparse(Stack stack, int rhs, int opt, int lhs)
{
  NspIMatrix *Matbeg,*Matind;
  NspMatrix *Matval;
  NspObject *Obj;
  int_types T[] = {obj, t_end} ;
  CheckRhs(1,1);
  CheckLhs(1,3);
  if ( GetArgs(stack,rhs,opt,T,&Obj) == FAIL) return RET_BUG;
  if ( IsMat(Obj) )
    {
      if ( nsp_matrix_to_sparse_triplet((NspMatrix *)Obj,NULL, &Matbeg,&Matind,&Matval) == FAIL)
	return RET_BUG;
    }
  else if ( IsSpColMat(Obj) )
    {
      if ( nsp_spcolmatrix_to_sparse_triplet((NspSpColMatrix *)Obj,NULL, &Matbeg,&Matind,&Matval) == FAIL)
	return RET_BUG;
    }
  else
    {
      Scierror("Error: second argument of function %s should be a real full or sparse matrix\n",NspFname(stack));
      return RET_BUG;
    }
  MoveObj(stack,1,NSP_OBJECT(Matbeg));
  if ( lhs >= 2 ) MoveObj(stack,2,NSP_OBJECT(Matind));
  else nsp_imatrix_destroy(Matind);
  if ( lhs >= 3 ) MoveObj(stack,3,NSP_OBJECT(Matval));
  else  nsp_matrix_destroy(Matval);
  return 3;
}

int int_clp_sparse2(Stack stack, int rhs, int opt, int lhs)
{
  NspIMatrix *Matbeg,*Matind;
  NspMatrix *Matval;
  NspObject *Obj1,*Obj2;
  int_types T[] = {obj, obj, t_end} ;
  CheckRhs(2,2);
  CheckLhs(1,3);
  if ( GetArgs(stack,rhs,opt,T,&Obj1,&Obj2) == FAIL) return RET_BUG;
  if ( IsMat(Obj1) && IsMat(Obj2) )
    {
      if ( nsp_matrix_to_sparse_triplet((NspMatrix *)Obj1,(NspMatrix *)Obj2,
					&Matbeg,&Matind,&Matval) == FAIL)
	return RET_BUG;
    }
  else if ( IsSpColMat(Obj1) &&  IsSpColMat(Obj2) )
    {
      if ( nsp_spcolmatrix_to_sparse_triplet((NspSpColMatrix *)Obj1,(NspSpColMatrix *)Obj2,
					     &Matbeg,&Matind,&Matval) == FAIL)
	return RET_BUG;
    }
  else
    {
      Scierror("Error: arguments of function %s should be a real full or sparse matrix\n",NspFname(stack));
      return RET_BUG;
    }
  MoveObj(stack,1,NSP_OBJECT(Matbeg));
  if ( lhs >= 2 ) MoveObj(stack,2,NSP_OBJECT(Matind));
  else nsp_imatrix_destroy(Matind);
  if ( lhs >= 3 ) MoveObj(stack,3,NSP_OBJECT(Matval));
  else  nsp_matrix_destroy(Matval);
  return 3;
}



/* 
 * Clp matrix utilities 
 * returns a sparse description of matrix [A2;A1] 
 * the sparse coding scheme is similar to the one returned by spget_mtlb(A)
 * the matrix A2 can be equal to NULL;
 */

static int nsp_matrix_to_sparse_triplet(NspMatrix *A1, NspMatrix *A2, NspIMatrix **Matbeg,NspIMatrix **Matind, NspMatrix **Matval)
{
  NspIMatrix *MatBeg=NULL,*MatInd=NULL;
  NspMatrix *MatVal=NULL;
  int i,j,nz,count;
  /* Convert to clp sparse code */
  if (A2 != NULL && A1->n != A2->n )
    {
      Scierror("Error: A1 and A2 should have same number of columns\n");
      return FAIL;
    }
  
  if ( ( MatBeg = nsp_imatrix_create(NVOID,1,A1->n+1,nsp_gint32)) == NULLIMAT )
    goto end;
  /* count non-null elements in [A1;A2] */
  MatBeg->Gint32[0]=0;
  for ( j=0; j < A1->n ; j++)
    {
      MatBeg->Gint32[j+1]=0;
      if ( A2 != NULL ) 
	{
	  for ( i=0; i < A2->m ; i++)
	    {
	      if ( A2->R[i+A2->m*j] != 0.0) MatBeg->Gint32[j+1]++;
	    }
	}
      for ( i=0; i < A1->m ; i++)
	{
	  if ( A1->R[i+A1->m*j] != 0.0) MatBeg->Gint32[j+1]++;
	}
    }
  /* second step to make the cumsum */
  for ( i=2 ; i < MatBeg->n ; i++)
    {
      MatBeg->Gint32[i] += MatBeg->Gint32[i-1];
    }
  /* row indices of non nul ements */
  nz = MatBeg->Gint32[MatBeg->n-1];
  if ( ( MatInd = nsp_imatrix_create(NVOID,1,nz,nsp_gint32)) == NULLIMAT )
    goto end;
  if ( ( MatVal = nsp_matrix_create(NVOID,'r',1,nz)) == NULLMAT )
    goto end;
  count=0;
  for ( j=0; j < A1->n ; j++)
    {
      int offset =0;
      if (A2 != NULL) 
	{
	  offset = A2->m;
	  for ( i=0; i < A2->m ; i++)
	    {
	      if ( A2->R[i+A2->m*j] != 0.0) 
		{
		  MatInd->Gint32[count]=i; 
		  MatVal->R[count]= A2->R[i+A2->m*j];
		  count++;
		}
	    }
	}
      for ( i=0; i < A1->m ; i++)
	{
	  if ( A1->R[i+A1->m*j] != 0.0) 
	    {
	      MatInd->Gint32[count]=i+offset;
	      MatVal->R[count]= A1->R[i+A1->m*j];
	      count++;
	    }
	}
    }

  *Matbeg = MatBeg;
  *Matind = MatInd;
  *Matval = MatVal;
  return OK;
 end:
  if ( MatBeg !=NULL) nsp_imatrix_destroy(MatBeg);
  if ( MatInd !=NULL) nsp_imatrix_destroy(MatInd);
  if ( MatVal !=NULL) nsp_matrix_destroy(MatVal);
  return FAIL;
}


static int nsp_spcolmatrix_to_sparse_triplet(NspSpColMatrix *A1,NspSpColMatrix *A2, NspIMatrix **Matbeg,NspIMatrix **Matind, NspMatrix **Matval)
{
  NspIMatrix *MatBeg=NULL,*MatInd=NULL;
  NspMatrix *MatVal=NULL;
  int i,j,nz,count;
  /* Convert to clp sparse code 
   * MatBeg: indices of the start of each column in the array of non-null values 
   * columnwize.
   */

  /* Convert to clp sparse code */
  if (A2 != NULL && A1->n != A2->n )
    {
      Scierror("Error: A1 and A2 should have same number of columns\n");
      return FAIL;
    }
  
  if ( ( MatBeg = nsp_imatrix_create(NVOID,1,A1->n+1,nsp_gint32)) == NULLIMAT )
    goto end;
  MatBeg->Gint32[0]=0;
  for ( j=0; j < A1->n ; j++)
    {
      MatBeg->Gint32[j+1] = A1->D[j]->size;
      if (A2 != NULL) 
	MatBeg->Gint32[j+1] += A2->D[j]->size;
    }

  /* second step to make the cumsum */
  for ( i=2 ; i < MatBeg->n ; i++)
    {
      MatBeg->Gint32[i] += MatBeg->Gint32[i-1];
    }
  /* row indices of non nul ements */
  nz = MatBeg->Gint32[MatBeg->n-1];
  if ( ( MatInd = nsp_imatrix_create(NVOID,1,nz,nsp_gint32)) == NULLIMAT )
    goto end;
  if ( ( MatVal = nsp_matrix_create(NVOID,'r',1,nz)) == NULLMAT )
    goto end;
  count=0;
  for ( j=0; j < A1->n ; j++)
    {
      int k, offset = 0;
      SpCol *Col;
      if (A2 != NULL) 
	{
	  offset = A2->m;
	  Col = A2->D[j];
	  for ( k = 0 ; k < Col->size ; k++)
	    {
	      MatInd->Gint32[count]= Col->J[k];
	      MatVal->R[count]= Col->R[k];
	      count++;
	    }
	}
      Col = A1->D[j];
      for ( k = 0 ; k < Col->size ; k++)
	{
	  MatInd->Gint32[count]= Col->J[k]+offset;
	  MatVal->R[count]= Col->R[k];
	  count++;
	}
    }
  *Matbeg = MatBeg;
  *Matind = MatInd;
  *Matval = MatVal;
  return OK;
 end:
  if ( MatBeg !=NULL) nsp_imatrix_destroy(MatBeg);
  if ( MatInd !=NULL) nsp_imatrix_destroy(MatInd);
  if ( MatVal !=NULL) nsp_matrix_destroy(MatVal);
  return FAIL;
}
