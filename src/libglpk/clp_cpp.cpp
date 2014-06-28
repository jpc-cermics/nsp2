#include "clp_cpp.h"
#include "Coin_C_defines.h"
#include "CoinMessageHandler.hpp"
#include "ClpSimplex.hpp"
#include "ClpInterior.hpp"
#include "ClpCholeskyBase.hpp"

class DerivedHandler :
  public CoinMessageHandler {
public:
  virtual int print() ;
};

int DerivedHandler::print()
{
  Sciprintf(messageBuffer());
  Sciprintf("\n");
  return 0;
}


double nsp_coin_dbl_max(void)
{
  return COIN_DBL_MAX; 
}

int nsp_clp_solve(nsp_clp_params *options,int sense, int ncols, int nrows, int neq, 
		  NspIMatrix*Cmatbeg, NspIMatrix *Cmatind, NspMatrix *Cmatval, 
		  NspMatrix *lower, NspMatrix *upper, NspMatrix *Objective,
		  NspIMatrix*Qmatbeg, NspIMatrix *Qmatind, NspMatrix *Qmatval, 
		  NspMatrix *Rhs,  NspMatrix *Lhs,char *var_type[],  NspMatrix *X,NspMatrix *Lambda,
		  NspMatrix *RetCost,NspMatrix *Retcode)
{
  double *primal, *dual;

  ClpSimplex *modelByColumn = NULL;
  ClpInterior *modelPrimalDual = NULL;
  DerivedHandler * mexprinter = NULL;
  
  switch (options->solverchoice)
    {
    default:
      {				
	modelByColumn = new ClpSimplex();
	modelByColumn->loadProblem(ncols,nrows,(const int*)Cmatbeg->Iv,(const int*)Cmatind->Iv,
				   Cmatval->R,lower->R,upper->R,Objective->R,Lhs->R,Rhs->R);
	if ( var_type != NULL) 
	  {
	    int i;
	    for ( i=0; i < ncols ; i++)
	      {
		if ( var_type[i] != NULL &&  var_type[i][0]== 'I') 
		  modelByColumn->setInteger(i);
	      }
	  }
	modelByColumn->setOptimizationDirection((sense==0) ? 1: -1);
	break;
      }
    case 3:
      {						
	modelPrimalDual = new ClpInterior();
	modelPrimalDual->loadProblem(ncols,nrows,(const int*)Cmatbeg->Iv,(const int*)Cmatind->Iv,
				     Cmatval->R,lower->R,upper->R,Objective->R,Lhs->R,Rhs->R);
	if ( var_type != NULL) 
	  {
	    int i;
	    for ( i=0; i < ncols ; i++)
	      {
		if ( var_type[i] != NULL &&  var_type[i][0]== 'I') 
		  modelByColumn->setInteger(i);
	      }
	  }
	modelByColumn->setOptimizationDirection((sense==0) ? 1: -1);
	break;
      }
    }
		
  if ( Qmatbeg != NULL ) 
    {	
      /* do we have a quadratic part  */
      if (options->solverchoice==2)
	{
	  options->solverchoice = 1;
	}
      switch (options->solverchoice)
	{
	default:
	  {							
	    modelByColumn->loadQuadraticObjective(ncols,(const int*) Qmatbeg->Iv,(const int*) Qmatind->Iv,Qmatval->R);
	    modelByColumn->setOptimizationDirection((sense==0) ? 1: -1);
	    break;
	  }	
	case 3:
	  {				
	    modelPrimalDual->loadQuadraticObjective(ncols,(const int*) Qmatbeg->Iv,(const int*) Qmatind->Iv,Qmatval->R);
	    modelByColumn->setOptimizationDirection((sense==0) ? 1: -1);
	  }
	}					
    }	

  /* change handler for printing */
  mexprinter = new DerivedHandler(); 
  mexprinter->setLogLevel(options->loglevel);		 
  
  if (options->solverchoice == 3)
    {		
      modelPrimalDual->setMaximumIterations(options->maxnumiterations);
      modelPrimalDual->setMaximumSeconds(options->maxnumseconds);
      modelPrimalDual->setPrimalTolerance(options->primaltolerance);
      modelPrimalDual->setDualTolerance(options->dualtolerance);	
      modelPrimalDual->passInMessageHandler(mexprinter);		    
    }
  else
    {
      modelByColumn->setMaximumIterations(options->maxnumiterations);
      modelByColumn->setMaximumSeconds(options->maxnumseconds);
      modelByColumn->setPrimalTolerance(options->primaltolerance);
      modelByColumn->setDualTolerance(options->dualtolerance);				
      modelByColumn->passInMessageHandler(mexprinter);	
    }
  
  switch (options->solverchoice)
    {
    default:
    case 1:			
      {	
	modelByColumn->primal();		
	break;
      }
    case 2:			
      {					
	modelByColumn->dual();
	break;
      }
    case 3:
      {			
	ClpCholeskyBase * cholesky = new ClpCholeskyBase();			
	cholesky->setKKT(true);		
	modelPrimalDual->setCholesky(cholesky);	
	if (modelPrimalDual->primalDual())
	  {
	    Sciprintf("Failed\n");
	  }
	break;
      }
    }
	
  if (options->solverchoice==3)
    {	
      primal = modelPrimalDual->primalColumnSolution();
      dual = modelPrimalDual->dualRowSolution();
      RetCost->R[0] = modelPrimalDual->getObjValue(); 
      Retcode->R[0] = modelPrimalDual->status();			
      modelPrimalDual->writeMps("poo.mps");
    }
  else
    {	
      primal = modelByColumn->primalColumnSolution();
      dual = modelByColumn->dualRowSolution();
      RetCost->R[0] =  modelByColumn->getObjValue(); 
      Retcode->R[0] = modelByColumn->status();		
      modelByColumn->writeMps("poo.mps");
    }
  
  
  /* variables */

  if (primal != NULL) 
    {
      memcpy(X->R,primal,   ncols*sizeof(double));
    }
  if (dual   != NULL) 
    { 
      /* change the order since we have here 
       * equality constraints and the inequalities 
       */
      /* first the inequality constraints */
      memcpy(Lambda->R,dual+neq,(nrows-neq)*sizeof(double));
      /* then the equality constraints */
      memcpy(Lambda->R+(nrows-neq),dual,(neq)*sizeof(double));
    }
  
  /* Delete C++ allocated objects */
  if (modelByColumn   != NULL){delete(modelByColumn);}	
  if (modelPrimalDual != NULL){delete(modelPrimalDual);}		
  if (mexprinter      != NULL){delete(mexprinter);}		
  return OK;
}

