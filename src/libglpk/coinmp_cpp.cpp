#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>
#include <math.h>

#include "CoinMP.h"
#include "coinmp_cpp.h"
#include "Coin_C_defines.h"
#include "CoinMessageHandler.hpp"
#include "ClpSimplex.hpp"
#include "ClpInterior.hpp"
#include "ClpCholeskyBase.hpp"

/* inteface for the coinmp interface to linear programming */

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

int SOLVCALL MsgLogCallback(const char* MessageStr, void *UserParam)
{
	//fprintf(stdout, "*** %s", MessageStr);
	fprintf(stdout, "*** MSG: %s, user=%s\n", MessageStr, (char*)UserParam);
	return 0;
}

int SOLVCALL IterCallback(int    IterCount, 
			double ObjectValue,
			int    IsFeasible, 
			double InfeasValue,
			void   *UserParam)
{
	fprintf(stdout, "*** ITER: iter=%d, obj=%.20g, feas=%d, infeas=%.20g, user=%s\n",
		IterCount, ObjectValue, IsFeasible, InfeasValue, (char*)UserParam);
	return 0;
}

int SOLVCALL MipNodeCallback(int    IterCount, 
				int	  MipNodeCount,
				double BestBound,
				double BestInteger,
				int    IsMipImproved,
				void   *UserParam)
{
	fprintf(stdout, "*** NODE: iter=%d, node=%d, bound=%.20g, best=%.20g, %s, user=%s\n",
		IterCount, MipNodeCount, BestBound, BestInteger, IsMipImproved ? "Improved" : "*", (char*)UserParam);
	return 0;
}

int SOLVCALL OldMsgLogCallback(const char* MessageStr)
{
	//fprintf(stdout, "*** %s", MessageStr);
	fprintf(stdout, "*** ");
	return 0;
}

int SOLVCALL OldIterCallback(int    IterCount, 
			double ObjectValue,
			int    IsFeasible, 
			double InfeasValue)
{
	fprintf(stdout, "ITER: iter=%d, obj=%.20g, feas=%d, infeas=%.20g\n",
		IterCount, ObjectValue, IsFeasible, InfeasValue);
	return 0;
}

int SOLVCALL OldMipNodeCallback(int    IterCount, 
				int	  MipNodeCount,
				double BestBound,
				double BestInteger,
				int    IsMipImproved)
{
	fprintf(stdout, "NODE: iter=%d, node=%d, bound=%.20g, best=%.20g, %s\n",
		IterCount, MipNodeCount, BestBound, BestInteger, IsMipImproved ? "Improved" : "");
	return 0;
}

int nsp_coinmp_solve(const char* problemName, nsp_clp_params *options,int sense, int ncols, int nrows, int neq, 
		     NspIMatrix*Cmatbeg, NspIMatrix *Cmatind, NspMatrix *Cmatval, 
		     NspMatrix *lower, NspMatrix *upper, NspMatrix *Objective,
		     NspIMatrix*Qmatbeg, NspIMatrix *Qmatind, NspMatrix *Qmatval, 
		     NspMatrix *Rhs, char *var_type[],  NspMatrix *X,NspMatrix *Lambda,
		     NspMatrix *RetCost,NspMatrix *Retcode)
{
  /* matrix A part */
  int *matrixBegin = (int *) Cmatbeg->Iv;
  int *matrixIndex = (int *) Cmatind->Iv;
  double *matrixValues = Cmatval->R; 
  int colCount = ncols, rowCount = nrows; 
  int nonZeroCount = matrixBegin[ncols]; /* number of non null elements in the matrix */
  int rangeCount = 0; double *rangeValues = NULL;
  /* we need to provide matrixCount which counts number of elements in each column */
  int *matrixCount = NULL;
  /* for (i = 0; i < ncols ; i++) 
     {
     matrixCount = matrixBegin[i+1] - matrixBegin[i];
     }
  */
  /* à vérifier XXX */
  int objectSense = (sense == 0 ) ?  SOLV_OBJSENS_MAX: SOLV_OBJSENS_MIN;
  double objectConst=0;
  double *objectCoeffs = Objective->R;
  double *lowerBounds = lower->R;
  double *upperBounds = upper->R;
  double *rhsValues = Rhs->R;

  /* rowType should be of size rowcount and should contain
   * 'L', 'E', 'G', 'R', 'N' */
  char *rowType = NULL;

  /* we can provide colType of size colcount and should contain 
   * 'C', 'B', 'I' 
   */
  char *columnType = NULL;

  HPROB hProb;
  int result;
  /* pass extra arguments to message callbacks */
  const char *userParam = "TEST";
  hProb = CoinCreateProblem(problemName);  
  result = CoinLoadMatrix(hProb, colCount, rowCount, nonZeroCount, rangeCount,
			  objectSense, objectConst, objectCoeffs, lowerBounds, upperBounds, 
			  rowType, rhsValues, rangeValues, matrixBegin, matrixCount, 
			  matrixIndex, matrixValues);
  /* 
  result = CoinLoadNames(hProb, colNames, rowNames, objectName);
  if (columnType) {
    result = CoinLoadInteger(hProb, columnType);
  }
  */

  result = CoinCheckProblem(hProb);
  if (result != SOLV_CALL_SUCCESS) {
    fprintf(stdout, "Check Problem failed (result = %d)\n", result);
  }
  result = CoinRegisterMsgLogCallback(hProb, &MsgLogCallback, (void*)userParam);
  if (columnType == NULL)
    result = CoinRegisterLPIterCallback(hProb, &IterCallback, (void*)userParam);
  else {
    result = CoinRegisterMipNodeCallback(hProb, &MipNodeCallback, (void*)userParam);
  }
  result = CoinOptimizeProblem(hProb, 0);
  /* 
  char filename[260];
  strcpy(filename, problemName);
  strcat(filename, ".mps");
  result = CoinWriteFile(hProb, SOLV_FILE_MPS, filename);
  */
  Retcode->R[0]= CoinGetSolutionStatus(hProb);
  /* const char* solutionText = CoinGetSolutionText(hProb); */
  RetCost->R[0]= CoinGetObjectValue(hProb);
  CoinGetSolutionValues(hProb, X->R, NULL, NULL, NULL);
  CoinUnloadProblem(hProb);
  return OK;
}



