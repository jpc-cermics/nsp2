#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>
#include <math.h>

#include "CoinMP.h"
#include "coinmp_cpp.h"

/* interface for the coinmp interface to linear programming */

int SOLVCALL MsgLogCallback(const char* MessageStr, void *UserParam)
{
  //fprintf(stdout, "*** MSG: %s, user=%s\n", MessageStr, (char*)UserParam);
  return 0;
}

int SOLVCALL IterCallback(int    IterCount, 
			double ObjectValue,
			int    IsFeasible, 
			double InfeasValue,
			void   *UserParam)
{
  //fprintf(stdout, "*** ITER: iter=%d, obj=%.20g, feas=%d, infeas=%.20g, user=%s\n",
  //IterCount, ObjectValue, IsFeasible, InfeasValue, (char*)UserParam);
  return 0;
}

int SOLVCALL MipNodeCallback(int    IterCount, 
			     int	  MipNodeCount,
			     double BestBound,
			     double BestInteger,
			     int    IsMipImproved,
			     void   *UserParam)
{
  //fprintf(stdout, "*** NODE: iter=%d, node=%d, bound=%.20g, best=%.20g, %s, user=%s\n",
  // IterCount, MipNodeCount, BestBound, BestInteger, IsMipImproved ? "Improved" : "*", (char*)UserParam);
  return 0;
}

int nsp_coinmp_solve(const char* problemName, int sense, int ncols, int nrows,
		     NspIMatrix*Cmatbeg, NspIMatrix *Cmatcount, NspIMatrix *Cmatind, NspMatrix *Cmatval, 
		     NspMatrix *lower, NspMatrix *upper, NspMatrix *Objective,
		     NspMatrix *Rhs,const char *columnType,  NspMatrix *X,NspMatrix *Lambda,
		     NspMatrix *RetCost,NspMatrix *Retcode,const char *rowType,
		     int semiCount, int *semiIndex)
{
  /* matrix A part */
  int *matrixBegin = (int *) Cmatbeg->Iv;
  int *matrixIndex = (int *) Cmatind->Iv;
  int *matrixCount = (int *) Cmatcount->Iv;
  double *matrixValues = Cmatval->R; 
  int colCount = ncols, rowCount = nrows; 
  int nonZeroCount = matrixBegin[ncols]; /* number of non null elements in the matrix */
  int rangeCount = 0; double *rangeValues = NULL;
  int objectSense = (sense == 0 ) ?  SOLV_OBJSENS_MIN: SOLV_OBJSENS_MAX;
  double objectConst=0;
  double *objectCoeffs = Objective->R;
  double *lowerBounds = lower->R;
  double *upperBounds = upper->R;
  double *rhsValues = Rhs->R;
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
  */

  if (columnType) 
    {
      result = CoinLoadInteger(hProb, columnType);
      if (result != SOLV_CALL_SUCCESS) {
	Scierror("Error: faile to set column types\n");
	return FAIL;
      }
    }
  if ( semiCount != 0) 
    {
      result = CoinLoadSemiCont(hProb, semiCount, semiIndex);
      if (result != SOLV_CALL_SUCCESS) {
	Scierror("Error: failed to set column types\n");
	return FAIL;
      }
    }
  
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
  CoinGetSolutionValues(hProb, X->R, NULL, NULL, Lambda->R);
  CoinUnloadProblem(hProb);
  return OK;
}



