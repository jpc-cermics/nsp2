#include "nsp/mex.h"
#include "lib/pipo.h"


void mexFunction(int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[])
{
  const mxArray *ptrA; 
  mxArray *ptrB;
  double *A,*B;
  int m,n;
  if (nrhs != 1) mexErrMsgTxt("This function requires 1 input!");
  if (nlhs > 1) mexErrMsgTxt("This function requires at most 1 output !");
  ptrA = prhs[0];
  if (! mxIsNumeric(prhs[0])) mexErrMsgTxt("First argument must be numeric matrix.");
  m = mxGetM(ptrA);n = mxGetN(ptrA);
  A = mxGetPr(ptrA);
  ptrB = mxCreateFull(1,1,0);
  B = mxGetPr(ptrB);
  if (nlhs > 1) {
    mexErrMsgTxt("At most 1 output!");
  }
  B[0] = foo( (int) A[0]); 
  plhs[0]= ptrB;
}  

