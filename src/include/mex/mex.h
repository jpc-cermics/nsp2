#ifndef SCI_MEX 
#define SCI_MEX 

#define INTERSIZ 256

#include "nsp/math.h"
#include "nsp/sciio.h"
#include "nsp/interf.h"

typedef int mxArray;
typedef void mexfun(int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[]);
int nsp_mex_wrapper(Stack stack, int rhs, int opt, int lhs,mexfun *mexFunction);


double *mxGetPr (const mxArray *ptr);
double *mxGetPi (const mxArray *ptr);
int mxGetM (const mxArray *ptr);
int mxGetN (const mxArray *ptr);
int mxIsString (const mxArray *ptr);
int mxIsNumeric (const mxArray *ptr);
int mxIsFull (const mxArray *ptr);
int mxIsSparse (const mxArray *ptr);
int mxIsComplex (const mxArray *ptr);
double mxGetScalar (const mxArray *ptr);
void mexErrMsgTxt (char *error_msg);
int *mxCreateFull (int m, int n, int it);
void *mxCalloc (unsigned int n, unsigned int size);
int mxGetString (const mxArray *ptr, char *str, int strl);
void mxFreeMatrix (mxArray *ptr);
void mxFree (mxArray *ptr);
int mexAtExit (mxArray *ptr);
void mxCreateSparse (mxArray *ptr);
int *mxCreateString (char *string);

#define mexPrintf Sciprintf

#ifndef MEXLIB 
/* typedef int NspMatrix; */
#endif 

#endif /* SCI_MEX */




