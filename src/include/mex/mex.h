#ifndef NSP_INC_MEX 
#define NSP_INC_MEX 

#define INTERSIZ 256

#include "nsp/math.h"
#include "nsp/sciio.h"
#include "nsp/interf.h"

typedef int mxArray;
typedef void mexfun(int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[]);

extern int nsp_mex_wrapper(Stack stack, int rhs, int opt, int lhs,mexfun *mexFunction);
extern double *mxGetPr (const mxArray *ptr);
extern double *mxGetPi (const mxArray *ptr);
extern int mxGetM (const mxArray *ptr);
extern int mxGetN (const mxArray *ptr);
extern int mxIsString (const mxArray *ptr);
extern int mxIsNumeric (const mxArray *ptr);
extern int mxIsFull (const mxArray *ptr);
extern int mxIsSparse (const mxArray *ptr);
extern int mxIsComplex (const mxArray *ptr);
extern double mxGetScalar (const mxArray *ptr);
extern void mexErrMsgTxt (char *error_msg);
extern int *mxCreateFull (int m, int n, int it);
extern void *mxCalloc (unsigned int n, unsigned int size);
extern int mxGetString (const mxArray *ptr, char *str, int strl);
extern void mxFreeMatrix (mxArray *ptr);
extern void mxFree (void *ptr);
extern int mexAtExit(void (*ExitFcn)(void));
extern void mxCreateSparse (mxArray *ptr);
extern int *mxCreateString (char *string);
extern int *mxGetJc(const mxArray *ptr);
extern int *mxGetIr(const mxArray *ptr);


#define mexPrintf Sciprintf

#ifndef MEXLIB 
/* typedef int NspMatrix; */
#endif 

#endif /* NSP_MEX */




