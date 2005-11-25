#ifndef NSP_INC_MEX 
#define NSP_INC_MEX 

#define INTERSIZ 256

#include "nsp/math.h"
#include "nsp/sciio.h"
#include "nsp/interf.h"

typedef int mxArray;
typedef void mexfun(int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[]);
typedef enum { mxREAL, mxCOMPLEX } mxComplexity; 

typedef int bool;

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
extern int *mxCreateDoubleMatrix (int m, int n, mxComplexity it);
extern void *mxCalloc (unsigned int n, unsigned int size);
extern int mxGetString (const mxArray *ptr, char *str, int strl);
extern void mxFreeMatrix (mxArray *ptr);
extern void mxFree (void *ptr);
extern int mexAtExit(void (*ExitFcn)(void));
extern void mxCreateSparse (mxArray *ptr);
extern int *mxCreateString (char *string);
extern int *mxGetJc(const mxArray *ptr);
extern int *mxGetIr(const mxArray *ptr);
extern bool mxIsChar(const mxArray *ptr);

extern int mxGetNumberOfDimensions (const mxArray *ptr);
extern int mxGetNumberOfFields (const mxArray *ptr);
extern int mxGetNumberOfElements(const mxArray *ptr);
extern bool mxIsStruct(const mxArray *ptr);
extern mxArray *mxCreateStructMatrix(int m, int n, int nfields, const char **field_names);
extern double mxGetInf(void);
extern double mxGetNaN(void);
extern double mxGetEps(void);
extern bool mxIsInf(double x);
extern bool mxIsFinite(double x);
extern bool mxIsNaN(double x);
extern double mxGetScalar(const mxArray *ptr);
extern mxArray *mxGetField (const mxArray *pa, int i, char *fieldname);
extern void mxSetField (mxArray *pa, int i, const char *fieldname, mxArray *value);
extern void mexWarnMsgTxt(char *error_msg);
extern bool mxIsCell (const mxArray *ptr);
extern mxArray *mxGetCell(const mxArray *ptr, int index);
extern void mxSetCell(mxArray *array_ptr, int index, mxArray *value);
extern mxArray *mxCreateCellMatrix(int m, int n);
extern char *mxArrayToString(const mxArray *array_ptr);

#define mexPrintf Sciprintf

#ifndef MEXLIB 
/* typedef int NspMatrix; */
#endif 

#endif /* NSP_MEX */




