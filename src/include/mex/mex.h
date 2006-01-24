#ifndef NSP_INC_MEX 
#define NSP_INC_MEX 

#define INTERSIZ 256

#include "nsp/math.h"
#include "nsp/sciio.h"
#include "nsp/interf.h"

typedef NspObject mxArray ;

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

extern mxArray *mxCreateFull (int m, int n, int it);
extern mxArray *mxCreateDoubleMatrix(int m, int n,  mxComplexity it);

extern void *mxCalloc (unsigned int n, unsigned int size);
extern int mxGetString (const mxArray *ptr, char *str, int strl);
extern void mxFreeMatrix (mxArray *ptr);
extern void mxFree (void *ptr);
extern int mexAtExit(void (*ExitFcn)(void));

extern mxArray *mxCreateSparse(int m, int n, int nzmax, 
			       mxComplexity ComplexFlag);

extern mxArray *mxCreateString(char *string);

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
extern void mxSetCell(mxArray *ptr, int index, mxArray *value);
extern mxArray *mxCreateCellMatrix(int m, int n);
extern char *mxArrayToString(const mxArray *ptr);

#define mexPrintf Sciprintf

#ifndef MEXLIB 
  /* typedef int NspMatrix; */
#endif 

typedef int  mxClassID;

extern mxClassID mxGetClassID(const mxArray *ptr) ;



/* this must be done after the followind id's have 
 * been initialized 
 */

#define  mxUNKNOWN_CLASS 0
#define  mxCELL_CLASS  nsp_type_cells_id
/* waiting for matlab struct */
#define  mxSTRUCT_CLASS  nsp_type_hash_id 
#define  mxCHAR_CLASS  nsp_type_smatrix_id
#define  mxLOGICAL_CLASS  nsp_type_bmatrix_id
#define  mxDOUBLE_CLASS  nsp_type_matrix_id
#define  mxSINGLE_CLASS  -1 
#define  mxINT8_CLASS  -1
#define  mxUINT8_CLASS  -1 
#define  mxINT16_CLASS  -1
#define  mxUINT16_CLASS  -1
#define  mxINT32_CLASS  -1
#define  mxUINT32_CLASS  -1
#define  mxINT64_CLASS  -1
#define  mxUINT64_CLASS  -1;
#define  mxFUNCTION_CLASS  nsp_type_plist_id 

extern void *mxMalloc(size_t n);
extern void mxDestroyArray(mxArray *ptr);
extern mxArray *mxCreateCellArray(int ndim, const int *dims);
extern int mxCalcSingleSubscript(const mxArray *ptr, int nsubs,const int *subs);extern const char * mxGetName(const mxArray *ptr) ;

extern int mexPutVariable(const char *workspace, const char *var_name,mxArray *array_ptr);

extern mxArray *mxCreateCharMatrixFromStrings(int m, const char **str);

extern int mexCallMATLAB(int nlhs, mxArray *plhs[], int nrhs,const  mxArray *prhs[], const char *command_name);
#define mexCallNsp mexCallMATLAB
#define mexCallScilab mexCallMATLAB


extern mxArray *mxDuplicateArray(const mxArray *in);
extern void mxSetName(mxArray *array_ptr,const char *var_name);
extern int mexPutArray( mxArray *array_ptr,const char *workspace);
extern int mexEvalString( char *command);
extern int *mxGetDimensions(const mxArray *ptr);
extern int mxSetNzmax( mxArray *array_ptr,int n);
extern int mxGetNzmax( mxArray *array_ptr);




#endif /* NSP_MEX */




