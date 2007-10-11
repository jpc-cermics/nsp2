#ifndef NSP_INC_MEX 
#define NSP_INC_MEX 

#define INTERSIZ 256

#ifdef __cplusplus
extern "C" { 
#endif 

#include "nsp/math.h"
#include "nsp/sciio.h"
#include "nsp/interf.h"

/**
 * mxArray: 
 *
 * object used in mex interfaces. This is just an alias (through 
 * typedef) of #NspObject.
 */

typedef NspObject mxArray ;
typedef int mxLogical; /* should be int on Nsp */
typedef char mxChar;

typedef void mexfun(int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[]);
/* typedef void mexfun(int nlhs, mxArray *plhs[], int nrhs,mxArray *prhs[]); */

typedef enum { mxREAL, mxCOMPLEX } mxComplexity; 

#ifndef __cplusplus
typedef int bool;
#endif 

#ifndef true 
#define true TRUE 
#endif 
#ifndef false 
#define false FALSE
#endif 

extern void _mxAssert(char *mess,int line,const char *file);
#define mxAssert(cond,mess) if ( !(cond) ) _mxAssert(mess,__LINE__,__FILE__) ;
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
extern void mexErrMsgTxt (const char *error_msg);

extern mxArray *mxCreateFull (int m, int n, int it);
extern mxArray *mxCreateDoubleMatrix(int m, int n,  mxComplexity it);

extern void *mxCalloc (unsigned int n, unsigned int size);
extern int mxGetString (const mxArray *ptr, char *str, int strl);
extern void mxFreeMatrix (mxArray *ptr);
extern void mxFree (void *ptr);
extern int mexAtExit(void (*ExitFcn)(void));

extern mxArray *mxCreateSparse(int m, int n, int nzmax, 
			       mxComplexity ComplexFlag);

extern mxArray *mxCreateString(const char *string);

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
extern mxArray *mxGetField (const mxArray *pa, int i,const char *fieldname);
extern void mxSetField (mxArray *pa, int i, const char *fieldname, mxArray *value);
extern void mexWarnMsgTxt(const char *error_msg);
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
#define  mxOBJECT_CLASS  nsp_type_object_id
#define  mxSPARSE_CLASS  nsp_type_spcolmatrix_id
#define  mxSINGLE_CLASS  -1 
#define  mxINT8_CLASS  -2
#define  mxUINT8_CLASS  -3
#define  mxINT16_CLASS  -4
#define  mxUINT16_CLASS  -5
#define  mxINT32_CLASS  -6
#define  mxUINT32_CLASS  -7
#define  mxINT64_CLASS  -8
#define  mxUINT64_CLASS  -9
#define  mxOPAQUE_CLASS  -10
#define  mxFUNCTION_CLASS  nsp_type_plist_id 

extern void *mxMalloc(size_t n);
extern void mxDestroyArray(mxArray *ptr);
extern mxArray *mxCreateCellArray(int ndim, const int *dims);
extern int mxCalcSingleSubscript(const mxArray *ptr, int nsubs,const int *subs);extern const char * mxGetName(const mxArray *ptr) ;

extern int mexPutVariable(const char *workspace, const char *var_name,mxArray *array_ptr);

extern mxArray *mxCreateCharMatrixFromStrings(int m, const char **str);

extern int mexCallMATLAB(int nlhs, mxArray *plhs[], int nrhs,mxArray *prhs[],const char *command_name);
#define mexCallNsp mexCallMATLAB
#define mexCallScilab mexCallMATLAB

extern mxArray *mxDuplicateArray(const mxArray *in);
extern void mxSetName(mxArray *array_ptr,const char *var_name);
extern int mexPutArray( mxArray *array_ptr,const char *workspace);
extern int mexEvalString( char *command);
extern int *mxGetDimensions(const mxArray *ptr);
extern int mxSetNzmax( mxArray *array_ptr,int n);
extern int mxGetNzmax(const mxArray *array_ptr);


extern mxArray *mxCreateScalarDouble(double value);
extern mxArray *mxCreateDoubleScalar(double value);
extern bool mxIsEmpty(const mxArray *array_ptr);
extern void mexMakeArrayPersistent(mxArray *array_ptr);
extern mxArray *mxCreateLogicalScalar(mxLogical value);
extern void mexMakeMemoryPersistent(void *ptr);
extern bool mxIsLogicalScalarTrue(const mxArray *array_ptr);
extern void mexLock(void); 
extern void mexUnlock(void); 
extern bool mexIsLocked(void);
extern bool mxIsLogicalScalar(const mxArray *array_ptr);
extern bool mxIsLogical(const mxArray *array_ptr);

extern const char *mxGetFieldNameByNumber(const mxArray *array_ptr, 
					  int field_number);

extern void *mxGetData(const mxArray *array_ptr);

extern mxArray *mxCreateCharArray(int ndim, const int *dims);

extern mxArray *mxGetFieldByNumber(const mxArray *array_ptr, int index, 
				   int field_number);

extern int mxGetElementSize(const mxArray *array_ptr);
extern mxChar *mxGetChars(const mxArray *array_ptr);

extern mxLogical *mxGetLogicals(const mxArray *array_ptr);

extern const char *mxGetClassName(const mxArray *array_ptr);

  
extern bool mxIsSharedArray(const mxArray *array_ptr);
extern void mxUnshareArray(const mxArray *array_ptr);

extern mxArray *mxCreateLogicalArray(int ndim, const int *dims);
extern mxArray *mxCreateLogicalMatrix(int m, int n);

extern bool mxIsDouble(const mxArray *array_ptr);

extern mxArray *mxCreateStructArray(int ndim, const int *dims, int nfields,
				    const char **field_names);

extern mxArray *mexGetVariable(const char *workspace, const char *var_name);
extern mxArray *mexGetArray(const char *name, const char *workspace);
extern void mxSetLogical(mxArray *array_ptr);

extern void mxSetData(mxArray *array_ptr, void *pr);
extern void mxSetPr(mxArray *array_ptr, double *pr);
extern void mxSetJc(mxArray *array_ptr, int *jc);
extern void mxSetIr(mxArray *array_ptr,int *ir);
extern void *mxRealloc(void *ptr, size_t size);
extern void mxSetPi(mxArray *array_ptr, double *pi);

typedef int mwSize; 

extern void mxSetN(mxArray *ptr, mwSize n);
extern void mxSetM(mxArray *ptr, mwSize m);

extern mxArray *mxCreateSparseLogicalMatrix(int m, int n, int nzmax);

extern mxArray *mxCreateNumericArray(int ndim, const int *dims, 
				     mxClassID xclass,mxComplexity ComplexFlag);
extern void mxSetFieldByNumber(mxArray *array_ptr, int index,  
			       int field_number, mxArray *value);

extern void mxFreeSparseMtlbTriplet(const mxArray *ptr);
extern void mxSparseMtlbTripletTonsp(const mxArray *ptr) ;

extern void mexSetTrapFlag(int trapflag);

#ifdef __cplusplus
}
#endif 

#endif /* NSP_MEX */




