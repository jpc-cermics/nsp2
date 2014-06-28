#ifndef CPL_CPP_H 
#define CPL_CPP_H 
#ifdef __cplusplus
extern "C" {
#endif 

#include <nsp/nsp.h>
#include <nsp/objects.h>
#include <nsp/imatrix.h>

  typedef struct _nsp_clp_params nsp_clp_params ;
  
  struct _nsp_clp_params
  {
    int solverchoice ;
    int maxnumiterations ;
    int loglevel ;
    int primalpivot ;
    int dualpivot ;
    double maxnumseconds ;
    double primaltolerance ;
    double dualtolerance ;
  };

  /* exported functions from C++ which are used in C */
  
  extern double nsp_coin_dbl_max(void);

  extern int nsp_clp_solve(nsp_clp_params *options,int sense, int ncols, int nrows, int neq, 
			   NspIMatrix*Cmatbeg, NspIMatrix *Cmatind, NspMatrix *Cmatval, 
			   NspMatrix *lower, NspMatrix *upper, NspMatrix *Objective,
			   NspIMatrix*Qmatbeg, NspIMatrix *Qmatind, NspMatrix *Qmatval, 
			   NspMatrix *Rhs,  NspMatrix *Lhs, char *var_type[], NspMatrix *X,NspMatrix *Lambda,
			   NspMatrix *RetCost,NspMatrix *Retcode);

#ifdef __cplusplus
}
#endif 

#endif /* CPL_CPP_H  */


