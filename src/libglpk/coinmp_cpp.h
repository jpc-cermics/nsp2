#ifndef NSP_COINMP_CPP_H 
#define NSP_COINMP_CPP_H 
#ifdef __cplusplus
extern "C" {
#endif 

#include <nsp/nsp.h>
#include <nsp/objects.h>
#include <nsp/imatrix.h>

  typedef struct _nsp_clp_params1 nsp_clp_params1 ;
  
  struct _nsp_clp_params1
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

  extern int nsp_coinmp_solve(const char* problemName, int sense, int ncols, int nrows,
			      NspIMatrix*Cmatbeg, NspIMatrix *Cmatcount, NspIMatrix *Cmatind, NspMatrix *Cmatval, 
			      NspMatrix *lower, NspMatrix *upper, NspMatrix *Objective,
			      NspMatrix *Rhs,const char *columnType,  NspMatrix *X,NspMatrix *Lambda,
			      NspMatrix *RetCost,NspMatrix *Retcode,const char *rowType,
			      int semiCount, int *semiIndex,NspHash *Options);

  extern NspHash *nsp_coinmp_get_options(void);

  extern int nsp_cplex_solve(const char* problemName, int sense, int ncols, int nrows,
			     NspIMatrix*Cmatbeg, NspIMatrix *Cmatcount, NspIMatrix *Cmatind, NspMatrix *Cmatval, 
			     NspMatrix *lower, NspMatrix *upper, NspMatrix *Objective,
			     NspIMatrix*Qmatbeg,NspIMatrix *Qmatcnt, NspIMatrix *Qmatind, NspMatrix *Qmatval, 
			     NspMatrix *Rhs,const char *columnType,  NspMatrix *X,NspMatrix *Lambda,
			     NspMatrix *RetCost,NspMatrix *Retcode,const char *rowType,
			     int semiCount, int *semiIndex,NspHash *Options,int loglevel);

#ifdef __cplusplus
}
#endif 


#endif /*NSP_COINMP_CPP_H */


