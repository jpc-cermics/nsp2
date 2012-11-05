#ifndef __SCICOS_BLOCK_4_H 
#define __SCICOS_BLOCK_4_H 

typedef enum { fun_macros, fun_macro_name, fun_pointer} scicos_funflag;

typedef struct  _scicos_block scicos_block ;

struct  _scicos_block {
  int nevprt;
  void *funpt ; /* hard coded function */
  int type;
  scicos_funflag scsptr_flag ;  /* to decide if next field is a name or a macro */
  void *scsptr;  /* macros (in fact a NspObject *) or name  */
  int nz;
  double *z;
  int nx;
  double *x;
  double *xd;
  double *res;
  double *res_init; /* keep track of initialy allocated res */
  int nin;
  int *insz;
  void **inptr;
  int nout;
  int *outsz;
  void **outptr;
  int nevout;
  double *evout;
  int nrpar;
  double *rpar;
  int nipar;
  int *ipar;
  int ng;
  double *g;
  int ztyp;
  int *jroot;
  int *jroot_init; /* keep track of initial jroot */
  char *label;
  void **work;
  int nmode;
  int *mode;
  int noz;
  int *ozsz;
  int *oztyp;
  void **ozptr;
  int *xprop;
  int nopar;
  int *oparsz;
  int *opartyp;
  void **oparptr;
  double *alpha;
  double *beta;
  void *grobj; /* Nsp graphic object */
};

#include "scicos4.h"

#endif 
