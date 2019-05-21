#ifndef NSP_INC_SPTRIPLETMATRIX 
#define NSP_INC_SPTRIPLETMATRIX 

/*
 * This Software is GPL (Copyright ENPC 1998-2019) 
 * Jean-Philippe Chancelier Enpc/Cermics         
 */

/* used to store a matlab compatible representation */

typedef struct _nsp_sparse_triplet nsp_sparse_triplet;

struct _nsp_sparse_triplet { /* used in mexlib */
  int *Jc; /* Ap;*/ /* m+ 1 */
  int *Ir; /* Ai;*/ /* Aisize */
  double *Pr; /* Ax;*/ /* Aisize */
  double *Pi; /* Ax;*/ /* Aisize */
  int m,n,Aisize;
};

#endif 
