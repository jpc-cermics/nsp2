#ifndef NSP_MAXPLUS_LIB 
#define NSP_MAXPLUS_LIB 

extern int Howard (int *ij, double *A,int nnodes,int narcs,double *chi,double *v,int *pi,
		   int *NIterations,int *NComponents,int verbosemode);

extern int Semi_Howard (int *ij, double *A,double *t,int nnodes,int narcs,double *chi,
			double *v,int *pi,int *NIterations,int *NComponents,int verbosemode);


extern int maxplus_matrix_karp(NspMatrix *A,int entry,double *res);

extern int in_span (double *A,int n, int p, double *b, double precision);
/* weakbasis */
extern void weakbasis (double *A,int n,int p, double **B, int *q, double precision);
/* weakbasis */
extern void weakbasis2 (double *A,int n,int p, double **B, int *q, double precision);

/* include_span */
extern int include_span (double *A,int n, int p, double *B, int q, double precision);
extern void product (double *A, int n, int p, double *B, int q, double *C);
extern void rowbasis (double *A,int n,double *B,double **U, int *q, double precision);
extern int solve2 (double *A, int n, int p, double *B,double **U,double precision);
extern int solve3 (double *A, int n, int p, double *B,double **U,double precision);
extern int FordBellman (int *ij, double *A,int nnodes,int narcs,int entry, double *u,int *policy, int *niterations);
extern void matrix_plus (double *B, int n, double *C);
extern void matrix_star (double *B, int n, double *C);

extern void Display_Policy(int *,double *,int);
extern void Display_Semi_Policy(int *,double *,double *, int);
extern void Display_Vector(int, double *);
extern void Display_Vector_String(int, double *,char *);
extern void Display_Sparse_Matrix(int, int *,double *);
extern void Display_Semi_Sparse_Matrix(int, int *,double *,double *);
extern void Display_Max_Policy(int *,int );
extern void Display_Min_Max_Function(int, int *,double *,int *);

#endif 


