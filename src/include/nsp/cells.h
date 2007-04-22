#ifndef NSP_INC_CELLS
#define NSP_INC_CELLS

/*
 * This Software is GPL (Copyright ENPC 1998-2007) 
 * Jean-Philippe Chancelier Enpc/Cermics         
 */

#include <stdio.h>   /* for file declaration **/
#include "nsp/sciio.h" 

/**
 * NspCells: 
 * @m: number of rows 
 * @n: number of columns
 * @mn: @m x @n
 * @objs: array containing the stored #NspObject 
 *
 * inherits from #NspObject and used for cells array i.e 
 * arrays of nsp objects. 
 */

typedef struct _NspCells NspCells ;

typedef struct _NspTypeCells { 
  NSP_TYPE_OBJECT__ 
  /*< public >*/
} NspTypeCells;

struct _NspCells {
  /*< private >*/
  NspObject father; 
  NspTypeCells *type; 
  /*< public >*/
  int m,n,mn;
  NspObject **objs;
};

extern int nsp_type_cells_id;
extern NspTypeCells *nsp_type_cells;

/* only useful when building a new class derived from matrix */

extern NspTypeCells *new_type_cells(type_mode mode) ;

/* only useful when building a new class derived from matrix */

extern NspCells *new_cells();

/*
 * Object methods redefined for smatrix 
 */

/* prototypes */

extern NspCells *nsp_cells_object(NspObject *O); 
extern int IsCellsObj (Stack stack, int i); 
extern int IsCells (const NspObject *O); 
extern NspCells *GetCellsCopy (Stack stack, int i); 
extern NspCells *GetCells (Stack stack, int i); 
extern NspCells *nsp_cells_create(const char *name, int m, int n);
extern NspCells *nsp_cells_clone(const char *name, NspCells *A, int m, int n, int init);
extern NspCells *nsp_cells_create_from_table(const char *name,NspObject **T); 
extern NspCells *nsp_cells_create_from_array(const char *name,int n, NspObject **T); 
extern NspCells *nsp_cells_copy(const NspCells *A); 
extern int nsp_cells_resize(NspCells *A, int m, int n); 
extern unsigned int  nsp_cells_elt_size(NspCells *A);
extern void nsp_cells_destroy(NspCells *A); 
extern void nsp_cells_info(const NspCells *Mat, int indent,char *name, int rec_level); 
extern void nsp_cells_print(const NspCells *Mat, int indent,char *name, int rec_level); 
extern int nsp_cells_redim(NspCells *A, int m, int n); 
extern int nsp_cells_enlarge(NspCells *A, int m, int n); 
extern int nsp_cells_concat_right(NspCells *A,const NspCells *B); 
extern int nsp_cells_add_columns(NspCells *A, int n); 
extern NspCells *nsp_cells_concat_down(const NspCells *A,const NspCells *B); 
extern int nsp_cells_add_rows(NspCells *A, int m); 
extern int nsp_cells_set_submatrix(NspCells *A,const NspMatrix *Rows,const NspMatrix *Cols,const NspCells *B); 
extern int nsp_cells_set_rows(NspCells *A, NspMatrix *Rows, NspCells *B); 
extern NspCells *nsp_cells_extract(NspCells *A, NspMatrix *Rows, NspMatrix *Cols); 
extern NspCells *nsp_cells_extract_elements(NspCells *A, NspMatrix *Elts, int *err); 
extern NspCells *nsp_cells_extract_columns(NspCells *A, NspMatrix *Cols, int *err); 
extern NspCells *CellsLoopCol (char *str, NspCells *Col, NspCells *A, int icol, int *rep); 
extern NspCells *nsp_cells_extract_rows(NspCells *A, NspMatrix *Rows, int *err); 
extern int nsp_cells_set_element(NspCells *A,int index, NspObject *B);
extern NspCells *nsp_cells_unique(NspCells *C, NspMatrix **Ind, NspMatrix **Occ);
extern Boolean nsp_cells_has(NspCells *C, NspObject *Obj, int *ind);
extern NspCells *nsp_cells_map(NspCells *C, NspPList *PL, NspList *args)  ;


#define NULLCELLS (NspCells *) NULL

extern NspMatrix *nsp_cells_strcmp(NspCells *A, NspCells *B); 
extern NspBMatrix *CellsCompOp (NspCells *A, NspCells *B, char *op); 
extern int CellsFullComp (NspCells *A, NspCells *B, char *op, int *err); 
extern NspCells *nsp_cells_transpose(const NspCells *A); 

#endif 

/* private declarations */

#ifdef Cells_Private 
static int init_cells(NspCells *ob,NspTypeCells *type);
static int nsp_cells_size(NspCells *Mat, int flag);
static NspMethods *cells_get_methods(void);
char *nsp_cells_type_as_string(void);
char *nsp_cells_type_short_string(NspObject *v);
NspObject *nsp_cells_loop_extract(char *str, NspObject *O, NspObject *O1, int i, int *rep);
int nsp_cells_eq(NspObject *A,NspObject *B);
int nsp_cells_neq(NspObject *A,NspObject *B);
int nsp_cells_is_true(NspCells *M);
NspCells *nsp_cells_xdr_load(XDR  *F);
int nsp_cells_xdr_save(XDR *xdrs, NspCells *M);
#endif 
