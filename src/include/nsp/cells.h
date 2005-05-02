#ifndef INC_NSP_CELLS
#define INC_NSP_CELLS

/*********************************************************************
 * This Software is ( Copyright ENPC 1998-2005 )
 * Jean-Philippe Chancelier Enpc/Cermics        
 *********************************************************************/

#include <stdio.h>   /** for file declaration **/
#include "nsp/sciio.h" 

/*
 * NspMatrix inherits from NspObject 
 */
typedef struct _nsp_cells NspCells ;

typedef int (*cells_save) (NspFile  *F, NspCells *M);

typedef struct _nsp_type_Cells { 
  NSP_TYPE_OBJECT__ 
  /* rajouts */
  cells_save *save;
} NspTypeCells;

struct _nsp_cells {
  NspObject father; 
  NspTypeCells *type; 
  integer m,n,mn;
  NspObject **objs;
};

extern int nsp_type_cells_id;
extern NspTypeCells *nsp_type_cells;

/* only useful when building a new class derived from matrix */

NspTypeCells *new_type_cells(type_mode mode) ;

/* only useful when building a new class derived from matrix */

NspCells *new_cells();

/*
 * Object methods redefined for smatrix 
 */

#ifdef Cells_Private 
static int init_cells(NspCells *ob,NspTypeCells *type);
static int nsp_cells_size(NspCells *Mat, int flag);
char *nsp_cells_type_as_string(void);
char *nsp_cells_type_short_string(void);
NspObject *nsp_cells_loop_extract(char *str, NspObject *O, NspObject *O1, int i, int *rep);
int nsp_cells_eq(NspObject *A,NspObject *B);
int nsp_cells_neq(NspObject *A,NspObject *B);
int nsp_cells_is_true(NspCells *M);
NspCells *nsp_cells_xdr_load(NspFile  *F);
int nsp_cells_xdr_save(NspFile  *F, NspCells *M);
#endif 


/* prototypes */

extern NspCells *nsp_cells_object(NspObject *O); 
extern char *nsp_string_object(NspObject *O); 
extern int IsCellsObj (Stack stack, int i); 
extern int IsCells (NspObject *O); 
extern NspCells *GetCellsCopy (Stack stack, int i); 
extern NspCells *GetCells (Stack stack, int i); 
extern NspCells *nsp_cells_create(const char *name, integer m, integer n);
extern NspCells *nsp_cells_create_from_table(NspObject **T); 
extern NspCells *nsp_cells_create_from_array(const char *name,int n, NspObject **T); 
extern NspCells *nsp_cells_copy(const NspCells *A); 
extern int nsp_cells_resize(NspCells *A, integer m, integer n); 
extern void nsp_cells_destroy(NspCells *A); 
extern void nsp_cells_info(const NspCells *Mat, int indent); 
extern void nsp_cells_print(const NspCells *Mat, int indent); 
extern int nsp_cells_redim(NspCells *A, integer m, integer n); 
extern int nsp_cells_enlarge(NspCells *A, integer m, integer n); 
extern int nsp_cells_concat_right(NspCells *A,const NspCells *B); 
extern int nsp_cells_add_columns(NspCells *A, integer n); 
extern NspCells *nsp_cells_concat_down(const NspCells *A,const NspCells *B); 
extern int nsp_cells_add_rows(NspCells *A, integer m); 
extern int nsp_cells_set_submatrix(NspCells *A,const NspMatrix *Rows,const NspMatrix *Cols,const NspCells *B); 
extern int nsp_cells_set_rows(NspCells *A, NspMatrix *Rows, NspCells *B); 
extern int nsp_cells_delete_columns(NspCells *A, NspMatrix *Cols); 
extern int nsp_cells_delete_rows(NspCells *A, NspMatrix *Rows); 
extern int nsp_cells_delete_elements(NspCells *A, NspMatrix *Elts); 
extern NspCells *nsp_cells_extract(NspCells *A, NspMatrix *Rows, NspMatrix *Cols); 
extern NspCells *nsp_cells_extract_elements(NspCells *A, NspMatrix *Elts, int *err); 
extern NspCells *nsp_cells_extract_columns(NspCells *A, NspMatrix *Cols, int *err); 
extern NspCells *CellsLoopCol (char *str, NspCells *Col, NspCells *A, int icol, int *rep); 
extern NspCells *nsp_cells_extract_rows(NspCells *A, NspMatrix *Rows, int *err); 

#define NULLCELLS (NspCells *) NULL

extern NspMatrix *nsp_cells_strcmp(NspCells *A, NspCells *B); 
extern NspBMatrix *CellsCompOp (NspCells *A, NspCells *B, char *op); 
extern int CellsFullComp (NspCells *A, NspCells *B, char *op, int *err); 
extern NspCells *nsp_cells_transpose(const NspCells *A); 


#endif 

