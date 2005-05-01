/* Nsp
 * Copyright (C) 1998-2005 Jean-Philippe Chancelier Enpc/Cermics
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 */

#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define Cells_Private 
#include "nsp/object.h"

#include "nsp/pr-output.h" 
#include "nsp/interf.h"
#include "nsp/matutil.h"


/*
 * NspCells inherits from NspObject 
 */

int nsp_type_cells_id=0;
NspTypeCells *nsp_type_cells=NULL;
int nsp_type_cells_init();

NspTypeCells *new_type_cells(type_mode mode)
{
  NspTypeCells *type = NULL;
  NspTypeObject *top;

  if ( nsp_type_cells != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_cells;
    }
  if ((type =  malloc(sizeof(NspTypeCells))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype =(NspTypeBase *) new_type_object(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = NULL; /* cells_attrs ; */
  type->get_attrs = (attrs_func *) int_get_attribute; 
  type->set_attrs = (attrs_func *) int_set_attribute; 
  type->methods = NULL ; /* cells_get_methods; */
  type->new = (new_func *) new_cells;

  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for smatrix */ 

  top->pr = (print_func *)nsp_cells_print;                    /* printing*/   
  top->dealloc = (dealloc_func *)nsp_cells_destroy;              /* dealloc */  
  top->copy  =  (copy_func *)nsp_cells_copy;                   /* copy object */  
  top->size  = (size_func *)nsp_cells_size;                   /* m,n or m*n  */  
  top->s_type =  (s_type_func *)nsp_cells_type_as_string;                /* type as a String */  
  top->sh_type = (sh_type_func *)nsp_cells_type_short_string;              /* type as a short string */  
  top->info = (info_func *)nsp_cells_info;                    /* info */  
  top->is_true = (is_true_func  *)nsp_cells_is_true;             /* check if object can be considered as true */  
  top->loop =(loop_func *)nsp_cells_loop_extract;                /* for loops */  
  top->path_extract =  NULL;        /* used for x(1)(2)(...) */  
  top->get_from_obj = (get_from_obj_func *)nsp_cells_object;    /* get object stored in SciObj */  
  top->eq  = (eq_func *)nsp_cells_eq;                       /* equality check */  
  top->neq  = (eq_func *)nsp_cells_neq;                      /* non-equality check */

  top->save  = (save_func *)nsp_cells_xdr_save;
  top->load  = (load_func *)nsp_cells_xdr_load;

  /* specific methods for smatrix */
  type->init = (init_func *) init_cells;
  /* 
   * interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  
  if ( nsp_type_cells_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeMatrix called nsp_type_cells
       */
      type->id =  nsp_type_cells_id = nsp_new_type_id();
      nsp_type_cells = type;
      if ( nsp_register_type(nsp_type_cells) == FALSE) return NULL;
      return ( mode == T_BASE ) ? type : new_type_cells(mode);
    }
  else 
    {
      type->id = nsp_type_cells_id;
      return type;
    }

}

/*
 * initialize Smatrix instances 
 * locally and by calling initializer on parent class 
 */

static int init_cells(NspCells *o,NspTypeCells *type)
{
  /* to be done always */ 
  if ( type->surtype->init(&o->father,type->surtype) == FAIL) return FAIL;
  o->type = type; 
  NSP_OBJECT(o)->basetype = (NspTypeBase *)type;
  /* specific */
  return OK;
}

/*
 * new instance of Smatrix 
 */

NspCells *new_cells() 
{
  NspCells *loc; 
  /* type must exists */
  nsp_type_cells = new_type_cells(T_BASE);
  if ( (loc = malloc(sizeof(NspCells)))== NULLCELLS) return loc;
  /* initialize object */
  if ( init_cells(loc,nsp_type_cells) == FAIL) return NULLCELLS;
  return loc;
}


/*
 * MatSize : returns m,n,or m*n 
 */

int nsp_cells_size(NspCells*Mat, int flag)
{
  switch (flag) 
    {
    case 0: return Mat->mn;
    case 1: return Mat->m;
    case 2: return Mat->n;
    }
  return 0;
}

/*
 * MatType 
 */

static char smat_type_name[]="Cells";
static char smat_short_type_name[]="ce";

char *nsp_cells_type_as_string(void)
{
  return(smat_type_name);
}

char *nsp_cells_type_short_string(void)
{
  return(smat_short_type_name);
}

NspObject *nsp_cells_loop_extract(char *str, NspObject *O, NspObject *O1, int i, int *rep)
{
  NspCells *M= (NspCells *) O1,*M1=NULLCELLS;
  if ( O == NULLOBJ ) 
    {
      if (( M1= CellsLoopCol(str,NULLCELLS,M,i,rep))==NULLCELLS) return NULLOBJ;
      if (( *rep == RET_ENDFOR )) return NULLOBJ;
      return (NspObject *) M1 ;
    }
  else
    {
      if (( M1 =nsp_cells_object(O)) == NULLCELLS ) return NULLOBJ;
      M1= CellsLoopCol(str,M1,M,i,rep);
      if (( *rep == RET_ENDFOR )) return NULLOBJ;
      return O;
    }
}

int nsp_cells_eq(NspObject *A, NspObject *B)
{
  int err,rep;
  if ( check_cast(B,nsp_type_cells_id) == FALSE) return FALSE ;
  rep = CellsFullComp((NspCells *) A,(NspCells *) B,"==",&err);
  if ( err == 1) return FALSE ; 
  return rep;
}

int nsp_cells_neq(NspObject *A, NspObject *B)
{
  int err,rep;
  if ( check_cast(B,nsp_type_cells_id) == FALSE) return TRUE;
  rep = CellsFullComp((NspCells *) A,(NspCells *) B,"<>",&err);
  if ( err == 1) return TRUE ; 
  return rep;
}

/*
 * Cells == TRUE ? 
 * if Mat != [] and all the elements of Mat are != NULL
 */

int nsp_cells_is_true(NspCells *M)
{
  int i;
  if ( M->mn == 0) return FALSE;
  for ( i = 0 ; i < M->mn ; i++ ) 
    if ( M->objs[i] == NULL) return FALSE;
  return TRUE;
}


/*
 * Save a NspCells object 
 * FIXME: this is unfinished 
 *   NULL elements are badly processed
 */

int nsp_cells_xdr_save(NspFile  *F, NspCells *M)
{
  int i,rep;
  if (nsp_xdr_save_i(F->xdrs,M->type->id) == FAIL) return FAIL;
  if (nsp_xdr_save_string(F->xdrs, NSP_OBJECT(M)->name) == FAIL) return FAIL;
  if (nsp_xdr_save_i(F->xdrs,M->m) == FAIL) return FAIL;
  if (nsp_xdr_save_i(F->xdrs,M->n) == FAIL) return FAIL;
  for ( i= 0 ; i < M->mn ; i++) 
    {
      if (M->objs[i]   == NULLOBJ)
	{
	  Scierror("Warning:\t trying to save a null object in a cell\n");
	  return FAIL;
	}
      rep = M->objs[i]->type->save(F,M->objs[i]);
      if ( rep == FAIL) return FAIL;
    }
  return OK;
}

/*
 * Load a NspCells 
 */

NspCells *nsp_cells_xdr_load(NspFile  *F)
{
  int m,n,i;
  NspCells *M;
  static char name[NAME_MAXL];
  if (nsp_xdr_load_string(F->xdrs,name,NAME_MAXL) == FAIL) return NULLCELLS;
  if (nsp_xdr_load_i(F->xdrs,&m) == FAIL) return NULLCELLS;
  if (nsp_xdr_load_i(F->xdrs,&n) == FAIL) return NULLCELLS ;
  /* initial mxn matrix with unallocated elements **/
  if ( ( M =nsp_cells_create(name,m,n)) == NULLCELLS) return(NULLCELLS);
  /* allocate elements and store copies of A elements **/
  for ( i = 0 ; i < m*n ; i++ )
    {
      NspObject *Ob= nsp_object_xdr_load(F);
      if ( Ob == NULLOBJ ) return NULLCELLS;
      M->objs[i] = Ob;
    }
  return M;
}


/*
 * A =nsp_cells_object(O);
 * checks that O is an object of NspCells type. 
 * or a Hobj which points to an object of type Cells
 * if so, returns a pointer to that NspCells and else returns NULL
 */

NspCells   *nsp_cells_object(NspObject *O)
{
  /* Follow pointer **/
  if ( check_cast(O,nsp_type_hobj_id) == TRUE)  O = ((NspHobj *) O)->O ;
  /* Check type **/
  if ( check_cast(O,nsp_type_cells_id) == TRUE) return ((NspCells *) O);
  else 
    Scierror("Error:\tArgument should be a %s\n",type_get_name(nsp_type_cells));
  return(NULL);
}



/*
 * IsCellsObj(stack,i)
 * only checks that object at position 
 * first + i -1  is an object of type  Cells 
 * or a Hobj which points to an object of type Cells
 */

int IsCellsObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i) , nsp_type_cells_id);
}

/*
 * IsCells(O)
 * only checks that object is an object of type  Cells 
 * or a Hobj which points to an object of type Cells
 */

int IsCells(NspObject *O)
{
  return nsp_object_type(O , nsp_type_cells_id);
}

/*
 * Checks that first+i object on the stack 
 * is a NspCells and returns that NspCells  
 * or a copy of that NspCells if its name 
 * is != NVOID 
 */

NspCells*GetCellsCopy(Stack stack, int i)
{
  if (  GetCells(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

/*
 * Checks that first+i object on the stack 
 * is a NspCells and returns that NspCells  
 */

NspCells*GetCells(Stack stack, int i)
{
  NspCells *M;
  if (( M =nsp_cells_object(NthObj(i))) == NULLCELLS  )
    ArgMessage(stack,i);
  return M;
}

/*
 * Now the interfaced function for cells 
 */

/*
 * Creation of a cell 
 */

int int_cells_create(Stack stack, int rhs, int opt, int lhs)
{
  NspCells *C;
  int  m1,n1,i;
  CheckRhs(2,1000);
  CheckLhs(1,1);
  if (GetScalarInt(stack,1,&m1) == FAIL) return RET_BUG;
  if (GetScalarInt(stack,2,&n1) == FAIL) return RET_BUG;
  if ( (C =nsp_cells_create(NVOID,m1,n1)) == NULLCELLS ) return RET_BUG;
  if ( rhs - 2 > m1*n1 ) 
    {
      Scierror("Error: too many arguments given for cell creation\n");
      return RET_BUG;
    }
  for ( i = 3 ; i <= rhs ; i++) 
    {
      NspObject *O;
      if (( O =nsp_object_copy(nsp_get_object(stack,i))) == NULLOBJ ) return RET_BUG;
      if (nsp_object_set_name(O,"ce") == FAIL) return RET_BUG;
      C->objs[i-3]= O;
    }
  MoveObj(stack,1,(NspObject *) C);
  return 1;
}

/* int_col_cells_create 
 * build a cell matrix given each col element 
 */

int int_col_cells_create(Stack stack, int rhs, int opt, int lhs)
{
  NspCells *C;
  int  i;
  CheckRhs(0,1000);
  CheckLhs(1,1);
  if ( (C =nsp_cells_create(NVOID,1,rhs)) == NULLCELLS ) return RET_BUG;
  for ( i = 1 ; i <= rhs ; i++) 
    {
      if ( MaybeObjCopy(&NthObj(i)) == NULL)  return RET_BUG;
      if (nsp_object_set_name(NthObj(i),"ce") == FAIL) return RET_BUG;
      C->objs[i-1]= NthObj(i);
      /* If NthObj(i) is not copied it is inserted in the cells 
       * we must set then NthObj(i) to NULLOBJ 
       * to prevent the cleaning process to clean the object 
       * that we have inserted in our cell */
      NthObj(i) = NULLOBJ ;
    }
  MoveObj(stack,1,(NspObject *) C);
  return 1;
}

/* int_row_cells_create 
 * build a cell matrix given each row element as a Cell 
 * each given cell must be of the same size 
 * 
 */

int int_row_cells_create(Stack stack, int rhs, int opt, int lhs)
{
  NspCells *C;
  int i,cols;
  CheckRhs(0,1000);
  CheckLhs(1,1);
  if ( rhs == 1 ) 
    {
      NthObj(1)->ret_pos = 1;
      return 1;
    }
  /* first pass to check size and types */
  for ( i = 1 ; i <= rhs ; i++) 
    {
      NspCells *Ci= GetCells(stack,i);
      if ( Ci == NULLCELLS ) return RET_BUG;
      if ( i==1) cols = Ci->n;
      else 
	{
	  CheckCols(stack.fname,i,Ci,cols);
	  CheckRows(stack.fname,i,Ci,1);
	};
    }
  if ( (C =nsp_cells_create(NVOID,rhs,cols)) == NULLCELLS ) return RET_BUG;
  for ( i = 1 ; i <= rhs ; i++) 
    {
      int j;
      NspCells *Ci= GetCells(stack,i);
      NspObject *Ob = NSP_OBJECT(Ci);
      if ( MaybeObjCopy(&Ob) == NULL)  return RET_BUG;
      for ( j = 0 ; j < Ci->n ; j++) 
	{
	  C->objs[i-1+j*C->m]= Ci->objs[j];
	  Ci->objs[j]=NULLOBJ;
	}
      /* now Ci can be safely destroyed when returning */
    }
  MoveObj(stack,1,(NspObject *) C);
  return 1;
}

/* int_diag_cells_create 
 * build a diag cell matrix given each block as a Cell 
 */

int int_diag_cells_create(Stack stack, int rhs, int opt, int lhs)
{
  CheckRhs(0,1000);
  CheckLhs(1,1);
  if ( rhs == 1 ) 
    {
      NthObj(1)->ret_pos = 1;
      return 1;
    }
  else 
    {
      Scierror("%s: is to be done !\n",stack.fname);
      return RET_BUG;
    }
}


/*
 *nsp_cells_redim: Changes matrix dimensions
 * m*n must be unchanged 
 * The NspCells is changed (m,n are changed ) 
 * return 0 on failure 
 */

int int_cells_redim(Stack stack, int rhs, int opt, int lhs)
{
  int  m1,n1;
  NspCells  *HMat;
  CheckRhs(3,3);
  CheckLhs(1,1);
  if ( (HMat=GetCells(stack,1))== NULLCELLS) return RET_BUG;
  if ( GetScalarInt(stack,2,&m1) == FAIL) return RET_BUG;
  if ( GetScalarInt(stack,3,&n1) == FAIL) return RET_BUG;
  if ( nsp_cells_redim(HMat,m1,n1) != OK) return RET_BUG;
  NSP_OBJECT(HMat)->ret_pos = 1;
  return 1;
}

/*
 * changes a copy of matrix stack object to column vector 
 */

int
int_cells_mat2vect (Stack stack, int rhs, int opt, int lhs)
{
  NspCells *HMat;
  CheckRhs (1, 1);
  CheckLhs (1, 1);
  if ((HMat = GetCellsCopy (stack, 1)) == NULLCELLS) return RET_BUG;
  if (nsp_cells_redim (HMat, HMat->mn, 1) != OK) return RET_BUG;
  NSP_OBJECT (HMat)->ret_pos = 1;
  return 1;
}


/*
 * Right Concatenation 
 * A= [A,B] 
 * return 0 on failure ( incompatible size or No more space )
 */

int int_cells_concatr(Stack stack, int rhs, int opt, int lhs)
{
  NspCells *HMat1,*HMat2;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ((HMat1 = GetCells(stack,1))  == NULLCELLS) return RET_BUG;
  if ( HMat1->mn == 0)
    {
      NSP_OBJECT(NthObj(2))->ret_pos = 1;
      return 1;
    }
  if ((HMat2 = GetCells(stack,2)) == NULLCELLS) return RET_BUG;
  if ( HMat2->mn == 0)
    {
      NSP_OBJECT(HMat1)->ret_pos = 1;
      return 1;
    }
  else
    {
      if ((HMat1 = GetCellsCopy(stack,1))  == NULLCELLS) return RET_BUG;
      if (nsp_cells_concat_right(HMat1,HMat2)!= OK) return RET_BUG;
      NSP_OBJECT(HMat1)->ret_pos = 1;
      return 1;
    }
  return 1;
}

/*
 * Right Concatenation 
 * Res = [A,B]  when A is a scalar matrix 
 * usefull when A=[]
 */

/* FIXME: unfinished */
static NspCells *nsp_matrix_to_cells(NspMatrix *HMat1) { return NULLCELLS;}


int int_cells_concatr_m_ce(Stack stack, int rhs, int opt, int lhs)
{
  NspCells *HMat2,*Res;
  NspMatrix * HMat1;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ((HMat1 = GetMat(stack,1))  == NULLMAT) return RET_BUG;
  if ( HMat1->mn == 0)
    {
      NSP_OBJECT(NthObj(2))->ret_pos = 1;
      return 1;
    }
  if ((HMat2 = GetCells(stack,2)) == NULLCELLS) return RET_BUG;
  if (( Res=nsp_matrix_to_cells(HMat1))== NULLCELLS) return RET_BUG;

  if ( HMat2->mn != 0)
    {
      if (nsp_cells_concat_right(Res,HMat2)!= OK) return RET_BUG;
    }
  MoveObj(stack,1,(NspObject *) Res);
  return 1;
}

/*
 * Down Concatenation 
 * Res = [A;B] 
 * return NULLSMat on failure ( incompatible size or No more space )
 * A and B are left unchanged 
 */

typedef NspCells * (*FSconcat) (const NspCells *,const NspCells *);

int int_cells__concat(Stack stack, int rhs, int opt, int lhs, FSconcat F)
{
  NspCells *HMat1,*HMat2;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ((HMat1 = GetCells(stack,1))  == NULLCELLS) return RET_BUG;
  if ( HMat1->mn == 0)
    {
      NSP_OBJECT(NthObj(2))->ret_pos = 1;
      return 1;
    }
  if ((HMat2 = GetCells(stack,2)) == NULLCELLS) return RET_BUG;
  if ( HMat2->mn == 0)
    {
      NSP_OBJECT(HMat1)->ret_pos = 1;
      return 1;
    }
  else
    {
      NspCells *HMat3;
      if (( HMat3 = (*F)(HMat1,HMat2)) == NULLCELLS)  return RET_BUG;
      MoveObj(stack,1,(NspObject *) HMat3);
    }
  return 1;
}

int int_cells_concatd(Stack stack, int rhs, int opt, int lhs)
{
  return int_cells__concat(stack,rhs,opt,lhs,nsp_cells_concat_down);
}


/*
 * Down Concatenation 
 * Res = [A;B]  when A is a scalar matrix 
 * usefull when A=[]
 */

int int_cells_concatd_m_ce(Stack stack, int rhs, int opt, int lhs)
{
  NspCells *HMat2,*Res;
  NspMatrix * HMat1;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ((HMat1 = GetMat(stack,1))  == NULLMAT) return RET_BUG;
  if ( HMat1->mn == 0)
    {
      NSP_OBJECT(NthObj(2))->ret_pos = 1;
      return 1;
    }
  if ((HMat2 = GetCells(stack,2)) == NULLCELLS) return RET_BUG;
  if (( Res=nsp_matrix_to_cells(HMat1)) == NULLCELLS) return RET_BUG;

  if ( HMat2->mn != 0)
    {
      if ((Res=nsp_cells_concat_down(Res,HMat2))== NULLCELLS ) return RET_BUG;
    }
  MoveObj(stack,1,(NspObject *) Res);
  return 1;
}

/*
 * Diag Concatenation
 * Res = [A,0;0,B]
 * return NULLMAT on failure ( No more space )
 * A and B are left unchanged
 */

int int_cells_concatdiag(Stack stack, int rhs, int opt, int lhs)
{
  Sciprintf("smxconcatdiag: A FAIRE XXXX");
  /*
    return int_cells__concat(stack,rhs,opt,lhs,nsp_cells_concat_diag); */
  return 0;
}

/*
 *nsp_cells_add_columns: add n cols of zero to NspCells A 
 * A= [A,ones(m,n)] 
 * return 0 on failure ( No more space )
 * A is changed 
 */

int int_cells_addcols(Stack stack, int rhs, int opt, int lhs)
{
  int  n1;
  NspCells *HMat;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ( (HMat=GetCellsCopy(stack,1))== NULLCELLS) return RET_BUG;
  if ( GetScalarInt(stack,2,&n1) == FAIL) return RET_BUG;
  if ( nsp_cells_add_columns(HMat,n1) != OK) return RET_BUG;
  NSP_OBJECT(HMat)->ret_pos = 1;
  return 1;
}


/*
 * AddRows : Add m rows of zero to a NspCells A 
 * A = [A;ones(m,n)]
 * return NULLSMat on failure ( incompatible size or No more space )
 * A and B are left unchanged 
 */

int int_cells_addrows(Stack stack, int rhs, int opt, int lhs)
{
  int  m1;
  NspCells *HMat;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ( (HMat=GetCellsCopy(stack,1))== NULLCELLS) return RET_BUG;
  if ( GetScalarInt(stack,2,&m1) == FAIL) return RET_BUG;
  if ( nsp_cells_add_rows(HMat,m1) != OK) return RET_BUG; ;
  NSP_OBJECT(HMat)->ret_pos = 1;
  return 1;
}

/*
 *  A(Rows,Cols) = B 
 *  A is changed and enlarged if necessary 
 *  Size Compatibility is checked 
 *  WARNING : A is not Copied we want this routine to change A
 *            and the adress of A must not be changed 
 *  =======
 *  A can be a String Matrix 
 *  A(x)=B is not allowed when A and B do not have the same type 
 */

int int_cells_setrc(Stack stack, int rhs, int opt, int lhs)
{
  NspCells *A,*B;
  NspMatrix *Rows,*Cols=NULLMAT;
  CheckRhs(3,4);
  CheckLhs(1,1);
  if ( IsSMatObj(stack,1)  ) 
    {
      /* A is string matrix **/
      if ((A = GetCells(stack,1)) == NULLCELLS) return RET_BUG;
    }
  else 
    {
      Scierror("Error: A(...)= B, A and B must be of the same type\n");
      return RET_BUG;
    }
  if ( IsBMatObj(stack,2)  ) 
    {
      /* Rows is boolean : use find(Rows) **/
      NspBMatrix *BRows ;
      if ((BRows = GetBMat(stack,2)) == NULLBMAT) return RET_BUG;
      if ((Rows =nsp_bmatrix_find(BRows)) == NULLMAT) return RET_BUG;
    }
  else
    {
      /* Rows is a real matrix **/
      if ((Rows = GetRealMat(stack,2)) == NULLMAT) return RET_BUG;
    }
  if ( rhs == 4 )
    {
      /* Cols is boolean : use find(Cols) **/
      if ( IsBMatObj(stack,3)  ) 
	{
	  NspBMatrix *BCols ;
	  if ((BCols = GetBMat(stack,2)) == NULLBMAT) return RET_BUG;
	  if ((Cols =nsp_bmatrix_find(BCols)) == NULLMAT) return RET_BUG;
	}  
      else
	{
	  if ((Cols = GetRealMat(stack,3)) == NULLMAT ) return RET_BUG;
	}
    }
  /* last argument is B a String NspMatrix **/
  if ((B = GetCells(stack,rhs)) == NULLCELLS) return RET_BUG;
  if ( B == A) 
    {
      if ((B = GetCellsCopy(stack,rhs)) == NULLCELLS) return RET_BUG;
    }
  if ( rhs == 3 )
    {  if ( nsp_cells_set_rows( A, Rows,B) != OK) return RET_BUG; }
  else
    {  if ( nsp_cells_set_submatrix( A, Rows,Cols,B) != OK) return RET_BUG;}


  NSP_OBJECT(A)->ret_pos = 1;
  return 1;
}

/*
 * Res=SMatDeletecols(A,Cols)
 * Cols unchanged  ( restored at end of function if necessary)
 * WARNING : A must be changed by this routine
 */

int int_cells_deletecols(Stack stack, int rhs, int opt, int lhs)
{
  NspCells *A;
  NspMatrix *Cols;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ((A = GetCells(stack,1)) == NULLCELLS) return RET_BUG;
  if ((Cols = GetMat(stack,2)) == NULLMAT) return RET_BUG;
  if ( nsp_cells_delete_columns( A, Cols) < 0) return RET_BUG;
  NSP_OBJECT(A)->ret_pos = 1;
  return 1;
}



/*
 * Res=SMatDeleterows(A,Rows)
 *     Rows unchanged  ( restored at end of function if necessary)
 * WARNING : A must be changed by this routine
 */

int int_cells_deleterows(Stack stack, int rhs, int opt, int lhs)
{
  NspCells *A;
  NspMatrix *Rows;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ((A = GetCells(stack,1)) == NULLCELLS) return RET_BUG;
  if ((Rows = GetMat(stack,2)) == NULLMAT) return RET_BUG;
  if ( nsp_cells_delete_rows( A, Rows) < 0) return RET_BUG;
  NSP_OBJECT(A)->ret_pos = 1;
  return 1;
}

/*
 * Res=SMatDeleteelts(A,Elts)
 *     Elts unchanged  ( restored at end of function if necessary)
 * WARNING : A must be changed by this routine
 */

int int_cells_deleteelts(Stack stack, int rhs, int opt, int lhs)
{
  NspCells *A;
  NspMatrix *Elts;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ((A = GetCells(stack,1)) == NULLCELLS) return RET_BUG;
  if ((Elts = GetMat(stack,2)) == NULLMAT) return RET_BUG;
  if ( nsp_cells_delete_elements( A, Elts) < 0) return RET_BUG;
  NSP_OBJECT(A)->ret_pos = 1;
  return 1;
}

/*
 * Res=nsp_cells_extract(A,Rows,Cols)
 * A unchanged, Rows and Cols are changed (i.e converted to int) 
 * 
 */	

int int_cells_extract(Stack stack, int rhs, int opt, int lhs)
{
  NspCells *A,*Res; 
  NspMatrix *Rows,*Cols;
  CheckRhs(3,3);
  CheckLhs(1,1);
  if ((A = GetCells(stack,1)) == NULLCELLS) return RET_BUG;
  if ((Rows = GetMat(stack,2)) == NULLMAT) return RET_BUG;
  if ((Cols = GetMat(stack,3)) == NULLMAT) return RET_BUG;
  Res =nsp_cells_extract( A, Rows,Cols);
  if ( Res == NULLCELLS) return RET_BUG;
  MoveObj(stack,1,(NspObject *) Res);
  return 1;
}

/*
 * Res=nsp_matrix_extract_elements(Elts,A)
 * A unchanged, Elts
 */	

/* generic function for elts extraction */

typedef NspCells * (*extrf) (NspCells*M,NspMatrix *Elts,int *err);

int int_cells_extractelts_gen(Stack stack, int rhs, int opt, int lhs, extrf F)
{
  int err;
  NspCells *A,*Res;
  NspMatrix *Elts;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ((A = GetCells(stack,1)) == NULLCELLS) return RET_BUG;

  if ( IsBMatObj(stack,2)  ) 
    {
      /* Elts is boolean : use find(Elts) **/
      NspBMatrix *BElts;
      if ((BElts = GetBMat(stack,2)) == NULLBMAT) return RET_BUG;
      if ((Elts =nsp_bmatrix_find(BElts)) == NULLMAT) return RET_BUG;
    }
  else
    {
      /* Elts is a real matrix  **/
      if ((Elts = GetRealMat(stack,2)) == NULLMAT) return RET_BUG;
    }

  Res = (*F)( A, Elts,&err);
  if ( err == 1) 
    {
      Scierror("Error:\tIndices out of bound\n");
      return RET_BUG;
    }
  if ( Res  == NULLCELLS) return RET_BUG;

  MoveObj(stack,1,(NspObject *)Res);
  return 1;
}

int int_cells_extractelts(Stack stack, int rhs, int opt, int lhs)
{
  return int_cells_extractelts_gen(stack,rhs,opt,lhs,nsp_cells_extract_elements);
}

/*
 * columns extraction  Cols A --> A(Cols)
 */	

int int_cells_extractcols(Stack stack, int rhs, int opt, int lhs)
{
  return int_cells_extractelts_gen(stack,rhs,opt,lhs,nsp_cells_extract_columns);
}

/*
 * rows extraction 					   
 */	

int int_cells_extractrows(Stack stack, int rhs, int opt, int lhs)
{
  return int_cells_extractelts_gen(stack,rhs,opt,lhs,nsp_cells_extract_rows);
}

/*
 * columns extraction for do loop
 * Cols A --> (Cols,A,Cols(A))
 */

int int_cells_extractcolforloop(Stack stack, int rhs, int opt, int lhs)
{
  NspCells *A,*Res;
  NspMatrix *Cols;
  int  err=0;
  CheckRhs(2,2);
  CheckLhs(3,3);
  if ((A = GetCells(stack,1)) == NULLCELLS) return RET_BUG;
  if ((Cols = GetMat(stack,2)) == NULLMAT) return RET_BUG;
  Res =nsp_cells_extract_columns( A,Cols,&err);
  if ( err == 1) return RET_ENDFOR;
  if ( Res == NULLCELLS) return RET_BUG;
  NthObj(3) = (NspObject *) Res;
  return 3;
}

/*
 *nsp_cells_resize: Changes NspCells dimensions
 * Warning : this routine only enlarges the array 
 * of the NspCells storage so as to contain mxn 
 * elements : the previous datas are not moved and 
 * occupy the first array cells 
 * The NspCells is changed 
 * return 0 on failure 
 */

int int_cells_resize(Stack stack, int rhs, int opt, int lhs)
{
  int  m1,n1;
  NspCells  *HMat;
  CheckRhs(3,3);
  CheckLhs(1,1);
  if ( (HMat=GetCellsCopy(stack,1))== NULLCELLS) return RET_BUG;
  if ( GetScalarInt(stack,2,&m1) == FAIL) return RET_BUG;
  if ( GetScalarInt(stack,3,&n1) == FAIL) return RET_BUG;
  if ( nsp_cells_resize(HMat,m1,n1) != OK) return RET_BUG;
  NSP_OBJECT(HMat)->ret_pos = 1;
  return 1;
}


/*
 *nsp_cells_enlarge(A,m,n) 
 *  changes A to B= [ A , 0; 0,0 ]  where 0 stands for "." strings
 *  in such a way that B (max(A->m,m)xmax(A->n,n));
 *  The result is stored in A 
 * WARNING : no copy 
 */

int int_cells_enlarge(Stack stack, int rhs, int opt, int lhs)
{
  NspCells *A;
  int  m1,n1;
  CheckRhs(3,3);
  CheckLhs(1,1);
  if ((A = GetCells(stack,1)) == NULLCELLS) return RET_BUG;
  if (GetScalarInt(stack,2,&m1) == FAIL) return RET_BUG;
  if (GetScalarInt(stack,3,&n1) == FAIL) return RET_BUG;
  if ( nsp_cells_enlarge(A,m1,n1)== FAIL)  return RET_BUG;
  NSP_OBJECT(A)->ret_pos = 1;
  return 1;
}

/*
 * Operation leading to Boolean result 
 */

/* A < B */ 

int int_cells_lt(Stack stack, int rhs, int opt, int lhs)
{
  NspCells *A,*B; NspBMatrix *Res;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ((A = GetCells(stack,1)) == NULLCELLS) return RET_BUG;
  if ((B = GetCells(stack,2)) == NULLCELLS) return RET_BUG;
  Res = CellsCompOp(A,B,"<");
  if ( Res == NULLBMAT) return RET_BUG;
  MoveObj(stack,1,(NspObject *)Res);
  return 1;
}

int int_cells_le(Stack stack, int rhs, int opt, int lhs)
{
  NspCells *A,*B; NspBMatrix *Res;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ((A = GetCells(stack,1)) == NULLCELLS) return RET_BUG;
  if ((B = GetCells(stack,2)) == NULLCELLS) return RET_BUG;
  Res = CellsCompOp(A,B,"<=");
  if ( Res == NULLBMAT) return RET_BUG;
  MoveObj(stack,1,(NspObject *)Res);
  return 1;
}

int int_cells_neq(Stack stack, int rhs, int opt, int lhs)
{
  NspCells *A,*B; NspBMatrix *Res;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ((A = GetCells(stack,1)) == NULLCELLS) return RET_BUG;
  if ((B = GetCells(stack,2)) == NULLCELLS) return RET_BUG;
  Res = CellsCompOp(A,B,"<>");
  if ( Res == NULLBMAT) return RET_BUG;
  MoveObj(stack,1,(NspObject *)Res);
  return 1;
}

int int_cells_eq(Stack stack, int rhs, int opt, int lhs)
{
  NspCells *A,*B; NspBMatrix *Res;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ((A = GetCells(stack,1)) == NULLCELLS) return RET_BUG;
  if ((B = GetCells(stack,2)) == NULLCELLS) return RET_BUG;
  Res = CellsCompOp(A,B,"==");
  if ( Res == NULLBMAT) return RET_BUG;
  MoveObj(stack,1,(NspObject *)Res);
  return 1;
}

int int_cells_gt(Stack stack, int rhs, int opt, int lhs)
{

  NspCells *A,*B; NspBMatrix *Res;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ((A = GetCells(stack,1)) == NULLCELLS) return RET_BUG;
  if ((B = GetCells(stack,2)) == NULLCELLS) return RET_BUG;
  Res = CellsCompOp(A,B,">");
  if ( Res == NULLBMAT) return RET_BUG;
  MoveObj(stack,1,(NspObject *)Res);
  return 1;
}


int int_cells_ge(Stack stack, int rhs, int opt, int lhs)
{
  NspCells *A,*B; NspBMatrix *Res;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ((A = GetCells(stack,1)) == NULLCELLS) return RET_BUG;
  if ((B = GetCells(stack,2)) == NULLCELLS) return RET_BUG;
  Res = CellsCompOp(A,B,">=");
  if ( Res == NULLBMAT) return RET_BUG;
  MoveObj(stack,1,(NspObject *)Res);
  return 1;
}

/*
 * Same but returns a unique boolean 
 */

static int int_cells_f_gen(Stack stack, int rhs, int opt, int lhs,char *op)
{
  int rep,err;
  NspCells *A,*B; NspObject *Res;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ((A = GetCells(stack,1)) == NULLCELLS) return RET_BUG;
  if ((B = GetCells(stack,2)) == NULLCELLS) return RET_BUG;
  rep = CellsFullComp(A,B,op,&err);
  if ( err == 1) 
    {
      Scierror("Error: operator %s , arguments with incompatible dimensions\n",op);
      return RET_BUG;
    }
  if ( rep == TRUE ) 
    {
      if (( Res =nsp_create_true_object(NVOID)) == NULLOBJ) return RET_BUG;
    }
  else 
    {
      if (( Res =nsp_create_false_object(NVOID)) == NULLOBJ) return RET_BUG;
    }
  MoveObj(stack,1,(NspObject *)Res);
  return 1;
}

int int_cells_flt(Stack stack, int rhs, int opt, int lhs)
{
  return int_cells_f_gen(stack,rhs,opt,lhs,"<");
}

int int_cells_fle(Stack stack, int rhs, int opt, int lhs)
{
  return int_cells_f_gen(stack,rhs,opt,lhs,"<=");
}


int int_cells_fneq(Stack stack, int rhs, int opt, int lhs)
{
  return int_cells_f_gen(stack,rhs,opt,lhs,"<>");
}

int int_cells_feq(Stack stack, int rhs, int opt, int lhs)
{
  return int_cells_f_gen(stack,rhs,opt,lhs,"==");
}

int int_cells_fgt(Stack stack, int rhs, int opt, int lhs)
{
  return int_cells_f_gen(stack,rhs,opt,lhs,">");
}

int int_cells_fge(Stack stack, int rhs, int opt, int lhs)
{
  return int_cells_f_gen(stack,rhs,opt,lhs,">=");
}



/*
 * Res =nsp_cells_transpose(A) 
 * Creates a Copy of NspCells A : A is not checked 
 */

int int_cells_transpose(Stack stack, int rhs, int opt, int lhs)
{
  NspCells *HMat1,*HMat2;
  CheckRhs(1,1);
  CheckLhs(1,1);
  if (( HMat1 = GetCells(stack,1)) == NULLCELLS) return RET_BUG;
  if (( HMat2 =nsp_cells_transpose(HMat1))  == NULLCELLS) return RET_BUG;
  MoveObj(stack,1,(NspObject *) HMat2);
  return 1;
}


/*
 * The Interface for basic matrices operation 
 */

static OpTab Cells_func[]={
  {"resize2vect_ce", int_cells_mat2vect},	
  {"extractcols_ce",int_cells_extractcols},	
  {"extractrows_ce",int_cells_extractrows},
  {"extractelts_ce",int_cells_extractelts},
  {"loopextract_m_ce",int_cells_extractcolforloop},
  {"deletecols_ce_m", int_cells_deletecols},
  {"deleterows_ce_m", int_cells_deleterows},
  {"deleteelts_ce_m", int_cells_deleteelts},
  {"cells_create",int_cells_create},
  {"col_cells_create", int_col_cells_create},
  {"row_cells_create", int_row_cells_create},
  {"diag_cells_create", int_row_cells_create},
  {"redim_ce",int_cells_redim},
  {"concatr_ce_ce",int_cells_concatr},
  {"concatr_m_ce",int_cells_concatr_m_ce},
  {"addcols_ce_m",int_cells_addcols},
  {"concatd_ce_ce",int_cells_concatd},
  {"concatd_m_ce",int_cells_concatd_m_ce},
  {"addrows_ce",int_cells_addrows},
  {"setrc_ce",int_cells_setrc},
  {"extract_ce",int_cells_extract},
  {"resize_ce",int_cells_resize},
  {"enlarge_ce", int_cells_enlarge },
  /** XXXX : comment regler le probleme de sort ? **/
  {"eq_ce_ce" ,  int_cells_eq },
  {"feq_ce_ce" ,  int_cells_feq },
  {"fge_ce_ce" ,  int_cells_fge },
  {"fgt_ce_ce" ,  int_cells_fgt },
  {"fle_ce_ce" ,  int_cells_fle },
  {"flt_ce_ce" ,  int_cells_flt },
  {"fneq_ce_ce" ,  int_cells_fneq },
  {"ge_ce_ce" ,  int_cells_ge },
  {"gt_ce_ce" ,  int_cells_gt },
  {"le_ce_ce" ,  int_cells_le },
  {"lt_ce_ce" ,  int_cells_lt },
  {"ne_ce_ce" ,  int_cells_neq },
  {"quote_ce", int_cells_transpose},
  {(char *) 0, NULL}
};

int Cells_Interf(int i, Stack stack, int rhs, int opt, int lhs)
{
  return (*(Cells_func[i].fonc))(stack,rhs,opt,lhs);
}

/* used to walk through the interface table 
    (for adding or removing functions) */

void Cells_Interf_Info(int i, char **fname, function (**f))
{
  *fname = Cells_func[i].name;
  *f = Cells_func[i].fonc;
}
