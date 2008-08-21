/* Nsp
 * Copyright (C) 1998-2008 Jean-Philippe Chancelier Enpc/Cermics
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
#include "nsp/matint.h"

/**
 * SECTION:cells
 * @title: arrays of #NspObject
 * @short_description: An object used to implement Matlab cells i.e arrays of objects
 * @see_also: 
 *
 * <para>
 * A #NspCells is used to represent arrays of nsp objects. 
 * It implement the matint interface which is used for generic matrices 
 * operations. When using the matint interface a #NspCells
 * can always be casted to a #NspSMatrix.
 * </para>
 **/


static NspObject *nsp_cells_path_extract(NspCells *C,int n, NspObject **Objs, int *copy);

/*
 * NspCells inherits from NspObject 
 */

int nsp_type_cells_id=0;
NspTypeCells *nsp_type_cells=NULL;
int nsp_type_cells_init();

NspTypeCells *new_type_cells(type_mode mode)
{
  NspTypeMatint *mati;/* interface */
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
  type->methods = cells_get_methods; 
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
  top->path_extract = (path_func *) nsp_cells_path_extract;        /* used for x(1)(2)(...) */  
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
  
  /*
   * Matrix implements Matint the matrix interface 
   * which is common to object that behaves like matrices.
   */

  mati = new_type_matint(T_DERIVED);
  mati->methods = matint_get_methods; 
  /* mati->redim = (matint_redim *) nsp_cells_redim; use default value */
  mati->resize = (matint_resize  *) nsp_cells_resize; 
  mati->free_elt = (matint_free_elt *) nsp_object_destroy;
  mati->elt_size = (matint_elt_size *) nsp_cells_elt_size ;
  mati->clone = (matint_clone *) nsp_cells_clone;
  mati->copy_elt = (matint_copy_elt *) nsp_object_copy; 
  mati->enlarge = (matint_enlarge *) nsp_cells_enlarge;
  mati->canonic =  nsp_matint_canonic;
  mati->copy_ind = nsp_matint_basic_copy_pointer;
  type->interface = (NspTypeBase *) mati;

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

char *nsp_cells_type_short_string(NspObject *v)
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

/* used for evaluation of H(exp1) in exps like H(exp1)(exp2)....(expn)= val 
 * note that H(exp1)= val          -> setrowscols
 *       and H(exp1)(.....) = val  -> pathextract(H,exp1) and then 
 *       iterate on the result 
 * FIXME: this is a first try we just assume here that we
 *        have just one argument nsp_cells_path_extract 
 *        is to be changed. since it can be used with more that 
 *        on elements. 
 */

#define RETURN_CELL_ELT(ival)  if ( C->objs[ival] == NULLOBJ )	\
    {									\
      if ((C->objs[ival]=nsp_create_empty_matrix_object("ce"))== NULLOBJ) \
	return NULLOBJ;							\
    }									\
  return C->objs[ival];


static NspObject *nsp_cells_path_extract(NspCells *C,int n, NspObject **Objs,int *copy)
{
  int ival;
  *copy = FALSE;
  switch (n) 
    {
    case 1: 
      if ( IsMat(*Objs)  ) 
	{
	  if ( IntScalar(*Objs,&ival) == FAIL ) return NULLOBJ ;
	  if ( ival >= 1 && ival <= C->mn )
	    {
	      RETURN_CELL_ELT(ival-1);
	    }
	  else if ( ival >=1 )
	    {
	      switch (C->m) 
		{
		case 1 :
		case 0: 
		  if (nsp_cells_resize(C, 1,ival)== FAIL) 
		    return NULLOBJ;
		  RETURN_CELL_ELT(ival-1);
		  break;
		default: 
		  if ( C->n == 0 || C->n == 1 ) 
		    {
		      /* mx0 -> newx 1 */
		      if (nsp_cells_resize(C, ival,1)== FAIL) 
			return NULLOBJ;
		      RETURN_CELL_ELT(ival-1);
		    }
		  Scierror("Error: indice %d is out of bounds for affectation in %dx%d\n",ival,C->m,C->n);
		  return NULLOBJ;
		}
	    }
	  else 
	    {
	      Scierror("Error: indice is out of bounds for affectation\n");
	      return NULLOBJ;
	    }
	}
      break;
    case 2: 
      if ( IsMat(Objs[0]) && IsMat(Objs[1]))
	{
	  int row,col;
	  if ( IntScalar(Objs[0],&row) == FAIL ) return NULLOBJ ;
	  if ( IntScalar(Objs[1],&col) == FAIL ) return NULLOBJ ;
	  if ( row < 1 || col < 1 ) 
	    {
	      Scierror("Error: indices {%d,%d} should be strictly positive\n",row,col);
	      return NULLOBJ;	      
	    }
	  if ( row <= C->m && col <= C->n) 
	    {
	      ival = (row-1) + C->m *(col-1);
	      RETURN_CELL_ELT(ival);
	    }
	  else 
	    {
	      if ( nsp_cells_enlarge(C,row,col)==FAIL) return NULLOBJ;
	      ival = (row-1) + C->m *(col-1);
	      RETURN_CELL_ELT(ival);
	    }
	}
      break;
    }
  return NULLOBJ;
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
 */

static int nsp_cells_xdr_save(XDR *xdrs, NspCells *M)
{
  int i,rep;
  if (nsp_xdr_save_i(xdrs,M->type->id) == FAIL) return FAIL;
  if (nsp_xdr_save_string(xdrs, NSP_OBJECT(M)->name) == FAIL) return FAIL;
  if (nsp_xdr_save_i(xdrs,M->m) == FAIL) return FAIL;
  if (nsp_xdr_save_i(xdrs,M->n) == FAIL) return FAIL;
  for ( i= 0 ; i < M->mn ; i++) 
    {
      if (M->objs[i]   == NULLOBJ)
	{
	  if ( nsp_xdr_save_c(xdrs,'N') == FAIL) return FAIL;
	}
      else 
	{
	  if ( nsp_xdr_save_c(xdrs,'Y') == FAIL) return FAIL;
	  rep = M->objs[i]->type->save(xdrs,M->objs[i]);
	  if ( rep == FAIL) return FAIL;
	}
    }
  return OK;
}

/*
 * Load a NspCells 
 */

static NspCells *nsp_cells_xdr_load(XDR *xdrs)
{
  int m,n,i;
  NspCells *M;
  static char name[NAME_MAXL];
  if (nsp_xdr_load_string(xdrs,name,NAME_MAXL) == FAIL) return NULLCELLS;
  if (nsp_xdr_load_i(xdrs,&m) == FAIL) return NULLCELLS;
  if (nsp_xdr_load_i(xdrs,&n) == FAIL) return NULLCELLS ;
  /* initial mxn matrix with unallocated elements **/
  if ( ( M =nsp_cells_create(name,m,n)) == NULLCELLS) return(NULLCELLS);
  /* allocate elements and store copies of A elements **/
  for ( i = 0 ; i < m*n ; i++ )
    {
      char c=EOF;
      NspObject *Ob;
      nsp_xdr_load_c(xdrs,&c);
      switch (c)
	{
	case 'Y':
	  Ob= nsp_object_xdr_load(xdrs);
	  if ( Ob == NULLOBJ ) return NULLCELLS;
	  M->objs[i] = Ob;
	  break;
	case 'N':
	  M->objs[i] = NULL;
	  break;
	default: 
	  Scierror("Error: cannot relad a cell object\n");
	  return NULLCELLS;
	}
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
  HOBJ_GET_OBJECT(O,NULL);
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

int IsCells(const NspObject *O)
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

/*------------------------------------------------------
 * methods 
 *------------------------------------------------------*/


/*
 * get first object of a cell 
 * FIXME : should be replaced by {..}
 */

static int int_meth_cells_get(void *self,Stack stack, int rhs, int opt, int lhs)
{
  NspCells *C=self;
  NspObject *Ob;
  CheckRhs(0,0);
  CheckLhs(1,1);
  lhs=Max(lhs,1);
  if ( C->mn == 0 || C->objs[0] == NULL) 
    {
      Scierror("Cell elt not found\n");
      return RET_BUG;
    }
  if ((Ob= nsp_object_copy(C->objs[0]))==NULL)  return RET_BUG;
  NthObj(rhs+1) = Ob ;
  NSP_OBJECT(Ob)->ret_pos = 1;
  return 1;
}


static int int_meth_cells_has(void *self, Stack stack, int rhs, int opt, int lhs)
{
  NspCells *C=self;
  NspObject *Obj;
  NspBMatrix *B=NULLBMAT;
  NspMatrix *Ind=NULLMAT,*Ind2=NULLMAT;
  int ind;
  
  CheckRhs(1,1); 
  CheckLhs(1,3);

  if ( (Obj = nsp_get_object(stack, 1)) == NULLOBJ )
    return RET_BUG;

  if ( (B = nsp_bmatrix_create(NVOID,1,1)) == NULLBMAT )
    return RET_BUG;
  B->B[0] = nsp_cells_has(C, Obj, &ind);

  if ( lhs >= 2 )
    {
      if ( (Ind = nsp_matrix_create(NVOID,'r',1,1)) == NULLMAT )
	goto err;
      
      if ( lhs == 3 )
	{
	  if ( (Ind2 = nsp_matrix_create(NVOID,'r',1,1)) == NULLMAT )
	    goto err;
	  if ( ind == 0 )
	    { Ind->R[0] = 0.0; Ind2->R[0] = 0.0; }
	  else
	    { Ind->R[0] = (ind-1)%C->m + 1; Ind2->R[0] = (ind-1)/C->m + 1; }
	}
      else
	Ind->R[0] = ind;
    }

  MoveObj(stack,1,NSP_OBJECT(B));
  if ( lhs >= 2 )
    {
      MoveObj(stack,2,NSP_OBJECT(Ind));
      if ( lhs == 3 )
	MoveObj(stack,3,NSP_OBJECT(Ind2));
    }

  return Max(lhs,1);

 err:
  nsp_bmatrix_destroy(B);
  nsp_matrix_destroy(Ind);
  nsp_matrix_destroy(Ind2);
  return RET_BUG;
}


static NspMethods cells_methods[] = {
  { "get", int_meth_cells_get},
  { "has", int_meth_cells_has},
  { (char *) 0, NULL}
};

static NspMethods *cells_get_methods(void) { return cells_methods;};

/*----------------------------------------------------
 * Interface 
 * i.e a set of function which are accessible at nsp level
 *----------------------------------------------------*/

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
 * build a cell matrix given each col element {..,...,...} 
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
 * {..;...;...}
 */

int int_row_cells_create(Stack stack, int rhs, int opt, int lhs)
{
  NspCells *C;
  int i,cols=1;
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
	  CheckCols(NspFname(stack),i,Ci,cols);
	  CheckRows(NspFname(stack),i,Ci,1);
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
 * {...#....}
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
      Scierror("%s: is to be done !\n",NspFname(stack));
      return RET_BUG;
    }
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
  return int_matint_concat_down(stack,rhs,opt,lhs,(Fconcat_d)nsp_matint_concat_down);
  /* return int_cells__concat(stack,rhs,opt,lhs,nsp_cells_concat_down); */
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
 * Returns the kthe diag of a NspSMatrix 
 */

static int int_cells_diage(Stack stack, int rhs, int opt, int lhs)
{
  int k1;
  NspCells *A,*Res;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ( GetScalarInt(stack,2,&k1) == FAIL) return RET_BUG;
  if ((A = GetCells(stack,1)) == NULLCELLS) return RET_BUG;
  Res =nsp_cells_extract_diag( A,k1);
  if ( Res == NULLCELLS)  return RET_BUG;
  MoveObj(stack,1,(NspObject *) Res);
  return 1;
}



/*
 *  Creates a NspCells with kth diag set to Diag 
 */

static int int_cells_diagcre(Stack stack, int rhs, int opt, int lhs)
{
  int k1=0;
  NspCells *Diag,*Res;
  CheckRhs(1,2);
  CheckLhs(1,1);
  if ((Diag = GetCells(stack,1)) == NULLCELLS) return RET_BUG;
  if ( rhs == 2 ) 
    {
      if ( GetScalarInt(stack,2,&k1) == FAIL) return RET_BUG;
    }
  if ( (Res =nsp_cells_create_diag(Diag,k1)) == NULLCELLS ) return RET_BUG ;
  MoveObj(stack,1,(NspObject *) Res);
  return 1;
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
 * Res =ce2m(C,indice=i,notm=val1,noti=val2) 
 * convert a cell matrix to a scalar matrix 
 * if C{j} is not a matrix val1 is inserted 
 * if C(j) is a matrix and indice i does not exists then val2 is inserted
 */

int int_ce2m(Stack stack, int rhs, int opt, int lhs)
{
  double d=0.0, nan = d/d, noti = nan, notm = nan;
  doubleC Cnoti;
  int indice = 1,i;
  NspCells *C;
  NspMatrix *Res;
  int_types T[] = {obj_check,new_opts, t_end} ;

  nsp_option opts[] ={{ "indice",s_int,NULLOBJ,-1},
		      { "noti",s_double,NULLOBJ,-1},
		      { "notm",s_double,NULLOBJ,-1},
		      { NULL,t_end,NULLOBJ,-1}};

  if ( GetArgs(stack,rhs,opt,T,&nsp_type_cells,&C,&opts,&indice,&noti,&notm) == FAIL) return RET_BUG;
  Cnoti.r = noti; Cnoti.i=noti;
  if ((Res = nsp_matrix_create(NVOID,'r',C->m,C->n))== NULLMAT) return RET_BUG;
  for (i= 0 ; i < Res->mn ; i++) 
    {
      if ( IsMat(C->objs[i]) )
	{
	  NspMatrix *M= (NspMatrix *) C->objs[i];
	  if ( M->rc_type == 'c' ) 
	    {
	      if ( Res->rc_type == 'r' )
		{
		  if (nsp_mat_complexify(Res,0.00) == FAIL ) return RET_BUG;
		}
	    }
	  if ( Res->rc_type == 'r')
	    {
	      Res->R[i] = ( indice > 0 && indice <= M->mn ) ?  M->R[indice-1]: noti;
	    }
	  else 
	    {
	      Res->C[i] = ( indice > 0 && indice <= M->mn ) ?  M->C[indice-1]: Cnoti;
	    }
	}
      else 
	{
	  Res->R[i]= notm;
	}
    }
  MoveObj(stack,1,(NspObject *) Res);
  return 1;
}


/*
 * push the cells elements on the stack 
 */

static int int_cells_to_seq (Stack stack, int rhs, int opt, int lhs)
{
  int i,count=0;
  NspCells *C;
  CheckRhs (1, 1);
  if ((C = GetCellsCopy (stack, 1)) == NULLCELLS) return RET_BUG;
  for ( i=1 ; i <= C->mn ; i++)
    {
      if ( C->objs[i-1] != NULLOBJ) 
	{
	  nsp_object_set_name(C->objs[i-1],NVOID);
	  NthObj(count+1) = C->objs[i-1];
	  NthObj(count+1)->ret_pos = i;


	  C->objs[i-1]= NULLOBJ;
	  count++;
	}
    }
  /* C can be safely destroyed since each element 
   * has been set to NULLOBJ
   */
  nsp_cells_destroy(C); 
  if ( count == 0 )
    {
      /* remove C from stack */
      NthObj(1)=NULLOBJ;
    }
  return count;
}

/*
 * set cells elements 
 * C{exps}=(....)
 * or C{exp,exp,..}=(...)
 *   FIXME: work in progress should be changed 
 *   since functions in cells should be used 
 */

static int int_cells_setrowscols(Stack stack, int rhs, int opt, int lhs)
{
  return int_matint_cells_setrowscols(stack, rhs, opt, lhs);
}

/* unique for cells
 *
 */
static int int_cells_unique(Stack stack, int rhs, int opt, int lhs)
{
  NspCells *C, *CC;
  NspMatrix *ind=NULLMAT, *occ=NULLMAT, **hind=NULL, **hocc=NULL;
  CheckRhs(1,1);
  CheckLhs(1,3);

  if ( (C = GetCells(stack,1)) == NULLCELLS ) return RET_BUG;
  if ( lhs >= 2 ) hind = &ind;
  if ( lhs == 3 ) hocc = &occ;
      
  if ( (CC = nsp_cells_unique(C, hind, hocc)) == NULLCELLS ) return RET_BUG;
  MoveObj(stack,1,NSP_OBJECT(CC));
  if ( lhs >= 2 )
    MoveObj(stack,2,NSP_OBJECT(ind));
  if ( lhs == 3 )
    MoveObj(stack,3,NSP_OBJECT(occ));
  return Max(lhs,1);
}


/*
 * map function 
 */

static int int_cells_map(Stack stack, int rhs, int opt, int lhs)
{
  NspPList *PL;
  NspList *args = NULLLIST ; 
  NspCells *L;
  CheckRhs(2,3);
  CheckLhs(-1,1);
  if ((L = GetCells(stack,1)) == NULLCELLS ) return RET_BUG;
  if ((PL = GetNspPList(stack,2)) == NULLP_PLIST ) return RET_BUG;
  if ( rhs == 3 ) 
    {
      if ((args = GetList(stack,3)) == NULLLIST ) return RET_BUG;
    }
  if ( ( L=nsp_cells_map(L,PL,args)) == NULLCELLS ) return RET_BUG;
  MoveObj(stack,1,(NspObject *) L);
  return 1;      
}

/*
 * The Interface for basic matrices operation 
 */

static OpTab Cells_func[]={
  {"extract_ce", int_matint_extract}, 
  {"extractelts_ce", int_matint_extractelts}, 
  {"extractcols_ce", int_matint_extractcols}, 
  {"extractrows_ce", int_matint_extractrows_pointer}, 
  {"resize2vect_ce", int_matint_resize2vect},
  {"setrowscols_ce", int_matint_setrowscols},
  {"deleteelts_ce", int_matint_deleteelts},
  {"deleterows_ce", int_matint_deleterows},
  {"deletecols_ce", int_matint_deletecols},
  {"tozero_ce", int_matint_tozero},
  {"repmat_ce", int_matint_repmat},
  /* {"loopextract_m_ce",int_cells_extractcolforloop}, */
  {"cells_create",int_cells_create},
  {"cell",int_cells_create},   /* matlab name (note that cells is also the matlab name but considered obsolete)*/
  {"col_cells_create", int_col_cells_create},
  {"row_cells_create", int_row_cells_create},
  {"diag_cells_create", int_row_cells_create},
  {"redim_ce",int_matint_redim},
  {"matrix_ce", int_matint_redim},
  {"concatr_ce_ce", int_matint_concatr}, /* int_cells_concatr}, */
  {"concatr_m_ce",int_cells_concatr_m_ce},
  {"addcols_ce_m",int_cells_addcols},
  {"concatd_ce_ce", int_matint_concatd}, /* int_cells_concatd}, */
  {"concatd_m_ce",int_cells_concatd_m_ce},
  {"concatdiag_ce_ce",int_matint_concat_diag},
  {"isvector_ce", int_matint_isvector},
  {"addrows_ce",int_cells_addrows},
  {"resize_ce",int_cells_resize},
  {"enlarge_ce", int_cells_enlarge },
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
  {"ce2m",int_ce2m},
  {"object2seq_ce",int_cells_to_seq}, /* C{...} : extract +object2seq */
  {"cells_setrowscols_ce",int_cells_setrowscols}, /* C{..}=(....) */
  {"setrowscols_ce", int_matint_setrowscols}, /* still used in EvalEqual2 : pb in the test in EvalEqual */
  {"unique_ce" ,  int_cells_unique },
  {"map_ce", int_cells_map},
  {"diagcre_ce",int_cells_diagcre},
  {"diage_ce",int_cells_diage},
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
