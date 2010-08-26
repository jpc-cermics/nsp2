/* Nsp
 * Copyright (C) 2009-2010 Jean-Philippe Chancelier Enpc/Cermics
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

/* Uses a 1x1 SMatrix as a buffer for input/output
 */

#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#define SMio_Private 
#include <nsp/object.h> 
#include <nsp/matrix.h> 
#include <nsp/smatrix.h> 
#include <nsp/smio.h> 
#include <nsp/file.h>
#include <nsp/type.h>
#include <nsp/hobj.h>

#include "nsp/sciio.h" 
#include "nsp/pr-output.h" 
#include "nsp/interf.h"
#include "nsp/matutil.h"
#include "nsp/stack.h"
#include "../system/files.h" /* FSIZE */

/**
 * SECTION: string stream 
 * @title: SMio
 * @short_description: An object used to read/write in a string stream.
 * @see_also: 
 *
 * <para>
 * A #SMio is used to store information on string streams opened for read/write 
 * access. 
 * </para>
 **/

/*
 * NspSMio inherits from NspObject 
 */

int nsp_type_smio_id=0;
NspTypeSMio *nsp_type_smio=NULL;

NspTypeSMio *new_type_smio(type_mode mode)
{
  NspTypeSMio *type = NULL;
  NspTypeObject *top;
  if ( nsp_type_smio != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_smio;
    }
  if (( type =  malloc(sizeof(NspTypeSMio)))== NULL) return NULL;
  type->interface = NULL;
  type->surtype =(NspTypeBase *) new_type_object(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = NULL ;/* file_attrs ; */
  type->get_attrs = (attrs_func *) int_get_attribute; 
  type->set_attrs = (attrs_func *) int_set_attribute; 
  type->methods =nsp_smio_get_methods; 
  type->new = (new_func *)nsp_new_smio;

  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for file */ 

  top->pr = (print_func *)nsp_smio_print;	              /* printing */   
  top->dealloc = (dealloc_func *)nsp_smio_destroy;           /* dealloc */  
  top->copy  =  (copy_func *)nsp_smio_copy;                  /* copy object */  
  top->size  = (size_func *)nsp_smio_size;                   /* m,n or m*n  */  
  top->s_type =  (s_type_func *)nsp_smio_type_as_string;               /* type as a String */  
  top->sh_type = (sh_type_func *)nsp_smio_type_short_string;           /* type as a short string */  
  top->info = (info_func *)nsp_smio_info;                    /* info */  
  /* top->is_true = (is_true_func  *) file_IsTrue;  */         /* check if object can be considered as true */  
  /*top->loop =(loop_func *) file_LoopExtract;     */	/* for loops */  
  top->path_extract =  NULL;					/* used for x(1)(2)(...) */  
  top->get_from_obj = (get_from_obj_func *)nsp_smio_object;    	/* get object stored in SciObj */  
  top->eq  = (eq_func *)nsp_smio_eq;			/* equality check */  
  top->neq  = (eq_func *)nsp_smio_neq;		        /* non-equality check */

  top->save  = (save_func *)nsp_smio_xdr_save;
  top->load  = (load_func *)nsp_smio_xdr_load;
  /* top->latex = (print_func *) nsp_smio_latex_print; */
  top->full_copy  =  (copy_func *)nsp_smio_full_copy;                   /* copy object */  


  /* specific methods for file */
  type->init = (init_func *)nsp_init_smio;

  /* 
   * interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  
  if ( nsp_type_smio_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeMatrix called nsp_type_smio
       */
      type->id =  nsp_type_smio_id = nsp_new_type_id();
      nsp_type_smio = type;
      if ( nsp_register_type(nsp_type_smio) == FALSE) return NULL;
      return ( mode == T_BASE ) ? type : new_type_smio(mode);
    }
  else 
    {
      type->id = nsp_type_smio_id;
      return type;
    }
}
/*
 * initialize Scifile instances 
 * locally and by calling initializer on parent class 
 */

static int nsp_init_smio(NspSMio *o,NspTypeSMio *type)
{
  /* to be done always */ 
  if ( type->surtype->init(&o->father,type->surtype) == FAIL) return FAIL;
  o->type = type; 
  NSP_OBJECT(o)->basetype = (NspTypeBase *)type;
  /* specific */
  return OK;
}

/*
 * new instance of SMio 
 */

NspSMio *nsp_new_smio() 
{
  NspSMio *loc; 
  /* type must exists */
  nsp_type_smio = new_type_smio(T_BASE);
  if ( (loc = malloc(sizeof(NspSMio)))== NULLSMIO) return loc;
  /* initialize object */
  if (nsp_init_smio(loc,nsp_type_smio) == FAIL) return NULLSMIO;
  loc->obj = NULL;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for file 
 *-----------------------------------------------*/

/*
 * size 
 */

static int nsp_smio_size(NspSMio  *H, int flag)
{
  return 0;
}

/*
 * type as string 
 */

static char nsp_smio_type_name[]="SMio";
static char nsp_smio_short_type_name[]="smio";

static char *nsp_smio_type_as_string(void)
{
  return(nsp_smio_type_name);
}

static char *nsp_smio_type_short_string(NspObject *v)
{
  return(nsp_smio_short_type_name);
}

static int nsp_smio_eq(NspObject *A, NspObject *B)
{
  if ( check_cast(B,nsp_type_smio_id) == FALSE) return FALSE ;
  if ( ((NspSMio *) A)->obj == ((NspSMio *) B)->obj ) return TRUE ;
  return FALSE;
}

static int nsp_smio_neq(NspObject *A, NspObject *B)
{
  return nsp_smio_eq(A,B)== TRUE ? FALSE : TRUE ;
}

/*
 * delete 
 */

void nsp_smio_destroy(NspSMio  *F)
{
  nsp_object_destroy_name(NSP_OBJECT(F));
  F->obj->ref_count--;
  if ( F->obj->ref_count == 0 )
   {
     FREE(F->obj->D);
     FREE(F->obj);
   }
  FREE(F);
}

/*
 * info 
 */

int nsp_smio_info(NspSMio  *F, int indent,char *name,int rec_level)
{
  const char *pname;
  if (F == NULLSMIO) 
    {
      Sciprintf("Null Pointer SMio \n");
      return TRUE;
    }
  pname = (name != NULL) ? name : NSP_OBJECT(F)->name;
  Sciprintf1(indent,"%s\t= \t\tSMio (len=\"%d\",%s,Flag=%d)\n",
	     (pname==NULL) ? "" : pname,
	     F->obj->len,F->obj->openf,F->obj->flag);
  return TRUE;
}


/*
 * print 
 */


int nsp_smio_print(NspSMio  *F, int indent,char *name, int rec_level)
{
  nsp_smio_info(F,indent,name,rec_level);
  return TRUE;
}


/*
 * Save
 */

int nsp_smio_xdr_save(XDR *xdrs, NspSMio *S)
{
  if (nsp_xdr_save_i(xdrs,nsp_dynamic_id) == FAIL) return FAIL;
  if (nsp_xdr_save_string(xdrs,type_get_name(nsp_type_smio)) == FAIL) return FAIL;
  if (nsp_xdr_save_string(xdrs, NSP_OBJECT(S)->name) == FAIL) return FAIL;
  if (nsp_xdr_save_i(xdrs,S->obj->len) == FAIL) return FAIL;
  if (nsp_xdr_save_array_c(xdrs,S->obj->openf,4) == FAIL) return FAIL;
  if (nsp_xdr_save_i(xdrs,S->obj->flag) == FAIL) return FAIL;
  if (nsp_xdr_save_array_c(xdrs,S->obj->D,S->obj->len) == FAIL) return FAIL;
  return OK;
}

/*
 * Load
 */

NspSMio *nsp_smio_xdr_load(XDR *xdrs)
{
  int ilen;
  NspSMio *F;
  char openf[4];
  static char name[NAME_MAXL];
  if (nsp_xdr_load_string(xdrs,name,NAME_MAXL) == FAIL) return NULL;
  if (nsp_xdr_load_i(xdrs,&ilen) == FAIL) return NULL;
  if ((F= nsp_smio_wcreate(name,openf,0,ilen))== NULL) return NULL;
  if (nsp_xdr_load_array_c(xdrs,openf,4) == FAIL) return NULL;
  strncpy(F->obj->openf,openf,4);
  if (nsp_xdr_load_i(xdrs,&F->obj->flag) == FAIL) return NULL;
  if (nsp_xdr_load_array_c(xdrs,F->obj->D,F->obj->len) == FAIL) return NULL;
  F->obj->pos = 0;
  return F;
}



/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for Hash objects 
 * Note that some of these functions could become MACROS XXXXX 
 *-----------------------------------------------------*/

NspSMio  *nsp_smio_object(NspObject *O)
{
  /* Follow pointer **/
  HOBJ_GET_OBJECT(O,NULL);
  /* Check type **/
  if ( check_cast(O,nsp_type_smio_id) == TRUE) return ((NspSMio *) O);
  else 
    Scierror("Error:\tArgument should be a %s\n",type_get_name(nsp_type_smio));
  return(NULL);
}


int IsSMioObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i) , nsp_type_smio_id);
}

int IsSMio(NspObject *obj)
{
  return nsp_object_type(obj, nsp_type_smio_id);
}

NspSMio *GetSMioCopy(Stack stack, int i)
{
  if (  GetSMio(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspSMio *GetSMio(Stack stack, int i)
{
  NspSMio *M;
  if (( M =nsp_smio_object(NthObj(i))) == NULLSMIO  )
    ArgMessage(stack,i);
  return M;
}

/*-----------------------------------------------------
 * constructor 
 *-----------------------------------------------------*/

NspSMio *nsp_smio_create_void(char *name,NspTypeBase *type)
{
  NspSMio *F =nsp_new_smio() ;
  if ( F == NULLSMIO) 
    {
      Scierror("Error:\tRunning out of memory\n");
      return NULLSMIO;
    }
  if ( nsp_object_set_initial_name(NSP_OBJECT(F),name) == NULL)
    return NULLSMIO;
  NSP_OBJECT(F)->ret_pos = -1 ; 
  return F;
}


NspSMio *nsp_smio_wcreate(char *name,char *str,int flag,unsigned int len)
{
  NspSMio *F =nsp_smio_create_void(name,(NspTypeBase *) nsp_type_smio);
  if ( F == NULLSMIO)  return NULLSMIO;
  if ((F->obj = malloc(sizeof(nsp_smio))) == NULL) return NULLSMIO;
  F->obj->ref_count=1;
  if ((F->obj->D =new_nsp_string_n(len)) == NULLSTRING) return NULLSMIO;
  F->obj->len = len;
  memset(F->obj->D,EOF,F->obj->len);
  F->obj->pos = 0;
  strncpy(F->obj->openf,str,4);
  F->obj->flag =flag;
  return(F);
}

NspSMio *nsp_smio_rcreate(char *name,char *str,int flag,const char *data,unsigned int len)
{
  NspSMio *F =nsp_smio_create_void(name,(NspTypeBase *) nsp_type_smio);
  if ( F == NULLSMIO)  return NULLSMIO;
  if ((F->obj = malloc(sizeof(nsp_smio))) == NULL) return NULLSMIO;
  F->obj->ref_count=1;
  if ((F->obj->D = new_nsp_string_n(len)) == NULLSTRING) return NULLSMIO;
  memcpy(F->obj->D,data,len);
  F->obj->len = len;
  F->obj->pos = 0;
  strncpy(F->obj->openf,str,4);
  F->obj->flag =flag;
  return(F);
}


/*
 * copy 
 */

NspSMio *nsp_smio_copy(NspSMio  *self)
{
  NspSMio *F  = nsp_smio_create_void(NVOID,(NspTypeBase *) nsp_type_smio);
  if ( F ==  NULLSMIO) return NULLSMIO;
  F->obj = self->obj;
  self->obj->ref_count++;
  return F;
}

/*
 * full copy 
 */

NspSMio *nsp_smio_full_copy(NspSMio  *self)
{
  NspSMio *F = self;
  NspSMio *F1 = nsp_smio_rcreate(NVOID,F->obj->openf,F->obj->flag,F->obj->D,F->obj->len);
  return F1;
}

/*------------------------------------------------------
 * methods 
 *------------------------------------------------------*/

/*
 * f.close[]
 */

static int int_smio_fclose(void *self,Stack stack, int rhs, int opt, int lhs)
{
  CheckRhs(0,0);
  if (nsp_smio_close(self) == FAIL ) return RET_BUG;
  return 0;
}

/*
 * f.putstr(str)
 */

static int int_smio_putstr(void *self, Stack stack, int rhs, int opt, int lhs)
{
  char *str ;
  int_types T[] = {string, t_end} ;
  if ( GetArgs(stack,rhs,opt,T,&str) == FAIL) return RET_BUG;
  if ( nsp_smio_putstr(self,str) == FAIL) return RET_BUG; 
  return 0;
}


/*
 * f.put[Mat,type=], 
 */

static int int_smio_put(void *self, Stack stack, int rhs, int opt, int lhs)
{
  NspMatrix *M;
  char *type = NULL;
  int_types T[] = {realmat,new_opts, t_end} ;
  nsp_option opts[] ={{ "type",string,NULLOBJ,-1},
		      { NULL,t_end,NULLOBJ,-1}};
  if ( GetArgs(stack,rhs,opt,T,&M,&opts,&type) == FAIL) return RET_BUG;
  if ( type == NULL) type = "l";
  /* on line convert M->R */
  nsp_convert_double_to_type(M->R,M->mn,type);
  if ( nsp_smio_put(self,M->R,M->mn,type) == FAIL) return RET_BUG; 
  /* back convert */
  nsp_convert_type_to_double(M->R,M->mn,type);
  return 0;
}

/*
 * f.get[n=,type=], 
 */

static int int_smio_get( void *self,Stack stack, int rhs, int opt, int lhs)
{
  NspMatrix *M;
  char *type = NULL;
  int n=1,items;
  int_types T[] = {new_opts, t_end} ;
  nsp_option opts[] ={{ "n",s_int,NULLOBJ,-1},
		      { "type",string,NULLOBJ,-1},
		      { NULL,t_end,NULLOBJ,-1}};
  if ( GetArgs(stack,rhs,opt,T,&opts,&n,&type) == FAIL) return RET_BUG;
  if ( type == NULL) type = "l";
  if ( n < 0 || n==0 ) 
    {
      if (( M = nsp_matrix_create(NVOID,'r',0,0))== NULLMAT) return RET_BUG;
    }
  else
    {
      n=Max(n,1);
      if (( M = nsp_matrix_create(NVOID,'r',1,n))== NULLMAT) return RET_BUG;
      if ( nsp_smio_get(self,M->R,M->mn,type,&items) == FAIL) return RET_BUG; 
      if ( items != M->mn) nsp_matrix_resize(M,1,items);
      nsp_convert_type_to_double(M->R,M->mn,type);
    }
  MoveObj(stack,1,(NspObject *) M);
  return 1;
}

/*
 * F.getstr[n=];
 */

static int int_smio_getstr(void *self, Stack stack, int rhs, int opt, int lhs)
{
  NspSMatrix *M;
  int n=1,count;
  int_types T[] = {new_opts, t_end} ;
  nsp_option opts[] ={{ "n",s_int,NULLOBJ,-1},
		      { NULL,t_end,NULLOBJ,-1}};
  if ( GetArgs(stack,rhs,opt,T,&opts,&n) == FAIL) return RET_BUG;
  if (( M =nsp_smatrix_create_with_length(NVOID,1,1,-1))== NULLSMAT) return RET_BUG;
  if (( M->S[0] =new_nsp_string_n(n+1))== NULL) return RET_BUG;
  if ( nsp_smio_getstr(self,M->S[0],n,&count) == FAIL) return RET_BUG; 
  if ( count != n) 
    {
      char *str;
      if (( str =new_nsp_string_n(count+1))== NULL) return RET_BUG;
      strncpy(str,M->S[0],count);
      str[count]='\0';
      nsp_string_destroy(&(M->S[0]));
      M->S[0]=str;
    }
  MoveObj(stack,1,(NspObject *) M);
  return 1;
}

/*
 * LAB function : meof, 
 */

static int int_smio_eof(void *self, Stack stack, int rhs, int opt, int lhs)
{
  int rep;
  CheckRhs(0,0);
  CheckLhs(1,1);
  rep = nsp_smio_eof(self);
  if ( nsp_move_boolean(stack,1,rep) == FAIL) return RET_BUG;
  return 1;
}

/*
 * f.seek[] ou f.seek["pos"]; 
 */

static char *seek_Table[] = {"set", "cur", "end", NULL};

static int int_smio_seek(void *self, Stack stack, int rhs, int opt, int lhs)
{
  double offset;
  int rep = 0;
  CheckRhs(1,2);
  if (GetScalarDouble(stack,1,&offset) == FAIL) return RET_BUG;
  if ( rhs == 2 ) 
    {
      if ((rep= GetStringInArray(stack,2,seek_Table,1)) == -1) return RET_BUG; 
    }
  if ( nsp_smio_seek(self,(long int)offset,seek_Table[rep])== FAIL) return RET_BUG;
  return 0;
}

/*
 * f.tell[]
 */

static int int_smio_tell(void *self, Stack stack, int rhs, int opt, int lhs)
{
  long int offset;
  CheckRhs(0,0);
  CheckLhs(1,1);
  /*  checking variable file */
  if ( nsp_smio_tell(self,&offset) == FAIL) return RET_BUG;
  if ( nsp_move_double(stack,1,(double)offset )== FAIL) return RET_BUG;
  return 1;
}

/*
 * f.clearerr[]
 */

static int int_smio_clearerr(void *self, Stack stack, int rhs, int opt, int lhs)
{
  CheckRhs(0,0);
  CheckLhs(0,0);
  nsp_clearerr(self);
  return 0;
}

/*
 * f.error[]
 */

static int int_smio_error(void *self, Stack stack, int rhs, int opt, int lhs)
{
  CheckRhs(0,0);
  CheckLhs(1,1);
  if ( nsp_move_boolean(stack,1,nsp_ferror(self)) == FAIL) return RET_BUG;
  return 1;
}


/*
 * [s]= f.getlines[n]
 */

static int int_smio_get_lines(void *self, Stack stack, int rhs, int opt, int lhs)
{
  int lines; 
  NspSMatrix *S = NULL;
  int_types T[] = {s_int , t_end} ;
  if ( rhs >= 1 ) 
    {
      if ( GetArgs(stack,rhs,opt,T,&lines) == FAIL) return RET_BUG;
    }
  else
    {
      lines = 1;
    }
  lines=Max(1,lines);
  if ( nsp_smio_read_lines(self,&S,lines) == FAIL) return RET_BUG; 
  MoveObj(stack,1,(NspObject *) S);
  return 1;
}

/*
 * [m,s]= f.getmatrix[format=,]
 */

static int int_smio_get_matrix(void *self, Stack stack, int rhs, int opt, int lhs)
{
  NspMatrix *M = NULL;
  NspSMatrix *S = NULL;
  char *format=NULL;
  int_types T[] = {new_opts, t_end} ;
  nsp_option opts[] ={{ "format",string,NULLOBJ,-1},
		      { NULL,t_end,NULLOBJ,-1}};
  if ( GetArgs(stack,rhs,opt,T,&opts,&format) == FAIL) return RET_BUG;
  if ( nsp_smio_scanf_matrix(self,format,&M,(lhs==2),&S) == FAIL) return RET_BUG; 

  MoveObj(stack,1,(NspObject *) M);
  if ( lhs == 2 ) MoveObj(stack,2,(NspObject *) S);
  return Max(lhs,1);
}


/*
 * [m,s]= f.getsmatrix[]
 */

static int int_smio_get_smatrix(void *self, Stack stack, int rhs, int opt, int lhs)
{
  NspSMatrix *S = NULL;
  CheckRhs(0,0);
  if (  nsp_smio_scanf_smatrix(self,&S) == FAIL) return  RET_BUG; 
  MoveObj(stack,1,(NspObject *) S);
  return 1;
}


/*
 * put_matrix method 
 */

static int int_smio_put_matrix(void *self, Stack stack, int rhs, int opt, int lhs)
{
  NspMatrix *M = NULL;
  NspSMatrix *S = NULL;
  char *format=NULL;
  char *sep=NULL;
  int_types T[] = {realmat,new_opts, t_end} ;
  nsp_option opts[] ={{ "format",string,NULLOBJ,-1},
		      { "sep",string,NULLOBJ,-1},
		      { "title",smat,NULLOBJ,-1},
		      { NULL,t_end,NULLOBJ,-1}};
  if ( GetArgs(stack,rhs,opt,T,&M,&opts,&format,&sep,&S) == FAIL) return RET_BUG;
  if ( nsp_smio_printf_matrix(self,format,sep,M,S) == FAIL) return RET_BUG; 
  return 0;
}

/*
 * put_smatrix method 
 */

static int int_smio_put_smatrix(void *self, Stack stack, int rhs, int opt, int lhs)
{
  NspSMatrix *S = NULL;
  int_types T[] = {smat, t_end} ;
  if ( GetArgs(stack,rhs,opt,T,&S) == FAIL) return RET_BUG;
  if ( nsp_smio_printf_smatrix(self,S) == FAIL) return RET_BUG; 
  return 0;
}


/*
 * printf method
 */

static int int_smio_printf(void *self,Stack stack, int rhs, int opt, int lhs)
{
  char *str = NULL;
  int i=0,rows=0;
  NspSMio *F =self;
  char *Format;
  if ( rhs < 1 ) 
    { Scierror("Error:\tRhs must be >= 1\n",rhs);return RET_BUG;}
  CheckLhs(0,1);
  if ( !IS_OPENED(F->obj->flag))
    {
      Scierror("Error:\tsmio is not opened \n");
      return RET_BUG;
    }
  if ((Format = GetString(stack,1)) == (char*)0) return RET_BUG;
  if ( rhs >= 2 ) 
    {
      rows = print_count_rows(stack,2,rhs); 
      for ( i= 0 ; i < rows ; i++)
	{
	  if ( do_printf("printf",(FILE*)0,Format,stack,rhs,1,i,&str) < 0) return RET_BUG;
	  if ( nsp_smio_putstr(self,str) == FAIL)  return RET_BUG;
	}
      return 0;
    }
  else
    {
      if ( do_printf("printf",(FILE*)0,Format,stack,rhs,1,i,&str) < 0) return RET_BUG;
      if ( nsp_smio_putstr(self,str) == FAIL)  return RET_BUG;
    }
  return 0;
}  

/*
 * print method
 * i.e nsp display 
 * f.print[A ,options]  options= 'as_read' 
 * see:  int_object_print to see the changes which are to 
 * be done 
 * 
 */

static int int_smio_print(void *self,Stack stack, int rhs, int opt, int lhs)
{
  IOVFun def=NULL ;
  MoreFun mf=NULL; 
  NspSMatrix *res;
  NspObject  *object;
  print_func *pr;
  int dp=user_pref.pr_depth;
  int at=user_pref.list_as_tree;
  int cr=user_pref.color;
  int as_read=FALSE,latex=FALSE,table=FALSE,depth=INT_MAX,indent=0,tree=FALSE,color=TRUE;
  char *name = NULL;
  nsp_option print_opts[] ={{ "as_read",s_bool,NULLOBJ,-1},
			    { "color",s_bool,NULLOBJ,-1},
			    { "depth", s_int,NULLOBJ,-1},
			    { "indent",s_int,NULLOBJ,-1},
			    { "latex",s_bool,NULLOBJ,-1},
			    { "name",string,NULLOBJ,-1},
			    { "table",s_bool,NULLOBJ,-1},
			    { NULL,t_end,NULLOBJ,-1}};

  CheckStdRhs(1,1);
  if ((object =nsp_get_object(stack,1))== NULLOBJ) return RET_BUG; 
  CheckLhs(0,1);
  if ( get_optional_args(stack, rhs, opt, print_opts,&as_read,&color,&depth,
			 &indent,&latex,&name,&table) == FAIL) 
    return RET_BUG;
  def = SetScilabIO(Sciprint2string);
  mf =nsp_set_nsp_more(scimore_void);
  /* print object */
  user_pref.pr_depth= depth;
  user_pref.list_as_tree=tree;
  user_pref.color=color;
  pr = ( latex == TRUE) ?  object->type->latex :  object->type->pr ;
  if ( as_read == TRUE ) 
    {
      int kp=user_pref.pr_as_read_syntax;
      user_pref.pr_as_read_syntax= 1;
      if ( latex == TRUE ) 
	{
	  Sciprintf("Warning: you cannot select both as_read and latex, latex ignored\n");
	}
      pr(object,indent,name,0);
      user_pref.pr_as_read_syntax= kp;
      user_pref.pr_depth= dp;
      user_pref.list_as_tree=at;
      user_pref.color=cr;
    }
  else 
    {
      pr(object,indent,name,0);
    }
  user_pref.pr_depth= dp;
  user_pref.list_as_tree=at;
  user_pref.list_as_tree=cr;
  /* restore to default values */
  res = (NspSMatrix *)Sciprint2string_reset(); 
  SetScilabIO(def);
  nsp_set_nsp_more(mf);
  if ( res == NULL) return RET_BUG; 
  
  /* push res in NspSMio 
   *
   */
  if ( nsp_smio_printf_smatrix(self,res)== FAIL) 
    return RET_BUG;
  nsp_smatrix_destroy(res);
  return 0;
}


/* This function is not able to move the @pos field 
 * of the NspSMio object. 
 */

static int int_smio_scanf(void *self, Stack stack, int rhs, int opt, int lhs)
{
  int args,i,ret,iter=1,rep;
  char *Format;
  NspSMio *S = self;
  CheckRhs(1,1);
  if ((Format = GetString(stack,1)) == (char*)0) return RET_BUG;
  stack.first += 1;
  rep = do_scanf("sscanf",(FILE *)0,Format,stack,0,&args,S->obj->D + S->obj->pos,&ret);
  stack.first -= 1;
  if ( rep == FAIL ) return RET_BUG; 
  rep = Min(args,lhs);
  for ( i = 1 ; i <= rep ; i++) 
    {
      if ( NthObj(i+1) == NULLOBJ ) 
	{
	  Scierror("Error:\tRunning out of memory\n");
	  return RET_BUG;
	}
      NthObj(i+1)->ret_pos = i; 
    }
      
  /* first pass to give proper dimensions to matrices */
  for ( i = 1 ; i <= rep ; i++) 
    {
      NspObject *obj = NthObj(i+1);
      if ( IsMat(obj) )
	{
	  if ( nsp_matrix_resize((NspMatrix *)obj,iter,1) == FAIL) return RET_BUG;
	}
      else if ( IsSMat(obj)) 
	{
	  /* XXXX A FAIRE */
	}
    }
  /* read each line */
  for ( i= 1 ; i < iter ; i++) 
    {
      int rep1;
      stack.first += 1;
      rep1 = do_scanf("sscanf",(FILE *) 0,Format,stack,i,&args,S->obj->D + S->obj->pos,&ret);
      stack.first -= 1;
      if ( rep1 == FAIL ) return RET_BUG; 
    }
  return rep;
}  


/* returns a new #NspSMio which contains 
 * the compressed version of the given #NspSMio.
 * If an argument is given. It gives the number of bytes
 * to compress in the given #NspSMio
 * 
 */

static int int_smio_compress(void *self, Stack stack, int rhs, int opt, int lhs)
{
#ifdef HAVE_ZLIB
  unsigned long len;
  int rep,nb, nbc;
  NspSMio *S = self, *New;
  CheckStdRhs(0,1);
  if ( rhs - opt == 1) 
    {
      if ( GetScalarInt(stack,1,&nb) == FAIL) return RET_BUG;
      nb = Min(Max(0,nb),S->obj->len);
    }
  else 
    {
      nb = S->obj->len;
    }
  nbc  = nb*(1+0.1) + 12;
  if ( (New = nsp_smio_wcreate(NVOID,"rb",0, nbc)) == NULL ) 
    return RET_BUG;
  OPEN_ON(New->obj->flag);
  len = New->obj->len;
  rep = compress ((Bytef *) New->obj->D,&len,(Bytef *) S->obj->D,nb);
  if ( rep != Z_OK )
    {
      Scierror("Error: compression failed\n");
      return RET_BUG;
    }
  New->obj->len = len;
  MoveObj(stack,1,NSP_OBJECT(New));
  return 1;
#else 
  Scierror("Error: cannot compress with this nsp version\n");
  return RET_BUG;
#endif 

}

static int int_smio_can_compress(void *self, Stack stack, int rhs, int opt, int lhs)
{
  int rep;
  CheckStdRhs(0,0);
  CheckLhs(0,1);
#ifdef HAVE_ZLIB
  rep = TRUE; 
#else 
  rep = FALSE; 
#endif 
  nsp_move_boolean(stack,1,TRUE);
  return 1;
}

/* returns a new NspSMio which contains an uncompressed 
 * version of the NspSMio compressed argument. 
 * A first argument must be passed to give the size of 
 * the uncompressed buffer. This size can be smaller than 
 * the size required to uncompress the whole given objet.
 * This is used to decompress Matlab compressed mode, since 
 * the size required for the whole decompression is obtained 
 * by first getting the first uncompressed bytes and get size 
 * information from them.
 */



static int int_smio_uncompress(void *self, Stack stack, int rhs, int opt, int lhs)
{
#ifdef HAVE_ZLIB
  unsigned long int len;
  int rep, ilen;
  NspSMio *S = self, *New;
  CheckRhs(1,1);
  if ( GetScalarInt(stack,1,&ilen) == FAIL) return RET_BUG;
  len = ilen;
  if ( (New = nsp_smio_wcreate(NVOID,"rb",0,len)) == NULL ) 
    return RET_BUG;
  OPEN_ON(New->obj->flag);
  rep = uncompress ((Bytef *)New->obj->D,&len,(Bytef *) S->obj->D,S->obj->len);

  if ( rep ==  Z_DATA_ERROR ) 
    {
      Scierror("Error: uncompression failed, input data was corrupted\n");
      nsp_smio_destroy(New);
      return RET_BUG;
    }
  else if ( rep ==  Z_MEM_ERROR )
    {
      Scierror("Error: uncompression failed, not enough memory\n");
      nsp_smio_destroy(New);
      return RET_BUG;
    }
  /* we accept here partial decompression if len is not big enough 
   * i.e we do not stop on  Z_BUF_ERROR
   */
  New->obj->len = len;
  MoveObj(stack,1,NSP_OBJECT(New));
  return 1;
#else 
  Scierror("Error: cannot uncompress with this nsp version\n");
  return RET_BUG;
#endif 
}

static int int_smio_length(void *self, Stack stack, int rhs, int opt, int lhs)
{
  CheckRhs(0,0);
  if ( nsp_move_double(stack,1,((NspSMio *)self)->obj->len)== FAIL) 
    return RET_BUG;
  return 1;
}

static int int_smio_clear(void *self, Stack stack, int rhs, int opt, int lhs)
{
  NspSMio *S= self;
  memset(S->obj->D,EOF,S->obj->len);
  S->obj->pos=0;
  return 0;
}


static int int_smio_resize(void *self, Stack stack, int rhs, int opt, int lhs)
{
  int newlen;
  NspSMio *F= self;
  CheckRhs(1,1);
  if ( GetScalarInt(stack,1,&newlen) == FAIL) return RET_BUG;
  if ( newlen > F->obj->len ) 
    {
      char *loc; 
      if ((loc = realloc(F->obj->D, newlen )) == NULL)
	{							
	  Scierror("Error: cannot enlarge smio buffer\n");	
	  return RET_BUG;						
        }
      F->obj->D= loc;
      F->obj->len = newlen; 
      memset(F->obj->D+F->obj->pos,EOF, F->obj->len - F->obj->pos);
    }
  return 0;
}




static NspMethods nsp_smio_methods[] = {
  {"close", int_smio_fclose},
  {"putstr", int_smio_putstr},
  {"put", int_smio_put },
  {"get", int_smio_get },
  {"getstr", int_smio_getstr },
  {"eof", int_smio_eof},
  {"seek", int_smio_seek },
  {"tell", int_smio_tell },
  {"clearerr", int_smio_clearerr },
  {"error", int_smio_error },
  {"get_matrix",int_smio_get_matrix},
  {"get_lines", int_smio_get_lines}, 
  {"get_smatrix",int_smio_get_smatrix},
  {"put_matrix",int_smio_put_matrix}, 
  {"put_smatrix",int_smio_put_smatrix}, 
  {"print",int_smio_print},
  {"printf",int_smio_printf},
  {"scanf",int_smio_scanf},
  {"compress",int_smio_compress},
  {"uncompress",int_smio_uncompress},
  {"can_compress",int_smio_can_compress},
  {"length", int_smio_length},
  {"clear", int_smio_clear},
  {"resize", int_smio_resize},
  { (char *) 0, NULL}
};

static NspMethods *nsp_smio_get_methods(void) { return nsp_smio_methods;};

/*----------------------------------------------------
 * Interface 
 * i.e a set of function which are accessible at nsp level
 *----------------------------------------------------*/

/* interface:
 * f=fopen(len|str [,mode,xdr,swap])
 */

int int_smio_sopen(Stack stack, int rhs, int opt, int lhs)
{
  char *str=NULL;
  int xdr= FALSE,swap = TRUE, len=0;
  nsp_option opts[] ={{ "xdr",s_bool,NULLOBJ,-1},
		      { "swap",s_bool,NULLOBJ,-1},
		      { NULL,t_end,NULLOBJ,-1}};
  NspSMio *F;
  char *mode = NULL;
  CheckStdRhs(1,1);
  if ( IsMatObj(stack,1)) 
    {
      if ( GetScalarInt(stack,1,&len) == FAIL) return RET_BUG;
      mode = "wb";
    }
  else
    {
      if ((str = GetString (stack, 1)) == (char *) 0)
	return RET_BUG;
      mode = "rb";
    }
  if ( get_optional_args(stack,rhs,opt,opts,&xdr,&swap) == FAIL) 
    return RET_BUG;
  if ( mode[0] == 'w') 
    {
      if ((F=nsp_smio_wopen(mode,xdr,swap,len)) == NULLSMIO) return RET_BUG;
    }
  else
    {
      if ((F=nsp_smio_ropen(mode,xdr,swap,str,strlen(str))) == NULLSMIO) return RET_BUG;
    }
  MoveObj(stack,1,(NspObject *) F);
  return 1;
}

/* Interface table. 
 */

static OpTab SMio_func[]={
  {"sopen", int_smio_sopen},
  {(char *) 0, NULL}
};

int SMio_Interf(int i, Stack stack, int rhs, int opt, int lhs)
{
  return (*(SMio_func[i].fonc))(stack,rhs,opt,lhs);
}

/* used to walk through the interface table 
 *  (for adding or removing functions) 
 */

void SMio_Interf_Info(int i, char **fname, function (**f))
{
  *fname = SMio_func[i].name;
  *f = SMio_func[i].fonc;
}

