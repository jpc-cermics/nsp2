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

/* XXXXX : reste a regler les ecritures ds un fichier ouvert en lecture etc...
 */

#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define SciFile_Private 
#include "nsp/object.h"
#include "nsp/pr-output.h" 
#include "nsp/interf.h"
#include "nsp/matutil.h"
#include "nsp/stack.h"
#include "../system/files.h" /* FSIZE */

/* FIXME */
extern int nsp_fscanf_matrix(NspFile *F,char *fmt,NspMatrix **M,int flag,NspSMatrix **S); 
extern int nsp_fscanf_smatrix(NspFile *F,NspSMatrix **S) ;
extern int nsp_fprintf_matrix(NspFile *F,char *format,char *sep,NspMatrix *M,NspSMatrix *S);
extern int nsp_fprintf_smatrix(NspFile *F,NspSMatrix *S);

/**
 * SECTION:file 
 * @title: File streams.
 * @short_description: An object used to open and read/write in a file stream.
 * @see_also: 
 *
 * <para>
 * A #NspFile is used to store information on file streams opened for read/write 
 * access. This is a by reference object at nsp level. 
 * </para>
 **/


/*
 * NspFile inherits from NspObject 
 */

int nsp_type_file_id=0;
NspTypeSciFile *nsp_type_file=NULL;

NspTypeSciFile *new_type_file(type_mode mode)
{
  NspTypeSciFile *type = NULL;
  NspTypeObject *top;
  if ( nsp_type_file != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_file;
    }
  if (( type =  malloc(sizeof(NspTypeSciFile)))== NULL) return NULL;
  type->interface = NULL;
  type->surtype =(NspTypeBase *) new_type_object(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = NULL ;/* file_attrs ; */
  type->get_attrs = (attrs_func *) int_get_attribute; 
  type->set_attrs = (attrs_func *) int_set_attribute; 
  type->methods =nsp_file_get_methods; 
  type->new = (new_func *)nsp_new_file;

  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for file */ 

  top->pr = (print_func *)nsp_file_print;	              /* printing */   
  top->dealloc = (dealloc_func *)nsp_file_destroy;           /* dealloc */  
  top->copy  =  (copy_func *)nsp_file_copy;                  /* copy object */  
  top->size  = (size_func *)nsp_file_size;                   /* m,n or m*n  */  
  top->s_type =  (s_type_func *)nsp_file_type_as_string;               /* type as a String */  
  top->sh_type = (sh_type_func *)nsp_file_type_short_string;           /* type as a short string */  
  top->info = (info_func *)nsp_file_info;                    /* info */  
  /* top->is_true = (is_true_func  *) file_IsTrue;  */         /* check if object can be considered as true */  
  /*top->loop =(loop_func *) file_LoopExtract;     */	/* for loops */  
  top->path_extract =  NULL;					/* used for x(1)(2)(...) */  
  top->get_from_obj = (get_from_obj_func *)nsp_file_object;    	/* get object stored in SciObj */  
  top->eq  = (eq_func *)nsp_file_eq;			/* equality check */  
  top->neq  = (eq_func *)nsp_file_neq;		        /* non-equality check */

  /* specific methods for file */
  type->init = (init_func *)nsp_init_file;

  /* 
   * interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  
  if ( nsp_type_file_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeMatrix called nsp_type_file
       */
      type->id =  nsp_type_file_id = nsp_new_type_id();
      nsp_type_file = type;
      if ( nsp_register_type(nsp_type_file) == FALSE) return NULL;
      return ( mode == T_BASE ) ? type : new_type_file(mode);
    }
  else 
    {
      type->id = nsp_type_file_id;
      return type;
    }
}
/*
 * initialize Scifile instances 
 * locally and by calling initializer on parent class 
 */

static int nsp_init_file(NspFile *o,NspTypeSciFile *type)
{
  /* to be done always */ 
  if ( type->surtype->init(&o->father,type->surtype) == FAIL) return FAIL;
  o->type = type; 
  NSP_OBJECT(o)->basetype = (NspTypeBase *)type;
  /* specific */
  return OK;
}

/*
 * new instance of SciFile 
 */

NspFile *nsp_new_file() 
{
  NspFile *loc; 
  /* type must exists */
  nsp_type_file = new_type_file(T_BASE);
  if ( (loc = malloc(sizeof(NspFile)))== NULLSCIFILE) return loc;
  /* initialize object */
  if (nsp_init_file(loc,nsp_type_file) == FAIL) return NULLSCIFILE;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for file 
 *-----------------------------------------------*/

/*
 * size 
 */

static int nsp_file_size(NspFile  *H, int flag)
{
  return 0;
}

/*
 * type as string 
 */

static char nsp_file_type_name[]="File";
static char nsp_file_short_type_name[]="sf";

static char *nsp_file_type_as_string(void)
{
  return(nsp_file_type_name);
}

static char *nsp_file_type_short_string(void)
{
  return(nsp_file_short_type_name);
}


static int nsp_file_full_comp(NspFile * A,NspFile * B,char *op,int *err)
{
  Scierror("SciFileFullComp: to be implemented \n");
  return FALSE;
}

static int nsp_file_eq(NspObject *A, NspObject *B)
{
  int err,rep;
  if ( check_cast(B,nsp_type_ivect_id) == FALSE) return FALSE ;
  rep =nsp_file_full_comp((NspFile *) A,(NspFile *) B,"==",&err);
  if ( err == 1) return FALSE ; 
  return rep;
}

static int nsp_file_neq(NspObject *A, NspObject *B)
{
  int err=0,rep;
  if ( check_cast(B,nsp_type_ivect_id) == FALSE) return TRUE;
  rep =nsp_file_full_comp((NspFile *) A,(NspFile *) B,"<>",&err);
  if ( err == 1) return TRUE ; 
  return rep;
}

/*
 * delete 
 */

void nsp_file_destroy(NspFile  *F)
{
  if ( F != NULLSCIFILE )
    {
      /* XXXXXX : we must close file ? before destroying everything **/
      nsp_object_destroy_name(NSP_OBJECT(F));
      FREE(F->fname);
      FREE(F) ;
    };
}

/*
 * info 
 */

void nsp_file_info(NspFile  *F, int indent,char *name,int rec_level)
{
  const char *pname;
  if (F == NULLSCIFILE) 
    {
      Sciprintf("Null Pointer SciFile \n");
      return;
    }
  pname = (name != NULL) ? name : NSP_OBJECT(F)->name;
  Sciprintf1(indent,"%s\t= \t\tSciFile (fname=\"%s\",%s,Flag=%d)\n",
	     (pname==NULL) ? "" : pname,
	     F->fname,F->openf,F->flag);
}


/*
 * print 
 */


void nsp_file_print(NspFile  *F, int indent,char *name, int rec_level)
{
  nsp_file_info(F,indent,name,rec_level);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for Hash objects 
 * Note that some of these functions could become MACROS XXXXX 
 *-----------------------------------------------------*/

NspFile  *nsp_file_object(NspObject *O)
{
  /* Follow pointer **/
  HOBJ_GET_OBJECT(O,NULL);
  /* Check type **/
  if ( check_cast(O,nsp_type_file_id) == TRUE) return ((NspFile *) O);
  else 
    Scierror("Error:\tArgument should be a %s\n",type_get_name(nsp_type_file));
  return(NULL);
}


int IsSciFileObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i) , nsp_type_file_id);
}

int IsSciFile(NspObject *obj)
{
  return nsp_object_type(obj, nsp_type_file_id);
}

NspFile *GetSciFileCopy(Stack stack, int i)
{
  if (  GetSciFile(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspFile *GetSciFile(Stack stack, int i)
{
  NspFile *M;
  if (( M =nsp_file_object(NthObj(i))) == NULLSCIFILE  )
    ArgMessage(stack,i);
  return M;
}

/*-----------------------------------------------------
 * constructor 
 *-----------------------------------------------------*/

NspFile *nsp_file_create(char *name, char *fname, char *str,int flag,FILE *f)
{
  NspFile *F =nsp_new_file() ;
  if ( F == NULLSCIFILE) 
    {
      Scierror("Error:\tRunning out of memory\n");
      return NULLSCIFILE;
    }
  if ( nsp_object_set_initial_name(NSP_OBJECT(F),name) == NULL)
    return NULLSCIFILE;
  NSP_OBJECT(F)->ret_pos = -1 ; 
  if ((F->fname =new_nsp_string(fname)) == NULLSTRING) return NULLSCIFILE;
  strncpy(F->openf,str,4);
  F->flag =flag;
  F->file =f;
  return(F);
}

/*
 * copy 
 */

NspFile *nsp_file_copy(NspFile  *A)
{
  return (nsp_file_create(NVOID,A->fname,A->openf,A->flag,A->file));
}



/*------------------------------------------------------
 * methods 
 *------------------------------------------------------*/

/*
 * f.close[]
 */

static int int_file_fclose(void *self,Stack stack, int rhs, int opt, int lhs)
{
  CheckRhs(0,0);
  if (nsp_file_close(self) == FAIL ) return RET_BUG;
  return 0;
}

/*
 * f.putstr(str)
 */

static int int_file_putstr(void *self, Stack stack, int rhs, int opt, int lhs)
{
  char *str ;
  int_types T[] = {string, t_end} ;
  if ( GetArgs(stack,rhs,opt,T,&str) == FAIL) return RET_BUG;
  if ( nsp_putstr(self,str) == FAIL) return RET_BUG; 
  return 0;
}


/*
 * f.put[Mat,type=], 
 */

static int int_file_put(void *self, Stack stack, int rhs, int opt, int lhs)
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
  if ( nsp_mput(self,M->R,M->mn,type) == FAIL) return RET_BUG; 
  /* back convert */
  nsp_convert_type_to_double(M->R,M->mn,type);
  return 0;
}

/*
 * f.get[n=,type=], 
 */

static int int_file_get( void *self,Stack stack, int rhs, int opt, int lhs)
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
      if ( nsp_mget(self,M->R,M->mn,type,&items) == FAIL) return RET_BUG; 
      if ( items != M->mn) nsp_matrix_resize(M,1,items);
      nsp_convert_type_to_double(M->R,M->mn,type);
    }
  MoveObj(stack,1,(NspObject *) M);
  return 1;
}

/*
 * F.getstr[n=];
 */

static int int_file_getstr(void *self, Stack stack, int rhs, int opt, int lhs)
{
  NspSMatrix *M;
  int n=1,count;
  int_types T[] = {new_opts, t_end} ;
  nsp_option opts[] ={{ "n",s_int,NULLOBJ,-1},
		      { NULL,t_end,NULLOBJ,-1}};
  if ( GetArgs(stack,rhs,opt,T,&opts,&n) == FAIL) return RET_BUG;
  if (( M =nsp_smatrix_create_with_length(NVOID,1,1,-1))== NULLSMAT) return RET_BUG;
  if (( M->S[0] =new_nsp_string_n(n+1))== NULL) return RET_BUG;
  if ( nsp_mgetstr1(self,M->S[0],n,&count) == FAIL) return RET_BUG; 
  if ( count != n) 
    {
      char *str;
      if (( str =new_nsp_string_n(count+1))== NULL) return RET_BUG;
      strncpy(str,M->S[0],count);
      nsp_string_destroy(&(M->S[0]));
      M->S[0]=str;
    }
  MoveObj(stack,1,(NspObject *) M);
  return 1;
}

/*
 * SCILAB function : meof, 
 */

static int int_file_eof(void *self, Stack stack, int rhs, int opt, int lhs)
{
  int rep;
  CheckRhs(0,0);
  CheckLhs(1,1);
  rep = nsp_feof(self);
  if ( nsp_move_boolean(stack,1,rep) == FAIL) return RET_BUG;
  return 1;
}

/*
 * f.seek[] ou f.seek["pos"]; 
 */

static char *seek_Table[] = {"set", "cur", "end", NULL};

static int int_file_seek(void *self, Stack stack, int rhs, int opt, int lhs)
{
  double offset;
  int rep = 0;
  CheckRhs(1,2);
  if (GetScalarDouble(stack,1,&offset) == FAIL) return RET_BUG;
  if ( rhs == 2 ) 
    {
      if ((rep= GetStringInArray(stack,1,seek_Table,1)) == -1) return RET_BUG; 
    }
  if ( nsp_fseek(self,(long int)offset,seek_Table[rep])== FAIL) return RET_BUG;
  return 0;
}

/*
 * f.tell[]
 */

static int int_file_tell(void *self, Stack stack, int rhs, int opt, int lhs)
{
  long int offset;
  CheckRhs(0,0);
  CheckLhs(1,1);
  /*  checking variable file */
  if ( nsp_ftell(self,&offset) == FAIL) return RET_BUG;
  if ( nsp_move_double(stack,1,(double)offset )== FAIL) return RET_BUG;
  return 1;
}

/*
 * f.clearerr[]
 */

static int int_file_clearerr(void *self, Stack stack, int rhs, int opt, int lhs)
{
  CheckRhs(0,0);
  CheckLhs(0,0);
  nsp_clearerr(self);
  return 0;
}

/*
 * f.error[]
 */

static int int_file_error(void *self, Stack stack, int rhs, int opt, int lhs)
{
  CheckRhs(0,0);
  CheckLhs(1,1);
  if ( nsp_move_boolean(stack,1,nsp_ferror(self)) == FAIL) return RET_BUG;
  return 1;
}


/*
 * [s]= f.getlines[n]
 */

static int int_file_get_lines(void *self, Stack stack, int rhs, int opt, int lhs)
{
  int lines; 
  NspSMatrix *S = NULL;
  int_types T[] = {s_int , t_end} ;
  if ( rhs >= 1 ) 
    {
      if ( GetArgs(stack,rhs,opt,T,&lines) == FAIL) return RET_BUG;
    }
  else
    lines = 1;
  lines=Max(1,lines);
  if ( nsp_read_lines(self,&S,lines) == FAIL) return RET_BUG; 
  MoveObj(stack,1,(NspObject *) S);
  return 1;
}

/*
 * [m,s]= f.getmatrix[format=,]
 */

static int int_file_get_matrix(void *self, Stack stack, int rhs, int opt, int lhs)
{
  NspMatrix *M = NULL;
  NspSMatrix *S = NULL;
  char *format=NULL;
  int_types T[] = {new_opts, t_end} ;
  nsp_option opts[] ={{ "format",string,NULLOBJ,-1},
		      { NULL,t_end,NULLOBJ,-1}};
  if ( GetArgs(stack,rhs,opt,T,&opts,&format) == FAIL) return RET_BUG;
  if ( nsp_fscanf_matrix(self,format,&M,(lhs==2),&S) == FAIL) return RET_BUG; 

  MoveObj(stack,1,(NspObject *) M);
  if ( lhs == 2 ) MoveObj(stack,2,(NspObject *) S);
  return Max(lhs,1);
}


/*
 * [m,s]= f.getsmatrix[]
 */

static int int_file_get_smatrix(void *self, Stack stack, int rhs, int opt, int lhs)
{
  NspSMatrix *S = NULL;
  CheckRhs(0,0);
  if (  nsp_fscanf_smatrix(self,&S) == FAIL) return  RET_BUG; 
  MoveObj(stack,1,(NspObject *) S);
  return 1;
}


/*
 * put_matrix method 
 */

static int int_file_put_matrix(void *self, Stack stack, int rhs, int opt, int lhs)
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
  if ( nsp_fprintf_matrix(self,format,sep,M,S) == FAIL) return RET_BUG; 
  return 0;
}

/*
 * put_smatrix method 
 */

static int int_file_put_smatrix(void *self, Stack stack, int rhs, int opt, int lhs)
{
  NspSMatrix *S = NULL;
  int_types T[] = {smat, t_end} ;
  if ( GetArgs(stack,rhs,opt,T,&S) == FAIL) return RET_BUG;
  if ( nsp_fprintf_smatrix(self,S) == FAIL) return RET_BUG; 
  return 0;
}


/*
 * printf method
 */

static int int_file_printf(void *self,Stack stack, int rhs, int opt, int lhs)
{
  int i=0,rows=0;
  NspFile *F =self;
  char *Format;
  if ( rhs < 1 ) 
    { Scierror("Error:\tRhs must be >= 1\n",rhs);return RET_BUG;}
  CheckLhs(0,1);
  if ( !IS_OPENED(F->flag))
    {
      Scierror("Error:\tfile %s is not opened \n",F->fname);
      return RET_BUG;
    }
  if ((Format = GetString(stack,1)) == (char*)0) return RET_BUG;
  if ( rhs >= 2 ) 
    {
      rows = print_count_rows(stack,2,rhs); 
      for ( i= 0 ; i < rows ; i++)
	{
	  if ( do_printf("printf",F->file,Format,stack,rhs,1,i,(char **) 0) < 0) 
	    return RET_BUG;
	}
      return 0;
    }
  else
    {
      if ( do_printf("printf",F->file,Format,stack,rhs,1,i,(char **) 0) < 0) 
	return RET_BUG;
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

static int int_file_print(void *self,Stack stack, int rhs, int opt, int lhs)
{
  NspFile *F = self;
  FILE *f;
  IOVFun def ;
  MoreFun mf; 
  
  NspObject *object;
  print_func *pr;
  int dp=user_pref.pr_depth;
  int as_read=FALSE,latex=FALSE,table=FALSE,depth=LONG_MAX,indent=0;
  char *name = NULL;
  nsp_option opts[] ={{ "as_read",s_bool,NULLOBJ,-1},
		      { "depth", s_int,NULLOBJ,-1},
		      { "indent",s_int,NULLOBJ,-1},
		      { "latex",s_bool,NULLOBJ,-1},
		      { "name",string,NULLOBJ,-1},
		      { "table",s_bool,NULLOBJ,-1},
		      { NULL,t_end,NULLOBJ,-1}};

  CheckStdRhs(1,1);
  CheckLhs(0,1);
  if ((object =nsp_get_object(stack,1))== NULLOBJ) return RET_BUG; 
  if ( get_optional_args(stack, rhs, opt, opts,&as_read,&depth,
			 &indent,&latex,&name,&table) == FAIL) 
    return RET_BUG;
  /* changes io in order to write to file F */
  if ( !IS_OPENED(F->flag))
    {
      Scierror("Warning:\tfile %s is already closed\n",F->fname);
      return RET_BUG;
    }
  f=Sciprint_file(F->file); 
  def = SetScilabIO(Sciprint2file);
  mf =nsp_set_nsp_more(scimore_void);
  /* print object */
  user_pref.pr_depth= depth;
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
    }
  else 
    {
      pr(object,indent,name,0);
    }
  user_pref.pr_depth= dp;
  /* restore to default values */
  SetScilabIO(def);
  nsp_set_nsp_more(mf);
  Sciprint_file(f); 
  return 0;
}


static NspMethods nsp_file_methods[] = {
  {"close", int_file_fclose},
  {"putstr", int_file_putstr},
  {"put", int_file_put },
  {"get", int_file_get },
  {"getstr", int_file_getstr },
  {"eof", int_file_eof},
  {"seek", int_file_seek },
  {"tell", int_file_tell },
  {"clearerr", int_file_clearerr },
  {"error", int_file_error },
  {"get_matrix",int_file_get_matrix},
  {"get_lines", int_file_get_lines}, 
  {"get_smatrix",int_file_get_smatrix},
  {"put_matrix",int_file_put_matrix}, 
  {"put_smatrix",int_file_put_smatrix}, 
  {"print",int_file_print},
  {"printf",int_file_printf},
  { (char *) 0, NULL}
};

static NspMethods *nsp_file_get_methods(void) { return nsp_file_methods;};

/*----------------------------------------------------
 * Interface 
 * i.e a set of function which are accessible at nsp level
 *----------------------------------------------------*/

/*
 * f=fopen(fname [,mode])
 */

static int int_file_fopen(Stack stack, int rhs, int opt, int lhs)
{
  char Fname_expanded[FSIZE+1];
  int xdr= FALSE,swap = TRUE;
  int_types T[] = {string,new_opts, t_end} ;
  nsp_option opts[] ={{ "mode",string,NULLOBJ,-1},
		      { "xdr",s_bool,NULLOBJ,-1},
		      { "swap",s_bool,NULLOBJ,-1},
		      { NULL,t_end,NULLOBJ,-1}};
  NspFile *F;
  char *Fname, *mode = NULL, *def_mode = "rb";
  if ( GetArgs(stack,rhs,opt,T,&Fname,&opts,&mode,&xdr,&swap) == FAIL) return RET_BUG;
  if ( mode == NULL) mode = def_mode; 
  /* expand keys in path name result in buf */
  nsp_expand_file_with_exec_dir(&stack,Fname,Fname_expanded);
  if ((F=nsp_file_open(Fname_expanded,mode,xdr,swap)) == NULLSCIFILE) return RET_BUG;
  MoveObj(stack,1,(NspObject *) F);
  return 1;
}


/*
 */

static int int_file_putfile(Stack stack, int rhs, int opt, int lhs)
{
  char Fname_expanded[FSIZE+1];
  NspSMatrix *S;
  int xdr= FALSE,swap = TRUE,rep;
  int_types T[] = {string,smat, t_end} ;
  NspFile *F;
  char *Fname, *mode = "wb";
  if ( GetArgs(stack,rhs,opt,T,&Fname,&S) == FAIL) return RET_BUG;
  nsp_expand_file_with_exec_dir(&stack,Fname,Fname_expanded);
  if ((F=nsp_file_open(Fname_expanded,mode,xdr,swap)) == NULLSCIFILE) return RET_BUG;
  rep= nsp_fprintf_smatrix(F,S); 
  if (nsp_file_close(F) == FAIL || rep == FAIL ) return RET_BUG;
  return 0;
}

/*
 */

static int int_file_getfile(Stack stack, int rhs, int opt, int lhs)
{
  char Fname_expanded[FSIZE+1];
  NspSMatrix *S = NULL;
  int xdr= FALSE,swap = TRUE;
  int_types T[] = {string, t_end} ;
  NspFile *F;
  char *Fname, *mode = "rb";
  if ( GetArgs(stack,rhs,opt,T,&Fname) == FAIL) return RET_BUG;
  nsp_expand_file_with_exec_dir(&stack,Fname,Fname_expanded);
  if ((F=nsp_file_open(Fname_expanded,mode,xdr,swap)) == NULLSCIFILE) return RET_BUG;
  if ( nsp_fscanf_smatrix(F,&S) == FAIL) 
    {
      nsp_file_close(F);
      return RET_BUG; 
    }
  if (nsp_file_close(F) == FAIL  ) return RET_BUG;
  MoveObj(stack,1,(NspObject *) S);
  return 1;
}

/* compatibility with Scilab 
 * format is not used. 
 */

static int int_file_fscanfMat(Stack stack, int rhs, int opt, int lhs)
{
  char Fname_expanded[FSIZE+1];
  NspMatrix *M;
  NspSMatrix *S = NULL;
  char *format=NULL;
  int xdr= FALSE,swap = TRUE;
  int_types T[] = {string, new_opts, t_end} ;
  NspFile *F;
  char *Fname, *mode = "rb";
  nsp_option opts[] ={{ "format",string,NULLOBJ,-1},
		      { NULL,t_end,NULLOBJ,-1}};
  if ( GetArgs(stack,rhs,opt,T,&Fname,&opts,&format) == FAIL) return RET_BUG;
  nsp_expand_file_with_exec_dir(&stack,Fname,Fname_expanded);
  if ((F=nsp_file_open(Fname_expanded,mode,xdr,swap)) == NULLSCIFILE) return RET_BUG;
  if ( nsp_fscanf_matrix(F,format,&M,(lhs==2),&S) == FAIL) 
    {
      nsp_file_close(F);
      return RET_BUG; 
    }
  MoveObj(stack,1,(NspObject *) M);
  if ( lhs == 2 ) MoveObj(stack,2,(NspObject *) S);
  return Max(lhs,1);
}

/*
 * 
 */

static int int_file_is_little_endian(Stack stack, int rhs, int opt, int lhs)
{
  CheckRhs(0,0);
  CheckLhs(1,1);
  if ( nsp_move_boolean(stack,1,is_little_endian()) == FAIL) return RET_BUG;
  return 1;
}

static OpTab File_func[]={
  {"fopen", int_file_fopen},
  {"is_little_endian",int_file_is_little_endian},
  {"getfile",int_file_getfile},
  {"putfile",int_file_putfile},
  {"fscanfMat",int_file_fscanfMat},
  {(char *) 0, NULL}
};

int File_Interf(int i, Stack stack, int rhs, int opt, int lhs)
{
  return (*(File_func[i].fonc))(stack,rhs,opt,lhs);
}

/* used to walk through the interface table 
 *  (for adding or removing functions) 
 */

void File_Interf_Info(int i, char **fname, function (**f))
{
  *fname = File_func[i].name;
  *f = File_func[i].fonc;
}

