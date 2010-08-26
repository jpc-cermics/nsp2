/* Nsp
 * Copyright (C) 1998-2010 Jean-Philippe Chancelier Enpc/Cermics
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
 *
 * PVM functions interfaces
 * rewriten from scilab code for nsp 
 * 
 * Initial copyright in scilab source 
 * Copyright (c) 1997 by Inria Lorraine.  All Rights Reserved 
 *
 */


#include <string.h> 
#include <sys/time.h>

#include <nsp/object.h> 
#include <nsp/matrix.h> 
#include <nsp/bmatrix.h> 
#include <nsp/list.h> 
#include <nsp/smatrix.h> 
#include <nsp/matint.h> 
#include <nsp/interf.h> 

#include "nsp/interf.h"
#include "nsp/stack.h"
#include "../system/files.h" /* FSIZE+1 */
#include "../../pvm3/include/pvm3.h"

#include "sci_pvm.h"

/* if set to one error in pvm 
 * raises a Scierror else it only returns 
 * an error code 
 */

extern char *pvm_errlist[];
extern int pvm_nerr;
extern int pvm_errno;

static char *nsp_pvm_geterror(int n)
{
  return (n <= 0 && n > -pvm_nerr ? pvm_errlist[-n] : "Unknown Error");
}

static int pvm_error = 0; 

#define NSP_PVM_ERROR(err) \
   if ( pvm_error == 1 && err < 0 ) { Scierror("Error: %s %s\n",NspFname(stack), nsp_pvm_geterror(err)); \
       return RET_BUG;}

/*
 * spawn  new nsp task running a given script 
 */ 

static int nsp_pvm_spawn(char *task_file,int nowin, char *where,int ntask, int *tids)
{
  int flag = (where == NULL ) ? PvmTaskDefault :PvmTaskHost ;
  char cmd[256];
  char *arg[4];
  int nargs= -1;
  arg[0] = NULL;
  cmd[0] = 0;
#if (defined __MSC__) || (defined __ABSC__) || defined(__MINGW32__) 
  strcpy(cmd, "scilex.exe");
#else
  /* I really need nsp here for gtk -version */
  strcpy(cmd, "nsp");
#endif 
  /* always starts with -f since nsp is not run in bg mode in 
   * that case 
   */
  arg[++nargs] = "-f";
  arg[++nargs] = task_file;
  if ( nowin == TRUE ) arg[++nargs] = "-nw";
  arg[++nargs]=NULL;
  return pvm_spawn(cmd, arg, flag, where,ntask, tids);
}

/*
 * interface for spawn 
 */


int int_pvm_spawn( Stack stack, int rhs, int opt, int lhs)
{
  char task_file[FSIZE+1];
  NspMatrix *M;
  char *task=NULL, *where=NULL;
  int ntask=1,res,nowindow=FALSE;
  CheckStdRhs(0,0);
  CheckLhs(1,2);
  int_types T[] = {new_opts, t_end} ;

  nsp_option opts[] ={{ "task",string,NULLOBJ,-1},
		      { "nowindow",s_bool,NULLOBJ,-1},
		      { "ntask",s_int,NULLOBJ,-1},
		      { "where",string,NULLOBJ,-1},
		      { NULL,t_end,NULLOBJ,-1}};

  if ( GetArgs(stack,rhs,opt,T,&opts,&task,&nowindow,&ntask,&where) == FAIL) return RET_BUG;
  if ( ntask <= 0) 
    {
      Scierror("%s: number of tasks is negativs\n",NspFname(stack));
      return RET_BUG;
    }
  if ((M=nsp_matrix_create(NVOID,'r',1,ntask))==NULLMAT) return RET_BUG;
  M->convert = 'i';
  if ( task != NULL) 
    nsp_expand_file_with_exec_dir(&stack,task,task_file);
  else 
    task_file[0]='\0';
  res= nsp_pvm_spawn(task_file,nowindow,where,M->mn,M->I);
  if ( res < 0 ) 
    {
      NSP_PVM_ERROR(res);
    }
  if ( res < M->mn )
    {
      Sciprintf("Warning: some spawn failed cheks the error code in the returned tids\n");
    }
  MoveObj(stack,1,NSP_OBJECT(M)); 
  if ( lhs == 2 ) 
    { 
      if ( nsp_move_double(stack,2,(double) res)==FAIL) return RET_BUG; 
    } 
  return Max(lhs,1); 
}

/*
 *  function : pvm_spawn_independent, fin = 20
 *  args added to pass arguments to the spawn function 
 *  pvm_spawn_independent('/usr/local/nsp2/bin/nsp',args=['-debug'])
 */

int int_pvm_spawn_independent( Stack stack, int rhs, int opt, int lhs)
{
  NspMatrix *M;
  NspSMatrix *S=NULL;
  char *task, *where= NULL;
  int ntask=1,res,flag;

  nsp_option opts[] ={{ "args",smat,NULLOBJ,-1},
		      { "ntask",s_int,NULLOBJ,-1},
		      { "where",string,NULLOBJ,-1},
		      { NULL,t_end,NULLOBJ,-1}};
  
  int_types T[] = {string,new_opts, t_end} ;

  CheckLhs(1,2);

  if ( GetArgs(stack,rhs,opt,T,&task,&opts,&S,&ntask,&where) == FAIL) 
    return RET_BUG;
  if ( ntask <= 0) 
    {
      Scierror("%s: number of tasks is negativs\n",NspFname(stack));
      return RET_BUG;
    }
  if ((M=nsp_matrix_create(NVOID,'r',1,ntask))==NULLMAT) return RET_BUG;
  M->convert = 'i';
  flag = (where == NULL) ? PvmTaskDefault: PvmTaskHost;
  res = pvm_spawn(task,(S==NULL) ? NULL: S->S ,flag, where,ntask,M->I);
  if ( res != M->mn ) 
    {
      NSP_PVM_ERROR(res);
    }
  MoveObj(stack,1,NSP_OBJECT(M));
  if ( lhs == 2 )
    {
      if ( nsp_move_double(stack,2,(double) res)==FAIL) return RET_BUG;
    }
  return Max(lhs,1);
}



/*
 *
 */

static int nsp_pvm_config(int *nhost, int *narch, NspMatrix **dtid,NspMatrix **speed,
		   NspSMatrix **name,NspSMatrix **arch)
{
  int info,i;
  struct pvmhostinfo *hostp;
  
  info = pvm_config(nhost, narch, &hostp);
  if ( info < 0 ) return info;
  
  if ( (*name =nsp_smatrix_create_with_length(NVOID,*nhost,1,-1) ) == NULLSMAT) return PvmNoMem;
  if ( (*arch =nsp_smatrix_create_with_length(NVOID,*nhost,1,-1) ) == NULLSMAT) return PvmNoMem;
  if ( (*dtid=nsp_matrix_create(NVOID,'r',*nhost,1))==NULLMAT) return PvmNoMem;
  if ( (*speed=nsp_matrix_create(NVOID,'r',*nhost,1))==NULLMAT) return PvmNoMem;
  
  for (i = 0; i < *nhost; ++i) 
    {
      if (((*name)->S[i] =nsp_string_copy(hostp[i].hi_name)) == (nsp_string) 0) return PvmNoMem;
      if (((*arch)->S[i] =nsp_string_copy(hostp[i].hi_arch)) == (nsp_string) 0) return PvmNoMem;
      (*dtid)->R[i] = hostp[i].hi_tid;
      (*speed)->R[i] = hostp[i].hi_speed;
    }
  return 0;
}

int int_pvm_config( Stack stack, int rhs, int opt, int lhs)
{
  NspObject *obj;
  int_types Ret[]={ s_int,s_int,mat,smat,smat,mat,s_int,list_end, t_end};
  int info=0,nhost,narch;
  NspMatrix *pdtid,*speed;
  NspSMatrix *name,*arch;
  CheckRhs(0,0);
  CheckLhs(1,1);
  info = nsp_pvm_config(&nhost,&narch,&pdtid,&speed,&name,&arch);
  if (info < 0 ) 
    {
      NSP_PVM_ERROR(info);
      return 0;
    }
  if (( obj = (NspObject *) BuildListFromArgs(NVOID,Ret,nhost,narch,pdtid,name,arch,speed,info)) == NULLOBJ)
    return RET_BUG;
  MoveObj(stack,1,obj);
  return 1;
}

/*
 *  function : pvm_error_mode
 * 
 */

int int_pvm_error_mode( Stack stack, int rhs, int opt, int lhs)
{ 
  char *str;
  CheckRhs(0,1);
  CheckLhs(1,1);
  if ( rhs == 1 ) 
    { 
      if ((str = GetString(stack,1)) == NULLSTRING) return RET_BUG;
      if ( strcmp(str,"stop") == 0) 
	pvm_error = 1; 
      else if ( strcmp(str,"continue") == 0)
	pvm_error = 0; 
      else {
	Scierror("%s: first argument should be \"stop\" or \"continue\"\n",NspFname(stack));
	return RET_BUG;
      }
    }
  else 
    {
      if ( nsp_move_string(stack,1,(pvm_error == 1) ? "stop": "continue",-1)== FAIL) 
	return RET_BUG;
      return 1;
    }
  return 0;
}

/* 
 * generic function 
 */

static int int_pvm_gen( Stack stack, int rhs, int opt, int lhs, int (*f)(char *str))
{
  char *str;
  int rep;
  CheckRhs(1,1);
  CheckLhs(1,1);
  if ((str = GetString(stack,1)) == NULLSTRING) return RET_BUG;
  rep = f(str);
  NSP_PVM_ERROR(rep);
  if ( nsp_move_double(stack,1,(double) rep)==FAIL) return RET_BUG;
  return 1;
}

/*
 *
 */

int int_pvm_joingroup( Stack stack, int rhs, int opt, int lhs)
{
  return int_pvm_gen(stack,rhs,opt,lhs,pvm_joingroup);
}

/*
 *
 */

int int_pvm_lvgroup( Stack stack, int rhs, int opt, int lhs)
{
  return int_pvm_gen(stack,rhs,opt,lhs,pvm_lvgroup);
}

/*
 *
 */

int int_pvm_gsize( Stack stack, int rhs, int opt, int lhs)
{
  return int_pvm_gen(stack,rhs,opt,lhs,pvm_gsize);
}

/*
 * 
 */

int int_pvm_gettid( Stack stack, int rhs, int opt, int lhs)
{
  char *str;
  int rep,tid;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ((str = GetString(stack,1)) == NULLSTRING) return RET_BUG;
  if ( GetScalarInt(stack,2,&tid) == FAIL) return RET_BUG;
  rep  = pvm_gettid(str,tid);
  NSP_PVM_ERROR(rep);
  if ( nsp_move_double(stack,1,(double) rep)==FAIL) return RET_BUG;
  return 1;
}

/*
 *
 */

int int_pvm_getinst( Stack stack, int rhs, int opt, int lhs)
{
  char *str;
  int i,rep;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ((str = GetString(stack,1)) == NULLSTRING) return RET_BUG;
  if ( GetScalarInt(stack,2,&i) == FAIL) return RET_BUG;
  rep  = pvm_getinst(str,i);
  NSP_PVM_ERROR(rep);
  if ( nsp_move_double(stack,1,(double) rep)==FAIL) return RET_BUG;
  return 1;
}

/*
 *
 */

int int_pvm_barrier( Stack stack, int rhs, int opt, int lhs)
{
  char *str;
  int i,rep;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ((str = GetString(stack,1)) == NULLSTRING) return RET_BUG;
  if ( GetScalarInt(stack,2,&i) == FAIL) return RET_BUG;
  rep  = pvm_barrier(str,i);
  NSP_PVM_ERROR(rep);
  if ( nsp_move_double(stack,1,(double) rep)==FAIL) return RET_BUG;
  return 1;
}

/*
 *
 */

static int nsp_pvm_bcast(char *group,int msgtag,NspObject *obj)
{
  int info, bufid;
  bufid = pvm_initsend(PvmDataDefault);
  if (bufid < 0) {
    (void) fprintf(stderr, "Error pvm_bcast: -init- %d\n", bufid);
    return bufid;
  }
  info = nsp_pvm_pkmatrix((NspMatrix *)obj);
  if (info < 0) {
    (void) fprintf(stderr, "Error in nsp_pvm_pkmatrix %d\n", info);
    pvm_freebuf(bufid);
    return info;
  }
  return  pvm_bcast(group,msgtag);
}

int int_pvm_bcast( Stack stack, int rhs, int opt, int lhs)
{
  NspObject *obj;
  char *str;
  int tag,rep;
  CheckRhs(3,3);
  CheckLhs(1,1);
  if ((str = GetString(stack,1)) == NULLSTRING) return RET_BUG;
  if ((obj =nsp_get_object(stack,2))== NULL) return FAIL;
  if ( GetScalarInt(stack,3,&tag) == FAIL) return RET_BUG;
  rep = nsp_pvm_bcast(str,tag,obj);
  NSP_PVM_ERROR(rep);
  if ( nsp_move_double(stack,1,(double) rep)==FAIL) return RET_BUG;
  return 1;
}

/*
 *
 */

static int nsp_pvm_tasks(int where, int *ntask, 
			 NspMatrix **tid,NspMatrix **ptid,NspMatrix **dtid,NspMatrix **flag,
			 NspSMatrix **name)
{
  int i,info;
  struct pvmtaskinfo *taskp;
  info = pvm_tasks(where, ntask, &taskp);
  if ( info < 0  ) {
    return info;
  }
  if ( (*tid=nsp_matrix_create(NVOID,'r',*ntask,1))==NULLMAT) return PvmNoMem;
  if ( (*ptid=nsp_matrix_create(NVOID,'r',*ntask,1))==NULLMAT) return PvmNoMem;
  if ( (*dtid=nsp_matrix_create(NVOID,'r',*ntask,1))==NULLMAT) return PvmNoMem;
  if ( (*flag=nsp_matrix_create(NVOID,'r',*ntask,1))==NULLMAT) return PvmNoMem;
  if ( (*name =nsp_smatrix_create_with_length(NVOID,*ntask,1,-1) ) == NULLSMAT) return PvmNoMem;
  
  for (i = 0; i < *ntask; ++i) {
    (*tid)->R[i] =  taskp[i].ti_tid;
    (*ptid)->R[i] =  taskp[i].ti_ptid;
    (*dtid)->R[i] =  taskp[i].ti_host;
    (*flag)->R[i] =  taskp[i].ti_flag;
    if (((*name)->S[i] =nsp_string_copy(taskp[i].ti_a_out)) == (nsp_string) 0) return PvmNoMem;
  }
  return 0;
}

int int_pvm_tasks( Stack stack, int rhs, int opt, int lhs)
{
  NspObject *obj;
  NspMatrix *tid,*ptid,*dtid,*flag;
  NspSMatrix *name;
  int where=0,info =1,ntask;
  int_types Ret[]={ mat,mat,mat,mat, smat,s_int,s_int, list_end, t_end};
  CheckRhs(0,1); 
  CheckLhs(1,1);
  if ( rhs == 1) { 
    if ( GetScalarInt(stack,1,&where) == FAIL) return RET_BUG;
  }
  info = nsp_pvm_tasks(where,&ntask,&tid,&ptid,&dtid,&flag,&name);
  if (info != 0 ) 
    {
      NSP_PVM_ERROR(info);
      return 0;
    }
  if (( obj = (NspObject *) BuildListFromArgs(NVOID,Ret,tid,ptid,dtid,flag,name,ntask,info)) == NULLOBJ)
    return RET_BUG;
  MoveObj(stack,1,obj);
  return 1;
}

/*
 *
 */

int int_pvm_addhosts( Stack stack, int rhs, int opt, int lhs)
{
  NspSMatrix *S;
  NspMatrix *M;
  CheckRhs(1,1);
  CheckLhs(1,1);
  if ((S = GetSMat(stack,1)) == NULLSMAT) return RET_BUG;
  if ((M=nsp_matrix_create(NVOID,'r',1,S->mn))==NULLMAT) return RET_BUG;
  pvm_addhosts(S->S,S->mn ,M->I);
  M->convert = 'i'; /* occ is filed with integers */
  M = Mat2double(M);
  MoveObj(stack,1,NSP_OBJECT(M));
  return 1;
}

/*
 *
 */

int int_pvm_delhosts( Stack stack, int rhs, int opt, int lhs)
{
  NspSMatrix *S;
  NspMatrix *M;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ((S = GetSMat(stack,1)) == NULLSMAT) return RET_BUG;
  if ((M=nsp_matrix_create(NVOID,'r',1,S->mn))==NULLMAT) return RET_BUG;
  pvm_delhosts(S->S,S->mn ,M->I);
  M->convert = 'i'; /* occ is filed with integers */
  M = Mat2double(M);
  MoveObj(stack,1,NSP_OBJECT(M));
  return 1;
}

/*
 *
 */

int int_pvm_parent( Stack stack, int rhs, int opt, int lhs)
{
  int rep;
  CheckRhs(0,0);
  CheckLhs(1,1);
  rep =pvm_parent();
  NSP_PVM_ERROR(rep);
  if ( nsp_move_double(stack,1,(double) rep)==FAIL) return RET_BUG;
  return 1;
}

/*
 *
 */

int int_pvm_tidtohost( Stack stack, int rhs, int opt, int lhs)
{
  int rep,tid;
  CheckRhs(1,1);
  CheckLhs(1,1);
  if ((GetScalarInt(stack,1,&tid)) == FAIL) return RET_BUG;
  rep = pvm_tidtohost(tid);
  NSP_PVM_ERROR(rep);
  if ( nsp_move_double(stack,1,(double) rep)==FAIL) return RET_BUG;
  return 1;
}

/*
 *
 */

static struct timeval t1;

static int nsp_pvm_settimer(void)
{
  return gettimeofday(&t1, NULL);
} 

static int nsp_pvm_gettimer(double *res)
{
  struct timeval t2;
  if ( gettimeofday(&t2, NULL) == -1) return -1;
  *res = (double)(t2.tv_sec-t1.tv_sec)*1000000.+(double)(t2.tv_usec-t1.tv_usec);
  t1 = t2;
  return 0;
} 

int int_pvm_set_timer( Stack stack, int rhs, int opt, int lhs)
{
  CheckRhs(0,0);
  CheckLhs(-1,0);
  nsp_pvm_settimer();
  return 0;
}

/*
 *
 */

int int_pvm_get_timer( Stack stack, int rhs, int opt, int lhs)
{
  double t=0.0;
  int rep;
  CheckRhs(0,0);
  CheckLhs(1,1);
  rep=nsp_pvm_gettimer(&t);
  if ( nsp_move_double(stack,1,t)==FAIL) return RET_BUG;
  return 1;
}

/*
 *
 */

int int_pvm_mytid( Stack stack, int rhs, int opt, int lhs)
{
  int rep;
  CheckRhs(0,0);
  CheckLhs(1,1);
  rep =pvm_mytid();
  NSP_PVM_ERROR(rep);
  if ( nsp_move_double(stack,1,(double) rep)==FAIL) return RET_BUG;
  return 1;
}


/*
 *
 */

int int_pvm_exit( Stack stack, int rhs, int opt, int lhs)
{
  int rep;
  CheckRhs(0,0);
  CheckLhs(1,1);
  rep =pvm_exit();
  NSP_PVM_ERROR(rep);
  if ( nsp_move_double(stack,1,(double) rep)==FAIL) return RET_BUG;
  return 1;
}

/*
 *
 */

int int_pvm_kill( Stack stack, int rhs, int opt, int lhs)
{
  NspMatrix *A;
  int i;
  CheckRhs(1,1);
  CheckLhs(1,1);
  if (( A = GetRealMatCopy(stack,1))==NULLMAT) return RET_BUG;
  A = Mat2int(A);
  for (i = 0; i < A->mn; ++i) {
    A->I[i] = pvm_kill( A->I[i]);
    NSP_PVM_ERROR(A->I[i]);
  }
  NthObj(1)->ret_pos = 1;
  return 1;
}


/*
 * just implemented for scalar ans string matrices 
 */

int int_pvm_recv( Stack stack, int rhs, int opt, int lhs)
{
  NspObject *obj;
  int tid, tag,info,msginfo,msgtag,msgtid;
  CheckRhs(2,2);
  CheckLhs(1,4);
  if ((GetScalarInt(stack,1,&tid)) == FAIL) return RET_BUG;
  if ((GetScalarInt(stack,2,&tag)) == FAIL) return RET_BUG;
  /* NSP_PVM_ERROR(rep) */
  info = nsp_pvm_recv(tid,tag,&obj,&msginfo,&msgtid,&msgtag);
  if ( info < 0 || obj == NULL) 
    {
      NSP_PVM_ERROR(info);
      return RET_BUG;
    }
  MoveObj(stack,1,NSP_OBJECT(obj));
  if ( lhs >= 2 ) 
    {
      if ( nsp_move_double(stack,2,msginfo)== FAIL) return RET_BUG;
    }
  if ( lhs >= 3) 
    {
      if ( nsp_move_double(stack,3,msgtid)== FAIL) return RET_BUG;
    }
  if ( lhs >= 4) 
    {
      if ( nsp_move_double(stack,4,msgtag)== FAIL) return RET_BUG;
    }
  return Max(1,lhs);
}

/*
 *  just implemented for scalar and string matrices 
 */

int int_pvm_send( Stack stack, int rhs, int opt, int lhs)
{
  NspMatrix *A;
  NspObject *obj;
  int tag,rep;
  CheckRhs(3,3);
  CheckLhs(1,1);
  if (( A = GetRealMatCopy(stack,1))==NULLMAT) return RET_BUG;
  A = Mat2int(A);
  /*  the data at position 2 will be packed by varpack */
  if ((obj =nsp_get_object(stack,2))== NULL) return FAIL;
  if ((GetScalarInt(stack,3,&tag)) == FAIL) return RET_BUG;
  rep = nsp_pvm_send(A->I,A->mn,obj,tag);
  if ( rep < 0 ) 
    {
      NSP_PVM_ERROR(rep);
      return RET_BUG;
    }
  return 0;
}

/* 
 * pvm_reduce 
 * 
 */

int int_pvm_reduce( Stack stack, int rhs, int opt, int lhs)
{
  NspMatrix *A;
  char *str,*str2;
  void (*op)(int *, void *, void *, int *, int *)= NULL;
  int rep,datatype,tag,rootginst,res;
  char *Table[] = {"Max", "Min", "Sum", "Pro", NULL};
  CheckRhs(5,5);
  CheckLhs(1,2);
  if ((str = GetString(stack,1)) == NULLSTRING) return RET_BUG;
  rep = is_string_in_array(str,Table,1);
  if ( rep < 0 ) 
    {
      string_not_in_array(stack,str,Table,"optional argument mode");
      return RET_BUG;
    }
  switch (rep) {
  case 0: op = PvmMax;break;
  case 1: op = PvmMin;break;
  case 2: op = PvmSum;break;
  case 3: op = PvmProduct;break;
  }
  if (( A = GetMatCopy(stack,2))==NULLMAT) return RET_BUG;
  if ((GetScalarInt(stack,3,&tag)) == FAIL) return RET_BUG;
  if ((str2 = GetString(stack,4)) == NULLSTRING) return RET_BUG;
  if ((GetScalarInt(stack,5,&rootginst)) == FAIL) return RET_BUG;
  datatype = (A->rc_type == 'r' ) ? PVM_DOUBLE : PVM_DCPLX;
  res = pvm_reduce(op,A->R,A->mn,datatype,tag,str2,rootginst);
  NSP_PVM_ERROR(res);
  MoveObj(stack,1,(NspObject *) A);
  if ( lhs == 2 )
    {
      if ( nsp_move_double(stack,2,(double) res)==FAIL) return RET_BUG;
    }
  return Max(1,lhs);
}


/*
 *
 */

int int_pvm_start( Stack stack, int rhs, int opt, int lhs)
{
  int  res;
  char *host= NULL;
  CheckRhs(0,1);
  CheckLhs(1,1);
  if ( rhs ==1 ) 
    {
      if ((host = GetString(stack,1)) == NULLSTRING) return RET_BUG;
    } 
  res = nsp_pvm_start(host);
  NSP_PVM_ERROR(res);
  if ( nsp_move_double(stack,1,(double) res)==FAIL) return RET_BUG;
  return 1;
}

/*
 *
 */

int int_pvm_halt( Stack stack, int rhs, int opt, int lhs)
{
  int rep;
  CheckRhs(0,0);
  CheckLhs(1,1);
  rep = nsp_pvm_halt();
  NSP_PVM_ERROR(rep);
  if ( nsp_move_double(stack,1,(double) rep)==FAIL) return RET_BUG;
  return 1;
}

/*
 *
 */

int int_pvm_error( Stack stack, int rhs, int opt, int lhs)
{
  char *res;
  int err;
  CheckRhs(1,1);
  CheckLhs(1,1);
  if ((GetScalarInt(stack,1,&err)) == FAIL) return RET_BUG;
  res = nsp_pvm_geterror(err);
  if ( nsp_move_string(stack,1,res,-1)== FAIL) return RET_BUG;
  return 1;
}

/*
 *
 */

int int_pvm_probe( Stack stack, int rhs, int opt, int lhs)
{
  int rep,tid,tag;
  CheckRhs(2,2);
  CheckLhs(1,1);
  /*  checking variable tid */
  if ((GetScalarInt(stack,1,&tid)) == FAIL) return RET_BUG;
  if ((GetScalarInt(stack,2,&tag)) == FAIL) return RET_BUG;
  rep = pvm_probe(tid,tag);
  if ( nsp_move_double(stack,1,(double) rep)==FAIL) return RET_BUG;
  return 1;
}

/*
 *
 */

int int_pvm_bufinfo( Stack stack, int rhs, int opt, int lhs) 
{
  /* res = [bytes,msgtag,tid,info]; */
  int bufid, res[4],i;
  CheckRhs(1,1);
  CheckLhs(1,4);
  if ((GetScalarInt(stack,1,&bufid)) == FAIL) return RET_BUG;
  res[3] = pvm_bufinfo(bufid,&res[0],&res[1],&res[2]);
  if ( res < 0 ) 
    {
      NSP_PVM_ERROR(res[3]);
    }
  if ( nsp_move_double(stack,1,(double) res[0])==FAIL) return RET_BUG;
  for ( i = 2 ; i <= 4; i++)
    if ( lhs >= i) {
      if ( nsp_move_double(stack,i,(double) res[i-1])==FAIL) return RET_BUG;
      }
  return Max(lhs,1);
}


static OpTab Pvm_func[]={
  {"pvm_joingroup",int_pvm_joingroup},
  {"pvm_lvgroup",int_pvm_lvgroup},
  {"pvm_gsize",int_pvm_gsize},
  {"pvm_gettid",int_pvm_gettid},
  {"pvm_getinst",int_pvm_getinst},
  {"pvm_barrier",int_pvm_barrier},
  {"pvm_bcast",int_pvm_bcast},
  {"pvm_tasks",int_pvm_tasks},
  {"pvm_config",int_pvm_config},
  {"pvm_addhosts",int_pvm_addhosts},
  {"pvm_delhosts",int_pvm_delhosts},
  {"pvm_parent",int_pvm_parent},
  {"pvm_tidtohost",int_pvm_tidtohost},
  {"pvm_set_timer",int_pvm_set_timer},
  {"pvm_get_timer",int_pvm_get_timer},
  {"pvm_mytid",int_pvm_mytid},
  {"pvm_exit",int_pvm_exit},
  {"pvm_kill",int_pvm_kill},
  {"pvm_spawn",int_pvm_spawn},
  {"pvm_spawn_independent",int_pvm_spawn_independent},
  {"pvm_recv",int_pvm_recv},
  {"pvm_send",int_pvm_send},
  {"pvm_reduce",int_pvm_reduce},
  {"pvm_start",int_pvm_start},
  {"pvm_halt",int_pvm_halt},
  {"pvm_error",int_pvm_error},
  {"pvm_probe",int_pvm_probe},
  {"pvm_bufinfo",int_pvm_bufinfo},
  {"pvm_error_mode",int_pvm_error_mode},
  {(char *) 0, NULL}
};

int Pvm_Interf(int i, Stack stack, int rhs, int opt, int lhs)
{
  return (*(Pvm_func[i].fonc))(stack,rhs,opt,lhs);
}

/** used to walk through the interface table 
    (for adding or removing functions) **/

void Pvm_Interf_Info(int i, char **fname, function (**f))
{
  *fname = Pvm_func[i].name;
  *f = Pvm_func[i].fonc;
}



