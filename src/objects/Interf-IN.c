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

/*
 * Interface demo file 
 */

#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "nsp/matrix-in.h"
#include "nsp/bmatrix-in.h"
#include "../interp/Eval.h"

/*
 * A short example 
 * test has four default values and tries to get them 
 * through the calling list 
 *
 * XXXX
 * Tout va bien sauf ? NthObj(1) =  (NspObject *) x1;
 * qui fait que l'argument qui était en position 1 n'est pas libéré 
 * et que AllInterf ne nettoie pas encore les Hopt 
 * On peut imaginer de faire un MoveObj plutot 
 * 
 * A first example for [a,b]= test1(mat,x1= realmat,x2=string,x3=double,x4=matcopy);
 * 
 */

int int_mxtest1(Stack stack, int rhs, int opt, int lhs)
{
  NspMatrix *A, *x1=NULLMAT,*x4=NULLMAT;
  char *x2= NULL;
  double x3 =20.0;
  int new=1;
  int_types T[] = {mat,new_opts, t_end} ;
  nsp_option opts[] ={{ "x1",realmat,NULLOBJ,-1},
		      { "x2",string,NULLOBJ,-1},
		      { "x3",s_double,NULLOBJ,-1},
		      { "x4",matcopy,NULLOBJ,-1},
		      { NULL,t_end,NULLOBJ,-1}};

  if ( GetArgs(stack,rhs,opt,T,&A,&opts,&x1,&x2,&x3,&x4) == FAIL) return RET_BUG;
  /* we give default values to x1 and x4 if they have not been provided 
   * x2 and x3 already have default values (see above ) */ 
  if ( x1 == NULLMAT ) 
    {
      if ((x1 = (NspMatrix *) nsp_create_object_from_double(NVOID,99.0)) == NULLMAT) return RET_BUG;
      NthObj(rhs+new++) = (NspObject *) x1;
    }
  if ( x4 == NULLMAT && lhs == 2 ) 
    {
      if ((x4 = (NspMatrix *)nsp_create_object_from_double(NVOID,56.0+x3)) == NULLMAT) return RET_BUG;
      NthObj(rhs+new++) = (NspObject *) x4;
    }
  NSP_OBJECT(x1)->ret_pos = 1;
  if ( lhs >= 2) NSP_OBJECT(x4)->ret_pos = 2;
  return Max(lhs,1);
}  


int int_mxtest2(Stack stack, int rhs, int opt, int lhs)
{
  NspMatrix *A, *x1=NULLMAT,*x4=NULLMAT;
  int x2    =10 ;
  double x3 =20;
  int new=1;
  /* expected arguments: one requested argument and named optional arguments
   * test1(mat,x1= mat,x2=string,x3=double,x4=matcopy);
   */ 

  nsp_option opts[] ={{ "x1",mat,NULLOBJ,-1},
		      { "x2",s_int,NULLOBJ,-1},
		      { "x3",s_double,NULLOBJ,-1},
		      { "x4",matcopy,NULLOBJ,-1},
		      { NULL,t_end,NULLOBJ,-1}};

  int_types T[] = {mat,new_opts, t_end} ;

  if ( GetArgs(stack,rhs,opt,T,&A,opts,&x1,&x2,&x3,&x4) == FAIL) return RET_BUG;

  if ( x1 == NULLMAT ) 
    {
      if ((x1 = (NspMatrix *)nsp_create_object_from_double(NVOID,99.0+x2)) == NULLMAT) return RET_BUG;
      NthObj(rhs+new++) = (NspObject *) x1;
    }
  if ( x4 == NULLMAT && lhs == 2 ) 
    {
      if ((x4 = (NspMatrix *)nsp_create_object_from_double(NVOID,56.0+x3)) == NULLMAT) return RET_BUG;
      NthObj(rhs+new++) = (NspObject *) x4;
    }
  NSP_OBJECT(x1)->ret_pos = 1;
  if ( lhs >= 2) NSP_OBJECT(x4)->ret_pos = 2;
  return Max(lhs,1);
}  


/*
 * GetArg and GetListArgs 
 */

int int_mxtest5(Stack stack, int rhs, int opt, int lhs)
{
  NspMatrix *A,*B,*Al,*D;
  NspSMatrix *S;
  NspList *C;
  NspObject *O1;
  int_types T[]={ matcopy, mat,list,obj,obj_check, t_end} ;
  int_types Tc[]={ mat,smat,list_end} ;
  if ( GetArgs(stack,rhs,opt, T,&A,&B,&C,&O1,&nsp_type_matrix,&D) == FAIL) return RET_BUG;
  nsp_matrix_print(D,0,TRUE);
  if ( GetListArgs(C,3,Tc,&Al,&S) == FAIL) return RET_BUG;
  nsp_mat_mult_el(A,B);
  nsp_matrix_print(Al,0,TRUE);
  nsp_smatrix_print(S,0);
  NSP_OBJECT(A)->ret_pos = 1;
  return 1;
}  

/* test6(10,20,list(10,'foo',list(30,'foo')),[10,20],[45,67]); */

int int_mxtest6(Stack stack, int rhs, int opt, int lhs)
{
  int L_1;
  double L_31;
  NspMatrix *A,*B;
  NspSMatrix *L_2,*L_32;
  NspObject *O1,*O;
  NspMatrix *O2;
  int_types T[]={matcopy, mat,list_begin,s_int,smat,list_begin,s_double,smat,list_end,
		 list_end,obj,obj_check, t_end} ;
  int_types Ret[]={ s_int,s_double,matcopy,string,list_begin,s_int,s_int,list_end, t_end};
  if ( GetArgs(stack,rhs,opt, T,&A,&B,&L_1,&L_2,&L_31,&L_32,&O1,&nsp_type_matrix,&O2) == FAIL) 
    return RET_BUG;
  nsp_mat_mult_el(A,B);
  nsp_smatrix_print(L_2,0);
  nsp_smatrix_print(L_32,0);
  nsp_object_print(O1,0);
  /* test the list builder **/
  if (( O = (NspObject *) BuildListFromArgs(Ret,10,20.67,A,"foo",10,20 ))== NULLOBJ ) 
    return RET_BUG;
  MoveObj(stack,1,O);
  return 1;
}  


/* test6(%t,list(10,%f),'fin'); */

int int_mxtest6_simp(Stack stack, int rhs, int opt, int lhs)
{
  int b,a,b1;
  char *str;
  int_types T[]={s_bool,list_begin,s_int,s_bool,list_end,string,t_end};
  if ( GetArgs(stack,rhs,opt, T,&b,&a,&b1,&str) == FAIL) return RET_BUG;
  return 0;
}  



int int_mxtest8(Stack stack, int rhs, int opt, int lhs)
{
  NspObject *O;
  int_types Ret[]={ s_int,s_double,string,list_begin,s_int,s_int,list_end, t_end};
  /* test the list builder **/
  if (( O = (NspObject *)BuildListFromArgs(Ret, 10,20.67,"foo",10,20 ))== NULLOBJ ) return RET_BUG;
  NthObj(1)=O;
  return 1;
}  


/* apres cet appel A et B sont devenus des matrices entieres 
 * 
 */

int int_mxtest5_2(Stack stack, int rhs, int opt, int lhs)
{
  int *ia;
  NspMatrix *A,*B;
  int_types T[]={ mat_int,mat_int, t_end} ;
  if ( GetArgs(stack,rhs,opt,T,&A,&B) == FAIL) return RET_BUG;
  ia = (int *) A->R;
  ia[0]=56;
  return 0;
}  


int int_mxtest7(Stack stack, int rhs, int opt, int lhs)
{
  int_types T[]={t_end} ;
  if ( GetArgs(stack,rhs,opt,T) == FAIL) return RET_BUG;
  return 0;
}  


int int_mxtest9(Stack stack, int rhs, int opt, int lhs)
{
  NspList *L1,*L2;
  int_types T[]={obj_check,obj_check, t_end} ;
  if ( GetArgs(stack,rhs,opt,T, &nsp_type_list, &L1, &nsp_type_list, &L2) == FAIL) return RET_BUG;
  return 0;
}  


/*
 * An other short example : 
 * we test how to transmit a primitive 
 */

int int_mxtest(Stack stack, int rhs, int opt, int lhs)
{
  NspPList *P;
  int n;
  NspMatrix *M1;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if (( M1 = GetMat(stack,1)) == NULLMAT) return RET_BUG;
  if (( P = NspPListObj(NthObj(2))) == NULLP_PLIST) return RET_BUG;
  /* second argument is a PList **/
  NthObj(3) = NthObj(1);
  /* Here we have the choice XXXXX NULLOBJ or a PList **/
  if ( (n=nsp_eval_func(NULLOBJ,NSP_OBJECT(P)->name,stack,stack.first+2,1,0,1)) < 0 ); return RET_BUG;
  NthObj(1) = NthObj(3);
  NthObj(3) = NULLOBJ;
  return 1;
}  

/*
 * An other test for returned arguments 
 * XXXX temporaire 
 * XXXXX test temporarire choix ds des strings avec abrev autorisees 
 */

int int_mxtest20(Stack stack, int rhs, int opt, int lhs)
{
  /* test un str parmis une table **/
  static char *Strings[] = {"trois","truc","un","deux", (char *) NULL};
  GetStringInArray(stack,1,Strings,0);
  return 0;
  /* int_mx2pmx(stack,rhs,opt,lhs) ;
     nsp_print_internalPM (NthObj(1)->Element);
     return 1; **/
}  


/*
 * target language : test at level Matrix.c 
 * compilation of 
 *   function [B]=f(A) B=sin(A)+cos(A);
 *   A is a matrix real or complex 
 */

/* On utilise le niveau bas **/

int int_mxtest3(Stack stack, int rhs, int opt, int lhs)
{
  NspMatrix *B,*C;
  CheckRhs(1,1);
  CheckLhs(1,1);
  /* first object is replaced by a copy of A if necessary **/
  if ((B=GetMatCopy(stack,1))== NULLMAT ) return RET_BUG;
  if ((C=nsp_matrix_copy(B))== NULLMAT ) return RET_BUG;
  nsp_mat_sin(B); /* B <-- sin(B) **/
  nsp_mat_cos(C);
  /* B <- B +C **/
  if (nsp_mat_dadd(B,C) == FAIL) return RET_BUG;
  nsp_matrix_destroy(C);
  /* since B is the correct object on the stack we have nothing to do **/
  return 1;
}

/* 
   On reste au niveau des interfaces : c'est plus lourd 
   on doit faire des copies eventuellement inutiles 
   mais les interface implementent vraiment 
   <<sementique>> d'une fonction Scilab 
**/


int int_mxtest4(Stack stack, int rhs, int opt, int lhs)
{
  NspObject *O;
  CheckRhs(1,1);
  CheckLhs(1,1);
  /* Je copie A a la position suivante stack=[.... A,A....[
     la il faudrait raffiner soit copier vraiment soit 
     que les 2 objets pointent sur A si A est nommee **/
  if ((O=nsp_object_copy(NthObj(1)))== NULLOBJ) return RET_BUG;
  NthObj(2)= O;
  /* J'appelle sin avec un stack decall'e **/
  stack.first += 1;
  if (int_mxsin(stack,1,0,1) == RET_BUG) return RET_BUG;
  /* Maintenant sur le stack je dois avoir =[.... A sin(A)...[ **/
  stack.first -= 1;
  SwapObjs(stack,1,2);
  /* Maintenant sur le stack je dois avoir =[.... sin(A) A...[ **/
  /* J'appelle cos avec un stack decall'e **/
  stack.first += 1;
  if (int_mxcos(stack,1,0,1) == RET_BUG) return RET_BUG;
  stack.first -= 1;
  /* Maintenant sur le stack je dois avoir =[.... sin(A) cos(A)...[ **/
  /* j'appelle + **/
  return int_mxdadd(stack,2,0,1);
}

/* test : construction d'une matrice avec valeurs 
 * Attention les valeurs doivent impérativement etre des doubles !!!
 */

int int_mxtest10(Stack stack, int rhs, int opt, int lhs)
{
  CheckRhs(0,0);
  CheckLhs(1,2);
  /* if ((A = nsp_matrix_create_from_doubles(NVOID,5,1,3.0,(double)6,7.0,90.0,3.145))==NULLMAT) return RET_BUG;
     MoveObj(stack,1,(NspObject *) A);*/
  if ( nsp_move_doubles(stack,1,1,2, 45.0, 64.34) == FAIL) return RET_BUG;
  if ( nsp_move_doubles(stack,2,0,0) == FAIL) return RET_BUG;
  return 2; 
}


/* RetArgs 
 */

int int_mxtest11(Stack stack, int rhs, int opt, int lhs)
{
  int_types T[]={string,s_int, s_double, s_bool, list_begin,s_int,string,list_end,mat,smat,t_end} ;
  char *St[] ={ "one","two",NULL};
  CheckRhs(0,0);
  CheckLhs(1,7);
  return  RetArgs(stack,lhs,T,"string",67,78.9,1,789,"string2",
		  nsp_matrix_create_from_doubles(NVOID,2,2, 1.0,8.9,7.8,45.6), 
		  nsp_smatrix_create_from_table(St));
}

/* 
 * Get values from a HashTable 
 * 
 */

typedef struct _test12 {
  NspMatrix *M;
  int x;
  double z;
  NspMatrix *N;
} test12_data ;

int int_mxtest12(Stack stack, int rhs, int opt, int lhs)
{
  test12_data data;
  NspHash *H;
  int_types T[]={hash,t_end} ;
  nsp_option opts[] ={{ "x1",mat,NULLOBJ,-1},
		      { "x2",s_int,NULLOBJ,-1},
		      { "x3",s_double,NULLOBJ,-1},
		      { "x4",matcopy,NULLOBJ,-1},
		      { NULL,t_end,NULLOBJ,-1}};
  if ( GetArgs(stack,rhs,opt,T,&H) == FAIL) return RET_BUG;
  if ( get_args_from_hash(stack,H,opts,&data.M,&data.x,&data.z,&data.N)==FAIL) 
    {
      Scierror("%s: wrong first argument %s\n",stack.fname,nsp_object_get_name(NSP_OBJECT(H)));
      return RET_BUG;
    }
  return 0;
}


/*
 * The Interface for basic matrices operation 
 */

static OpTab Interf_func[]={
  {"test", int_mxtest},
  {"test1", int_mxtest1},
  {"test2", int_mxtest2},
  {"test3", int_mxtest3},
  {"test4", int_mxtest4},
  {"test5", int_mxtest5},
  {"test6", int_mxtest6},
  {"test7", int_mxtest7},
  {"test8", int_mxtest8},
  {"test9", int_mxtest9},
  {"test10", int_mxtest10},
  {"test11", int_mxtest11},
  {"test12", int_mxtest12},
  {(char *) 0, NULL}
};

int Interf_Interf(int i, Stack stack, int rhs, int opt, int lhs)
{
  return (*(Interf_func[i].fonc))(stack,rhs,opt,lhs);
}


/* used to walk through the interface table 
   (for adding or removing functions) **/

void Interf_Interf_Info(int i, char **fname, function (**f))
{
  *fname = Interf_func[i].name;
  *f = Interf_func[i].fonc;
}
