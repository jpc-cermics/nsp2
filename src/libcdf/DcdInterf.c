/* 
 * Copyright (C) 1998-2010 Jean-Philippe Chancelier Cermics/Enpc 
 * 
 * This file is part of <NSP> a free cacsd package 
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. 
 */

/*
 * interfaces for the cdfxxx functions 
 * plus some numerical routines which are present in the dcd package 
 * erf, erfc etc.....
 */

#include <string.h>
#include <nsp/machine.h> 
#include <nsp/object.h> 
#include <nsp/matrix.h> 
#include <nsp/smatrix.h> 
#include <nsp/interf.h> 
#include "cdf.h" 

static int CdfBase(Stack stack,int rhs, int opt, int lhs,
		   int inarg,int oarg,const int pos[],const char *option,
		   const char *errnames,int which,int (*fonc)(),
		   void (*foncErr)(int status,double bound,const int pos[] ));

static void cdfbetErr (int status,double bound, const int pos[]);
static void cdfbinErr (int status,double bound, const int pos[]);
static void cdfchiErr (int status,double bound, const int pos[]);
static void cdffErr (int status,double bound, const int pos[]);
static void cdffncErr (int status,double bound, const int pos[]);
static void cdfgamErr (int status,double bound, const int pos[]);
static void cdfnbnErr (int status,double bound, const int pos[]);
static void cdfnorErr (int status,double bound, const int pos[]);
static void cdfpoiErr (int status,double bound, const int pos[]);
static void cdftErr (int status,double bound, const int pos[]);
static void cdftncErr (int status,double bound, const int pos[]);
static void cdfchnErr (int status,double bound, const int pos[]);

/**************************************************
 *  hand written interface 
 *      Interface for cdfbet 
 *
 *      SUBROUTINE CDFBET( WHICH, P, Q, X, Y, A, B, STATUS, BOUND )
 *               Cumulative Distribution Function
 *                         BETa Distribution
 ***********************************************************************/

int int_cdfbet(Stack stack, int rhs, int opt, int lhs)
{ 
  int rep;
  char *Table[] = {"A","B","PQ","XY",  NULL};
  int minrhs = 5,maxrhs = 6,minlhs=1,maxlhs=2;
  CheckRhs(minrhs,maxrhs);
  CheckLhs(minlhs,maxlhs);

  if ((rep= GetStringInArray(stack,1,Table,1)) == -1) return RET_BUG; 
  switch (rep) 
    {
    case 2: 
      {
	static const int callpos[6] = {4,5,0,1,2,3};
	return CdfBase(stack,rhs,opt,lhs,4,2,callpos,"PQ","X,Y,A and B",1,cdf_cdfbet,cdfbetErr);
      }
      break;
    case 3 :
      {
	static const int callpos[6] = {2,3,4,5,0,1};
	return CdfBase(stack,rhs,opt,lhs,4,2,callpos,"XY","A,B,P and Q",2,cdf_cdfbet, cdfbetErr);
      }
      break;
    case 0 :
      {
	static const int callpos[6] = {1,2,3,4,5,0};
	return CdfBase(stack,rhs,opt,lhs,5,1,callpos,"A","B,P,Q,X and Y",3,cdf_cdfbet,cdfbetErr);
      }
      break;
    case 1 : 
      {
	static const int callpos[6] = {0,1,2,3,4,5};
	return CdfBase(stack,rhs,opt,lhs,5,1,callpos,"B","P,Q,X,Y and A",4,cdf_cdfbet, cdfbetErr);
      }
    }
  return RET_BUG;
}


static void cdfbetErr( int status, double bound, const int pos[])
{
  static char param[]="-PQXYAB";
  switch ( status ) 
    {
    case 1 : Scierror("answer appears to be lower than lowest search bound %g\n",bound);break;
    case 2 : Scierror("answer appears to be higher than greatest search bound %g\n",bound);break;
    case 3 : Scierror(" P + Q .ne. 1 \n");break ;
    case 4 : Scierror(" X + Y .ne. 1 \n");break;
    default : 
      Scierror("input parameter %c is out of range \n\tbound exceeded: %g\n",
	       param[-status-1],bound);
    }
}


/**************************************************
 *  hand written interface 
 *      Interface for cdfbin
 *
 *     SUBROUTINE CDFBIN ( WHICH, P, Q, S, XN, PR, OMPR, STATUS, BOUND )
 *              Cumulative Distribution Function
 *                        BINomial distribution
 ***********************************************************************/


int int_cdfbin(Stack stack, int rhs, int opt, int lhs)
{ 
  int rep;
  char *Table[] = {"PQ","PrOmpr","S","Xn", NULL};
  int minrhs = 5,maxrhs = 6,minlhs=1,maxlhs=2;
  CheckRhs(minrhs,maxrhs);
  CheckLhs(minlhs,maxlhs);

  if ((rep= GetStringInArray(stack,1,Table,1)) == -1) return RET_BUG; 
  switch (rep) 
    {
    case 0 : /* "PQ" */
      {
	static const int callpos[6] = {4,5,0,1,2,3};
	return CdfBase(stack,rhs,opt,lhs,4,2,callpos,"PQ","S,Xn,Pr and Ompr",1,cdf_cdfbin,cdfbinErr);
      }
      break;
    case 2: /* "S" */
      {
	static const int callpos[6] = {3,4,5,0,1,2};
	return CdfBase(stack,rhs,opt,lhs,5,1,callpos,"S","Xn,Pr,Ompr,P and Q",2,cdf_cdfbin,cdfbinErr);
      }
      break;
    case 3: /* "Xn" */
      {
	static const int callpos[6] = {2,3,4,5,0,1};
	return CdfBase(stack,rhs,opt,lhs,5,1,callpos,"Xn","Pr,OMPr,P,Q and S",3,cdf_cdfbin,cdfbinErr);
      }
      break;
    case 1: /* "PrOmpr" */
      {
	static const int callpos[6] = {0,1,2,3,4,5};
	return CdfBase(stack,rhs,opt,lhs,4,2,callpos,"PrOmpr","P,Q,S  and Xn",4,cdf_cdfbin,cdfbinErr);
      }
      break;
    }
  return RET_BUG;
}

static void cdfbinErr(     int status,   double bound, const int pos[])
{
  static char *param[7]={"Which","P","Q","S" ,"Xn","PrOmpr"};
  switch ( status ) 
    {
    case 1 : Scierror("answer appears to be lower than lowest search bound %g\n",bound);break;
    case 2 : Scierror("answer appears to be higher than greatest search bound %g\n",bound);break;
    case 3 : Scierror(" P + Q .ne. 1 \n");break ;
    case 4 : Scierror(" Pr + Ompr .ne. 1 \n");break;
    default : 
      Scierror("input parameter %s is out of range \n\tbound exceeded: %g\n",
	       param[-status-1],bound);
    }
}


/**************************************************
 *  hand written interface 
 *      Interface for cdfchi
 *
 *      SUBROUTINE int_CDFCH( WHICH, P, Q, X, DF, STATUS, BOUND )
 *              Cumulative Distribution Function
 *              CHI-Square distribution
 ***********************************************************************/


int int_cdfchi(Stack stack, int rhs, int opt, int lhs)
{ 
  int rep;
  char *Table[] = {"Df","PQ","X",  NULL};
  int minrhs = 3,maxrhs = 4,minlhs=1,maxlhs=2;
  CheckRhs(minrhs,maxrhs);
  CheckLhs(minlhs,maxlhs);
  if ((rep= GetStringInArray(stack,1,Table,1)) == -1) return RET_BUG; 
  switch (rep) 
    {
    case 1: /* "PQ" */
      {
	static const int callpos[4] = {2,3,0,1};
	return CdfBase(stack,rhs,opt,lhs,2,2,callpos,"PQ","X and Df",1,cdf_cdfchi,cdfchiErr);      
      }
      break;
    case 2: /* "X" */
      {
	static const int callpos[4] = {1,2,3,0};
	return CdfBase(stack,rhs,opt,lhs,3,1,callpos,"X","Df, P and Q",2,cdf_cdfchi,cdfchiErr);
      }
      break;
    case 0: /* "Df" */
      {
	static const int callpos[4] = {0,1,2,3};
	return CdfBase(stack,rhs,opt,lhs,3,1,callpos,"Df","P,Q and X",3,cdf_cdfchi,cdfchiErr);
      }
      break;
    }
  return RET_BUG;
}

static void cdfchiErr(     int status,   double bound, const int pos[])
{
  static char *param[7]={"X","P","Q","X","Df"};
  switch ( status ) 
    {
    case 1 : Scierror("answer appears to be lower than lowest search bound %g\n",bound);break;
    case 2 : Scierror("answer appears to be higher than greatest search bound %g\n",bound);break;
    case 3 : Scierror(" P + Q .ne. 1 \n");break ;
    case 10 : Scierror("cdfchi: Error in  cumgam\n");break;
    default : 
      Scierror("input parameter %s is out of range \n\tbound exceeded: %g\n",
	       param[-status-1],bound);
    }
}


/**************************************************
 *  hand written interface 
 *      Interface for cdff
 *               Cumulative Distribution Function
 *              F distribution
 ***********************************************************************/


int int_cdff(Stack stack, int rhs, int opt, int lhs)
{ 
  int rep;
  char *Table[] = {"Dfd","Dfn","F","PQ",  NULL};
  int minrhs = 4,maxrhs = 5,minlhs=1,maxlhs=2;
  CheckRhs(minrhs,maxrhs);
  CheckLhs(minlhs,maxlhs);
  if ((rep= GetStringInArray(stack,1,Table,1)) == -1) return RET_BUG; 
  switch (rep) 
    {
    case 3: /* "PQ" */
      {
	static const int callpos[5] = {3,4,0,1,2};
	return CdfBase(stack,rhs,opt,lhs,3,2,callpos,"PQ","F,Dfn and Dfd",1,cdf_cdff,cdffErr);
      }
      break;
    case 2: /* "F" */
      {
	static const int callpos[5] = {2,3,4,0,1};
	return CdfBase(stack,rhs,opt,lhs,4,1,callpos,"F","Dfn,Dfd,P and Q",2,cdf_cdff,cdffErr);
      }
      break;
    case 1: /* "Dfn" */
      {
	static const int callpos[5] = {1,2,3,4,0};
	return CdfBase(stack,rhs,opt,lhs,4,1,callpos,"Dfn","Dfd,P,Q and F",3,cdf_cdff,cdffErr);
      }
      break;
    case 0: /* "Dfd" */
      {
	static const int callpos[5] = {0,1,2,3,4};
	return CdfBase(stack,rhs,opt,lhs,4,1,callpos,"Dfd","P,Q,F and Dfn",4,cdf_cdff,cdffErr);
      }
      break;
    }
  return RET_BUG;
}


static void cdffErr(     int status,   double bound, const int pos[])
{
  static char *param[7]={"WHICH","P","Q","F","Dfn","Dfd"};
  switch ( status ) 
    {
    case 1 : Scierror("answer appears to be lower than lowest search bound %g\n",bound);break;
    case 2 : Scierror("answer appears to be higher than greatest search bound %g\n",bound);break;
    case 3 : Scierror(" P + Q .ne. 1 \n");break ;
    case 4 : Scierror("unexpected failure (should not occur)\n");break ;
    default : 
      Scierror("input parameter %s is out of range \n\tbound exceeded: %g\n",
	       param[-status-1],bound);
    }
}



/**************************************************
 *  hand written interface 
 *      Interface for cdffnc
 *               Cumulative Distribution Function
 *              Non-central F distribution
 ***********************************************************************/


int int_cdffnc(Stack stack, int rhs, int opt, int lhs)
{ 
  int rep;
  char *Table[] = {"Dfd","Dfn","F","Pnonc","PQ",  NULL};
  int minrhs = 5,maxrhs = 6,minlhs=1,maxlhs=2;
  CheckRhs(minrhs,maxrhs);
  CheckLhs(minlhs,maxlhs);
  if ((rep= GetStringInArray(stack,1,Table,1)) == -1) return RET_BUG; 
  switch (rep) 
    {
    case 4: /* "PQ" */
      {
	static const int callpos[6] = {4,5,0,1,2,3};
	return CdfBase(stack,rhs,opt,lhs,4,2,callpos,"PQ","F,Dfn,Dfd and Pnonc",1,cdf_cdffnc,cdffncErr);
      }
      break;
    case 2: /* "F" */
      {
	static const int callpos[6] = {3,4,5,0,1,2};
	return CdfBase(stack,rhs,opt,lhs,5,1,callpos,"F","Dfn,Dfd,Pnonc,P and Q",2,cdf_cdffnc,cdffncErr);
      }
      break;
    case 1: /* "Dfn" */
      {
	static const int callpos[6] = {2,3,4,5,0,1};
	return CdfBase(stack,rhs,opt,lhs,5,1,callpos,"Dfn","Dfd,Pnonc,P,Q and F",3,cdf_cdffnc,cdffncErr);
      }
      break;
    case 0: /* "Dfd" */
      {
	static const int callpos[6] = {1,2,3,4,5,0};
	return CdfBase(stack,rhs,opt,lhs,5,1,callpos,"Dfd","Pnonc,P,Q,F and Dfn",4,cdf_cdffnc,cdffncErr);
      }
      break;
    case 3: /* "Pnonc" */
      {
	static const int callpos[6] = {0,1,2,3,4,5};
	return CdfBase(stack,rhs,opt,lhs,5,1,callpos,"Pnonc","P,Q,F,Dfn and Dfd",5,cdf_cdffnc,cdffncErr);
      }
    }
  return 0;
}


static void cdffncErr(     int status,   double bound, const int pos[])
{
  static char *param[7]={"WHICH","P","Q","F","Dfn","Dfd","Pnonc"};
  switch ( status ) 
    {
    case 1 : Scierror("answer appears to be lower than lowest search bound %g\n",bound);break;
    case 2 : Scierror("answer appears to be higher than greatest search bound %g\n",bound);break;
    case 3 : Scierror(" P + Q .ne. 1 \n");break ;
    default : 
      Scierror("input parameter %s is out of range \n\tbound exceeded: %g\n",
	       param[-status-1],bound);
    }
}



/**************************************************
 *  hand written interface 
 *      Interface for cdfgam
 *               Cumulative Distribution Function
 *              F distribution
 ***********************************************************************/


int int_cdfgam(Stack stack, int rhs, int opt, int lhs)
{
  int rep;
  char *Table[] = {"PQ", "Scale" ,  "Shape","X", NULL}; 
  int minrhs = 4,maxrhs = 5,minlhs=1,maxlhs=2;
  CheckRhs(minrhs,maxrhs);
  CheckLhs(minlhs,maxlhs);
  if ((rep= GetStringInArray(stack,1,Table,1)) == -1) return RET_BUG; 
  switch (rep) 
    {
    case 0: /* "PQ" */
      {
	static const int callpos[5] = {3,4,0,1,2};
	return CdfBase(stack,rhs,opt,lhs,3,2,callpos,"PQ","X,Shape and Scale",1,cdf_cdfgam,cdfgamErr);
      }
      break;
    case 3: /* "X" */
      {
	static const int callpos[5] = {2,3,4,0,1};
	return CdfBase(stack,rhs,opt,lhs,4,1,callpos,"X","Shape,Scale,P and Q",2,cdf_cdfgam,cdfgamErr);
      }
      break;
    case 2: /* "Shape" */
      {
	static const int callpos[5] = {1,2,3,4,0};
	return CdfBase(stack,rhs,opt,lhs,4,1,callpos,"Shape","Scale,P,Q and X",3,cdf_cdfgam,cdfgamErr);
      }
      break;
    case 1: /* "Scale" */
      {
	static const int callpos[5] = {0,1,2,3,4};
	return CdfBase(stack,rhs,opt,lhs,4,1,callpos,"Scale","P,Q,X and Shape",4,cdf_cdfgam,cdfgamErr);
      }
    }
  return 0;
}


static void cdfgamErr(int status,double bound, const int pos[])
{
  static char *param[7]={"WHICH","P","Q","X","Shape","Scale"};
  switch ( status ) 
    {
    case 1 : Scierror("answer appears to be lower than lowest search bound %g\n",bound);break;
    case 2 : Scierror("answer appears to be higher than greatest search bound %g\n",bound);break;
    case 3 : Scierror("P + Q .ne. 1 \n");break ;
    case 10 : Scierror(" cannot compute the answer \n");break ;
    default : 
      Scierror("input parameter %s is out of range \n\tbound exceeded: %g\n",
	       param[-status-1],bound);
    }
}


/**************************************************
 *  hand written interface 
 *      Interface for cdfnbn 
 * SUBROUTINE cdfnbn(which,p,q,s,xn,pr,ompr,status,bound)
 *               Cumulative Distribution Function
 *               Negative BiNomial distribution
 ***********************************************************************/

int int_cdfnbn(Stack stack, int rhs, int opt, int lhs)
{ 
  int rep;
  char *Table[] = { "PQ" ,  "PrOmpr", "S", "Xn", NULL};
  int minrhs = 5,maxrhs = 6,minlhs=1,maxlhs=2;
  CheckRhs(minrhs,maxrhs);
  CheckLhs(minlhs,maxlhs);
  if ((rep= GetStringInArray(stack,1,Table,1)) == -1) return RET_BUG; 
  switch (rep) 
    {
    case 0: /* "PQ" */
      {
	static const int callpos[6] = {4,5,0,1,2,3};
	return CdfBase(stack,rhs,opt,lhs,4,2,callpos,"PQ","S,XN,PR and OMPR",1,cdf_cdfnbn,cdfnbnErr);
      }
      break;
    case 2: /* "S" */
      {
	static const int callpos[6] = {3,4,5,0,1,2};
	return CdfBase(stack,rhs,opt,lhs,5,1,callpos,"S","XN,PR,OMPR,P and Q",2,cdf_cdfnbn,cdfnbnErr);
      }
      break;
    case 3: /* "Xn" */
      {
	static const int callpos[6] = {2,3,4,5,0,1};
	return CdfBase(stack,rhs,opt,lhs,5,1,callpos,"Xn","PR,OMPR,P,Q and S",3,cdf_cdfnbn,cdfnbnErr);
	return 0;
      }
      break;
    case 1: /* "PrOmpr" */
      {
	static const int callpos[6] = {0,1,2,3,4,5};
	return CdfBase(stack,rhs,opt,lhs,4,2,callpos,"PrOmpr","P,Q,S and Xn",4,cdf_cdfnbn,cdfnbnErr);
      }
    }
  return 0;
}

static void cdfnbnErr(    int status,double bound, const int pos[])
{
  static char *param[]={"WHICH", "P","Q","S","Xn","PrOmpr"};
  switch ( status ) 
    {
    case 1 : Scierror("answer appears to be lower than lowest search bound %g\n",bound);break;
    case 2 : Scierror("answer appears to be higher than greatest search bound %g\n",bound);break;
    case 3 : Scierror(" P + Q .ne. 1 \n");break ;
    case 4 : Scierror(" Pr + Ompr .ne. 1 \n");break;
    default : 
      Scierror("input parameter %s is out of range \n\tbound exceeded: %g\n",
	       param[-status-1],bound);
    }
}


/**************************************************
 *  hand written interface 
 *      Interface for cdfnor
 *               Cumulative Distribution Function
 *               NORmal distribution
 ***********************************************************************/


int int_cdfnor(Stack stack, int rhs, int opt, int lhs)
{ 
  int rep;
  char *Table[] = {"Mean", "PQ","Std" ,"X",  NULL};
  int minrhs = 4,maxrhs = 5,minlhs=1,maxlhs=2;
  CheckRhs(minrhs,maxrhs);
  CheckLhs(minlhs,maxlhs);
  if ((rep= GetStringInArray(stack,1,Table,1)) == -1) return RET_BUG; 
  switch (rep) 
    {
    case 1: /* "PQ" */
      {
	static const int callpos[5] = {3,4,0,1,2};
	return CdfBase(stack,rhs,opt,lhs,3,2,callpos,"PQ","X,Mean and Std",1,cdf_cdfnor,cdfnorErr);
      }
      break;
    case 3: /* "X" */
      {
	static const int callpos[5] = {2,3,4,0,1};
	return CdfBase(stack,rhs,opt,lhs,4,1,callpos,"X","Mean,Std,P and Q",2,cdf_cdfnor,cdfnorErr);
      }
      break;
    case 0: /* "Mean" */
      {
	static const int callpos[5] = {1,2,3,4,0};
	return CdfBase(stack,rhs,opt,lhs,4,1,callpos,"Mean","Std,P,Q and X",3,cdf_cdfnor,cdfnorErr);
      }
      break;
    case 2: /* "Std" */
      {
	static const int callpos[5] = {0,1,2,3,4};
	return CdfBase(stack,rhs,opt,lhs,4,1,callpos,"Std","P,Q,X and Mean",4,cdf_cdfnor,cdfnorErr);
      }
    }
  return 0;
}

static void cdfnorErr(    int status,double bound, const int pos[])
{
  int iname;
  static char *param[]={"WHICH", "P","Q","X","Mean","Std"};
  switch ( status ) 
    {
    case 1 : Scierror("answer appears to be lower than lowest search bound %g\n",bound);break;
    case 2 : Scierror("answer appears to be higher than greatest search bound %g\n",bound);break;
    case 3 : Scierror(" P + Q .ne. 1 \n");break ;
    case 4 : Scierror(" Std must not be zero \n");break ;
    default : 
      iname = Max(0,Min(5,-status -1));
      Scierror("input parameter %s is out of range \n\tbound exceeded: %g\n",param[iname],bound);
    }
}



/**************************************************
 *  hand written interface 
 *      Interface for cdfpoi
 * POIsson distribution
 ***********************************************************************/


int int_cdfpoi(Stack stack, int rhs, int opt, int lhs)
{ 
  int rep;
  char *Table[] = {"PQ", "S", "Xlam", NULL};
  int minrhs = 3,maxrhs = 4,minlhs=1,maxlhs=2;
  CheckRhs(minrhs,maxrhs);
  CheckLhs(minlhs,maxlhs);
  if ((rep= GetStringInArray(stack,1,Table,1)) == -1) return RET_BUG; 
  switch (rep) 
    {
    case 0: /* "PQ" */
      {
	static const int callpos[4] = {2,3,0,1};
	return CdfBase(stack,rhs,opt,lhs,2,2,callpos,"PQ","S and Xlam",1,cdf_cdfpoi,cdfpoiErr);
      }
      break;
    case 1: /* "S" */
      {
	static const int callpos[4] = {1,2,3,0};
	return CdfBase(stack,rhs,opt,lhs,3,1,callpos,"S","Xlam,P and Q",2,cdf_cdfpoi,cdfpoiErr);
      }
      break;
    case 2: /* "Xlam" */
      {
	static const int callpos[4] = {0,1,2,3};
	return CdfBase(stack,rhs,opt,lhs,3,1,callpos,"Xlam","P,Q and S",3,cdf_cdfpoi,cdfpoiErr);
      }
    }
  return 0;
}

static void cdfpoiErr(    int status,double bound, const int pos[])
{
  static char *param[7]={"WHICH", "P","Q","S","Xlam"};
  switch ( status ) 
    {
    case 1 : Scierror("answer appears to be lower than lowest search bound %g\n",bound);break;
    case 2 : Scierror("answer appears to be higher than greatest search bound %g\n",bound);break;
    case 3 : Scierror(" P + Q .ne. 1 \n");break ;
    default : 
      Scierror("input parameter %s is out of range \n\tbound exceeded: %g\n",
	       param[-status-1],bound);
    }
}



/**************************************************
 *  hand written interface 
 *      Interface for cdft
 *              T distribution
 ***********************************************************************/


int int_cdft(Stack stack, int rhs, int opt, int lhs)
{ 
  int rep;
  char *Table[] = {"Df", "PQ", "T" , NULL};
  int minrhs = 3,maxrhs = 4,minlhs=1,maxlhs=2;
  CheckRhs(minrhs,maxrhs);
  CheckLhs(minlhs,maxlhs);
  if ((rep= GetStringInArray(stack,1,Table,1)) == -1) return RET_BUG; 
  switch (rep) 
    {
    case 1: /* "PQ" */
      {
	static const int callpos[4] = {2,3,0,1};
	return CdfBase(stack,rhs,opt,lhs,2,2,callpos,"PQ","T and Df",1,cdf_cdft,cdftErr);
      }
      break;
    case 2: /* "T" */
      {
	static const int callpos[4] = {1,2,3,0};
	return CdfBase(stack,rhs,opt,lhs,3,1,callpos,"T","Df, P and Q",2,cdf_cdft,cdftErr);
      }
      break;
    case 0: /* "Df" */
      {
	static const int callpos[4] = {0,1,2,3};
	return CdfBase(stack,rhs,opt,lhs,3,1,callpos,"Df","P,Q and T",3,cdf_cdft,cdftErr);
      }
    }
  return 0;
}

static void cdftErr(    int status,double bound, const int pos[])
{
  static char *param[7]={"WHICH", "P","Q","T","Df"};
  switch ( status ) 
    {
    case 1 : Scierror("answer appears to be lower than lowest search bound %g\n",bound);break;
    case 2 : Scierror("answer appears to be higher than greatest search bound %g\n",bound);break;
    case 3 : Scierror(" P + Q .ne. 1 \n");break ;
    default : 
      Scierror("input parameter %s is out of range \n\tbound exceeded: %g\n",
	       param[-status-1],bound);
    }
}

/**************************************************
 *  hand written interface 
 *      Interface for cdft
 *              T distribution non central
 ***********************************************************************/


int int_cdftnc(Stack stack, int rhs, int opt, int lhs)
{ 
  int rep;
  char *Table[] = {"Df", "PQ", "T" , "Pnonc", NULL};
  int minrhs = 3,maxrhs = 5,minlhs=1,maxlhs=2;
  CheckRhs(minrhs,maxrhs);
  CheckLhs(minlhs,maxlhs);
  if ((rep= GetStringInArray(stack,1,Table,1)) == -1) return RET_BUG; 
  switch (rep) 
    {
    case 1: /* "PQ" */
      {
	static const int callpos[5] = {3,4,0,1,2};
	return CdfBase(stack,rhs,opt,lhs,3,2,callpos,"PQ","T, Df and Pnonc",1,cdf_cdftnc,cdftncErr);
      }
      break;
    case 2: /* "T" */
      {
	static const int callpos[5] = {2,3,4,0,1};
	return CdfBase(stack,rhs,opt,lhs,4,1,callpos,"T","Df, Pnonc, P and Q",2,cdf_cdftnc,cdftncErr);
      }
      break;
    case 0: /* "Df" */
      {
	/* XXX here pnonc should be five */
	static const int callpos[5] = {1,2,3,4,0};
	return CdfBase(stack,rhs,opt,lhs,4,1,callpos,"Df","Pnonc, P,Q and T",3,cdf_cdftnc,cdftncErr);
      }
    case 3: /* "Pnonc" */
      {
	static const int callpos[5] = {0,1,2,3,4};
	return CdfBase(stack,rhs,opt,lhs,4,1,callpos,"Pnonc","P,Q, T,and Df",4,cdf_cdftnc,cdftncErr);
      }
    }
  return 0;
}

static void cdftncErr(    int status,double bound, const int pos[])
{
  static char *param[7]={"WHICH", "P","Q","T","Df"};
  switch ( status ) 
    {
    case 1 : Scierror("answer appears to be lower than lowest search bound %g\n",bound);break;
    case 2 : Scierror("answer appears to be higher than greatest search bound %g\n",bound);break;
    case 3 : Scierror(" P + Q .ne. 1 \n");break ;
    default : 
      Scierror("input parameter %s is out of range \n\tbound exceeded: %g\n",
	       param[-status-1],bound);
    }
}


/**************************************************
 *  hand written interface 
 *      Interface for cdfchn
 *  Non-central Chi-Square
 ***********************************************************************/


int int_cdfchn(Stack stack, int rhs, int opt, int lhs)
{ 
  int rep;
  char *Table[] = { "Df" ,"Pnonc", "PQ", "X",NULL};
  int minrhs = 4,maxrhs = 5,minlhs=1,maxlhs=2;
  CheckRhs(minrhs,maxrhs);
  CheckLhs(minlhs,maxlhs);
  if ((rep= GetStringInArray(stack,1,Table,1)) == -1) return RET_BUG; 
  switch (rep) 
    {
    case 2: /* "PQ" */
      {
	static const int callpos[5] = {3,4,0,1,2};
	return CdfBase(stack,rhs,opt,lhs,3,2,callpos,"PQ","X,Df and Pnonc",1,cdf_cdfchn,cdfchnErr);
      }
      break;
    case 3: /* "X" */
      {
	static const int callpos[5] = {2,3,4,0,1};
	return CdfBase(stack,rhs,opt,lhs,4,1,callpos,"X","Df,Pnonc,P and Q",2,cdf_cdfchn,cdfchnErr);
      }
      break;
    case 0: /* "Df" */
      {
	static const int callpos[5] = {1,2,3,4,0};
	return CdfBase(stack,rhs,opt,lhs,4,1,callpos,"Df","Pnonc,P,Q and X",3,cdf_cdfchn,cdfchnErr);
      }
      break;
    case 1: /* "Pnonc" */
      {
	static const int callpos[5] = {0,1,2,3,4};
	return CdfBase(stack,rhs,opt,lhs,4,1,callpos,"Pnonc","P,Q,X and Df",4,cdf_cdfchn,cdfchnErr);
      }
    }
  return 0;
}


static void cdfchnErr(    int status,double bound, const int pos[])
{
  static char *param[7]={"WHICH", "P","Q","X","Df","Pnonc"};
  switch ( status ) 
    {
    case 1 : Scierror("answer appears to be lower than lowest search bound %g\n",bound);break;
    case 2 : Scierror("answer appears to be higher than greatest search bound %g\n",bound);break;
    case 3 : Scierror(" P + Q .ne. 1 \n");break ;
    case 4 : Scierror("unexpected failure (should not occur)\n");break ;
    default : 
      Scierror("input parameter %s is out of range \n\tbound exceeded: %g\n",
	       param[-status-1],bound);
    }
}

/* test */

typedef double (*VM11)(double x);
typedef double (*VM12)(double x,double y);

static int int_mx_genv11 (Stack stack, int rhs, int opt, int lhs, VM11 F, VM12 G)
{
  NspMatrix *HMat,*B;
  int i;
  CheckRhs (1, 2);
  CheckLhs (1, 1);
  if ((HMat = GetRealMatCopy (stack, 1)) == NULLMAT)
    return RET_BUG;
  if ( rhs == 2 ) 
    {
      if ( G == NULL) 
	{
	  Scierror("Error: expecting just one argument\n");
	  return RET_BUG;
	}
      if ((B = GetRealMat(stack,2)) == NULLMAT) 
	return RET_BUG;
      CheckSameDims(NspFname(stack),1,2,HMat,B);
      for ( i = 0 ; i < HMat->mn ; i++) 
	{
	  HMat->R[i]= G(HMat->R[i],B->R[i]);
	}
    }
  else 
    {
      if ( F == NULL) 
	{
	  Scierror("Error: expecting two argument\n");
	  return RET_BUG;
	}
      for ( i = 0 ; i < HMat->mn ; i++) 
	{
	  HMat->R[i]= F(HMat->R[i]);
	}
    }
  NSP_OBJECT (HMat)->ret_pos = 1;
  return 1;
}


static int int_cdf_rlog1(Stack stack, int rhs, int opt, int lhs)
{
  return int_mx_genv11( stack,rhs,opt,lhs, cdf_rlog1,NULL);
}

static int int_cdf_rlog1_old(Stack stack, int rhs, int opt, int lhs)
{
  return int_mx_genv11( stack,rhs,opt,lhs, cdf_rlog1_old,NULL);
}

static int int_cdf_rlog(Stack stack, int rhs, int opt, int lhs)
{
  return int_mx_genv11( stack,rhs,opt,lhs, cdf_rlog,NULL);
}

static int int_cdf_rlog_old(Stack stack, int rhs, int opt, int lhs)
{
  return int_mx_genv11( stack,rhs,opt,lhs, cdf_rlog_old,NULL);
}

static int int_cdf_gamln(Stack stack, int rhs, int opt, int lhs)
{
  return int_mx_genv11( stack,rhs,opt,lhs, cdf_gamln,NULL);
}

static int int_cdf_gamln1(Stack stack, int rhs, int opt, int lhs)
{
  return int_mx_genv11( stack,rhs,opt,lhs, cdf_gamln1,NULL);
}

static int int_cdf_gamln1_old(Stack stack, int rhs, int opt, int lhs)
{
  return int_mx_genv11( stack,rhs,opt,lhs, cdf_gamln1_old,NULL);
}

static int int_cdf_dln1px(Stack stack, int rhs, int opt, int lhs)
{
  return int_mx_genv11( stack,rhs,opt,lhs, cdf_dln1px,NULL);
}

static int int_cdf_dln1px_old(Stack stack, int rhs, int opt, int lhs)
{
  return int_mx_genv11( stack,rhs,opt,lhs, cdf_dln1px_old,NULL);
}


static int int_cdf_dlanor(Stack stack, int rhs, int opt, int lhs)
{
  return int_mx_genv11( stack,rhs,opt,lhs, cdf_dlanor,NULL);
}


static int int_cdf_rexp(Stack stack, int rhs, int opt, int lhs)
{
  return int_mx_genv11( stack,rhs,opt,lhs, cdf_rexp,NULL);
}

static int int_cdf_rexp_old(Stack stack, int rhs, int opt, int lhs)
{
  return int_mx_genv11( stack,rhs,opt,lhs, cdf_rexp_old,NULL);
}

static int int_cdf_gamma(Stack stack, int rhs, int opt, int lhs)
{
  return int_mx_genv11( stack,rhs,opt,lhs, cdf_gamma,NULL);
}

static int int_cdf_gamma_old(Stack stack, int rhs, int opt, int lhs)
{
  return int_mx_genv11( stack,rhs,opt,lhs, cdf_gamma_old,NULL);
}

double cdf_grat1_test(double a, double x)
{
  double r,p,q, eps=1.e-16;
  /* e**(-x)*x**a/gamma(a).  */
  r =  exp(-x)* exp (a * log (x))/(cdf_gamma(a));
  cdf_grat1 (&a, &x, &r, &p, &q, &eps);
  return p;
}

static int int_cdf_grat1(Stack stack, int rhs, int opt, int lhs)
{
  return int_mx_genv11( stack,rhs,opt,lhs,NULL, cdf_grat1_test);
}

static int int_cdf_gam1(Stack stack, int rhs, int opt, int lhs)
{
  return int_mx_genv11( stack,rhs,opt,lhs, cdf_gam1,NULL);
}

static int int_cdf_gam1_old(Stack stack, int rhs, int opt, int lhs)
{
  return int_mx_genv11( stack,rhs,opt,lhs, cdf_gam1_old,NULL);
}


static int int_cdf_dlanor_old(Stack stack, int rhs, int opt, int lhs)
{
  return int_mx_genv11( stack,rhs,opt,lhs, cdf_dlanor_old,NULL);
}

static int int_cdf_alngam(Stack stack, int rhs, int opt, int lhs)
{
  return int_mx_genv11( stack,rhs,opt,lhs, cdf_alngam,NULL);
}

static int int_cdf_alngam_old(Stack stack, int rhs, int opt, int lhs)
{
  return int_mx_genv11( stack,rhs,opt,lhs, cdf_alngam_old,NULL);
}

static int int_cdf_algdiv(Stack stack, int rhs, int opt, int lhs)
{
  return int_mx_genv11( stack,rhs,opt,lhs, NULL, cdf_algdiv);
}

static int int_cdf_algdiv_old(Stack stack, int rhs, int opt, int lhs)
{
  return int_mx_genv11( stack,rhs,opt,lhs, NULL, cdf_algdiv_old);
}

static int int_cdf_betaln(Stack stack, int rhs, int opt, int lhs)
{
  return int_mx_genv11( stack,rhs,opt,lhs, NULL, cdf_betaln);
}

static int int_cdf_betaln_old(Stack stack, int rhs, int opt, int lhs)
{
  return int_mx_genv11( stack,rhs,opt,lhs, NULL, cdf_betaln_old);
}

static int int_cdf_bcorr(Stack stack, int rhs, int opt, int lhs)
{
  return int_mx_genv11( stack,rhs,opt,lhs, NULL, cdf_bcorr);
}

static int int_cdf_bcorr_old(Stack stack, int rhs, int opt, int lhs)
{
  return int_mx_genv11( stack,rhs,opt,lhs, NULL, cdf_bcorr_old);
}

static int int_cdf_psi1(Stack stack, int rhs, int opt, int lhs)
{
  return int_mx_genv11( stack,rhs,opt,lhs, cdf_psi1, NULL);
}

static int int_cdf_psi1_old(Stack stack, int rhs, int opt, int lhs)
{
  return int_mx_genv11( stack,rhs,opt,lhs, cdf_psi1_old, NULL);
}

static int int_cdf_erf(Stack stack, int rhs, int opt, int lhs)
{
  return int_mx_genv11( stack,rhs,opt,lhs, cdf_erf, NULL);
}

double cdf_erfc_1(double x)
{
  return cdf_erfc(0,x);
}

static int int_cdf_erfc(Stack stack, int rhs, int opt, int lhs)
{
  return int_mx_genv11( stack,rhs,opt,lhs, cdf_erfc_1, NULL);
}

double cdf_erfc_2(double x)
{
  return cdf_erfc(1,x);
}

static int int_cdf_erfc1(Stack stack, int rhs, int opt, int lhs)
{
  return int_mx_genv11( stack,rhs,opt,lhs, cdf_erfc_2, NULL);
}




static int int_cdf_stirling_series_diff(Stack stack, int rhs, int opt, int lhs)
{
  return int_mx_genv11( stack,rhs,opt,lhs, NULL,cdf_stirling_series_diff );
}




/*************************************************************
 * The Interface for basic matrices operation 
 *************************************************************/

static OpTab Dcd_func[]={
  {"cdfbet",int_cdfbet}, 
  {"cdfbin",int_cdfbin}, 
  {"cdfchi",int_cdfchi}, 
  {"cdfchn",int_cdfchn}, 
  {"cdff",int_cdff}, 
  {"cdffnc",int_cdffnc}, 
  {"cdfgam",int_cdfgam}, 
  {"cdfnbn",int_cdfnbn}, 
  {"cdfnor",int_cdfnor}, 
  {"cdfpoi",int_cdfpoi}, 
  {"cdft",int_cdft}, 
  {"cdftnc",int_cdftnc}, 
  {"cdf_rlog1",int_cdf_rlog1},
  {"cdf_rlog1_old",int_cdf_rlog1_old},
  {"cdf_rexp",int_cdf_rexp},
  {"cdf_rexp_old",int_cdf_rexp_old},
  {"cdf_gamma",int_cdf_gamma},
  {"cdf_gamma_old",int_cdf_gamma_old},
  {"cdf_gam1",int_cdf_gam1},
  {"cdf_gam1_old",int_cdf_gam1_old},
  {"cdf_rlog",int_cdf_rlog},
  {"cdf_rlog_old",int_cdf_rlog_old},
  {"cdf_algdiv",int_cdf_algdiv},
  {"cdf_algdiv_old",int_cdf_algdiv_old},
  {"cdf_betaln",int_cdf_betaln},
  {"cdf_betaln_old",int_cdf_betaln_old},
  {"cdf_bcorr",int_cdf_bcorr},
  {"cdf_bcorr_old",int_cdf_bcorr_old},
  {"cdf_gamln",int_cdf_gamln},
  {"cdf_gamln1",int_cdf_gamln1},
  {"cdf_gamln1_old",int_cdf_gamln1_old},
  {"cdf_dln1px",int_cdf_dln1px}, 
  {"cdf_dln1px_old",int_cdf_dln1px_old},
  {"cdf_dlanor",int_cdf_dlanor},
  {"cdf_dlanor_old",int_cdf_dlanor_old},
  {"cdf_alngam",int_cdf_alngam},
  {"cdf_alngam_old",int_cdf_alngam_old},
  {"cdf_psi",int_cdf_psi1},
  {"cdf_psi_old",int_cdf_psi1_old},
  {"cdf_grat1",int_cdf_grat1},
  {"cdf_erf",int_cdf_erf},
  {"cdf_erfc",int_cdf_erfc},
  {"cdf_erfc1",int_cdf_erfc1},
  {"cdf_stirling_series_diff",int_cdf_stirling_series_diff},
  {(char *) 0, NULL}
};

int Dcd_Interf(int i, Stack stack, int rhs, int opt, int lhs)
{
  return (*(Dcd_func[i].fonc))(stack,rhs,opt,lhs);
}

/** used to walk through the interface table 
    (for adding or removing functions) **/

void Dcd_Interf_Info(int i, char **fname, function (**f))
{
  *fname = Dcd_func[i].name;
  *f = Dcd_func[i].fonc;
}

/* 
 * A utility function for the cdf family 
 */

static int CdfBase(Stack stack,int rhs, int opt, int lhs,
		   int inarg,int oarg,const int pos[],const char *option,
		   const char *errnames,int which,int (*fonc)(),void (*foncErr)(int status,double bound,const int pos[] ))
{
  NspMatrix *M[6];
  int i,status=0;
  double bound;
  if ( rhs != inarg+1 ) 
    {
      Scierror("%s: Rhs must be %d for '%s' option'\n",NspFname(stack),inarg+1,option);
      return RET_BUG;
    }

  lhs = Max(lhs,1); /* always return at least one value 1 */

  if ( oarg < lhs ) 
    {
      /* just in case, but this is checked elsewhere */
      Scierror("%s: lhs must be smaller or equal to %d for '%s' option'\n",
	       NspFname(stack),oarg,option);
      return RET_BUG;
    }
  for ( i = 0 ; i < inarg ; i++ )
    {
      if ((M[i] = GetRealMat(stack,i+2)) == NULLMAT) return RET_BUG;
    }
  for ( i = 1 ; i < inarg ; i++) 
    if ( M[i]->m != M[i-1]->m || M[i]->n != M[i-1]->n) 
      {
	Scierror("%s %s must have same size\n",NspFname(stack),errnames);
	return RET_BUG;
      } 
  for ( i = 0 ; i < oarg ; i++) 
    {
      if ((M[i+inarg] = nsp_matrix_create(NVOID,'r',M[0]->m,M[0]->n))== NULLMAT) return RET_BUG;
    }
  switch ( inarg+oarg) 
    {
    case 5:
      for ( i=0 ; i < M[0]->mn ; i++) 
	{
	  (*fonc)(&which,M[pos[0]]->R+i,M[pos[1]]->R+i,M[pos[2]]->R+i,M[pos[3]]->R+i,M[pos[4]]->R+i, &status,&bound);
	  if (status != 0) 
	    {
	      (*foncErr)(status,bound,pos); return RET_BUG;
	    }
	}
      break;
    case 6:
      for ( i=0 ; i < M[0]->mn ; i++) 
	{
	  (*fonc)(&which,M[pos[0]]->R+i,M[pos[1]]->R+i,M[pos[2]]->R+i,M[pos[3]]->R+i,M[pos[4]]->R+i,M[pos[5]]->R+i, &status,&bound);
	  if (status != 0) 
	    {
	      /** Scierror("i=%d\n",i); **/
	      (*foncErr)(status,bound,pos); return  RET_BUG;
	    }
	}
      break;
    case 4:
      for ( i=0 ; i <  M[0]->mn ; i++) 
	{
	  (*fonc)(&which,M[pos[0]]->R+i,M[pos[1]]->R+i, M[pos[2]]->R+i,M[pos[3]]->R+i, &status,&bound);
	  if (status != 0) 
	    {
	      (*foncErr)(status,bound,pos); return RET_BUG;
	    }
	}
      break;
    }

  for ( i = 0 ; i < Min(oarg,lhs) ; i++) 
    { 
      NthObj(inarg+2+i) = NSP_OBJECT( M[i+inarg]);  NthObj(inarg+2+i)->ret_pos = i+1;
    }
  /* just free unreturned matrices */
  for ( i = Min(oarg,lhs); i < oarg ; i++) 
    {
      nsp_matrix_destroy(M[i+inarg]);
    }
  return Min(oarg,lhs);
}




  
