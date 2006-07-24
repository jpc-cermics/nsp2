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


/***************************************************
 * Interface for testing mex interfaces function 
 ***************************************************/

#include <math.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h> /* isxxxx */

#include "nsp/interf.h"
#include "nsp/datas.h"
#define MEXLIB
#include "mex/mex.h"

static void abcopy (const double a[],double b[],int mn);

/*
 * Example of mexfunction 
 */


static void abcopy(const double a[], double b[], int mn);  

static void mexFunction(int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[]) 
{
  const mxArray *ptrA;
  mxArray *ptrB;
  double *A,*B;
  int m,n,strl;
  char *str;
  mexPrintf("%s\n"," My message");
  if (nrhs != 2) {
    mexErrMsgTxt("Two inputs are required!");
  }
  ptrA = prhs[0];
  if ( !mxIsNumeric(ptrA)) {
    mexErrMsgTxt("Invalid first parameter!");
  }
  m = mxGetM(ptrA);
  n = mxGetN(ptrA);
  A = mxGetPr(ptrA);
  if (!mxIsString(prhs[1])) {
    mexErrMsgTxt("Invalid second parameter!");
  }
  ptrB = mxCreateFull(n,m,0);
  B = mxGetPr(ptrB);
  abcopy(A,B,m*n);
  plhs[0] = ptrB;
  strl = mxGetN(prhs[1]);
  str = mxCalloc(strl+1,sizeof(char));
  mxGetString(prhs[1],str,strl);
  plhs[1]=mxCreateString(str);
  if (nlhs > 2) {
    mexErrMsgTxt("At most 2 outputs!");
  }
}  

static void mexFunction1(int nlhs,mxArray *plhs[],int nrhs,const mxArray *prhs[])
{
 int      rows=2, cols=4; 
 double   pr_data[] = {5.2, 7.9, 1.3, 4.2, 6.7, 5.9, 1.9, 9.4};
 double   pi_data[] = {3.4, 6.5, 2.2, 9.1, 8.3, 4.7, 2.5, 7.5};
 double  *start_of_pr, *start_of_pi;
 mxArray *array_ptr;

 /* Create a 2-by-4 real double matrix named "B". */ 
 array_ptr = mxCreateFull(rows, cols, 1 );

 /* Populate the real part of the created array. */ 
 start_of_pr = (double *)mxGetPr(array_ptr);
 memcpy(start_of_pr, pr_data, rows * cols * sizeof(double) );

 /* Populate the imaginary part of the created array.  */
 start_of_pi = (double *)mxGetPi(array_ptr);
 memcpy(start_of_pi, pi_data, rows * cols * sizeof(double) );
 plhs[0] = array_ptr;
}


/*
 * called function 
 */

static void abcopy(const double a[], double b[], int mn)
{
  int i;
  for ( i=0 ; i < mn ; i++)  b[i] = 3.0 +a[i];
}


/*
 * FIXME: need to define a structure for storing 
 * interface which can be accessed from different wrappers 
 * here we just provide a table for mex interfaces 
 */

typedef  struct  {
  char *name;
  mexfun *fonc;
} nsp_mex_tab ;


static nsp_mex_tab mex_func[]={
  {"mexfun", mexFunction},
  {"mexfun1", mexFunction1},
  {(char *) 0, NULL}
};

int mex_Interf(int i, Stack stack, int rhs, int opt, int lhs)
{
  return nsp_mex_wrapper(stack,rhs,opt,lhs,mex_func[i].fonc);
}

/*
 * used to walk through the interface table 
 * (for adding or removing functions) 
 */

void mex_Interf_Info(int i, char **fname, function (**f))
{
  *fname = mex_func[i].name;
  *f = (function *) mex_func[i].fonc;
}







