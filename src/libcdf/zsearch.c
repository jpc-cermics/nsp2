/* Nsp
 * Copyright (C) 2010 Bruno Pincon Esial/Iecn
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

#include "cdf.h"

     
/*
 *   This routine may replace the old dinvr.c routine from
 *   cdflib (translated in C by Jpc). dinvr is simply a 
 *   search routine for bracketing a zero of a function f and 
 *   when done it uses dzror to find accurately a zero of f.
 *   Both dinvr and nsp_zsearch work using "reverse communication"
 *   which means that they don't compute internally f(x) (no
 *   function pointer is provided to the routine) but
 *   asks a caller loop to compute f on x until success or failure.
 *   (see "how to use this stuff" for an example).
 *    
 *   There are slight differences between this new routine 
 *   (nsp_zsearch) and dinvr which results in :
 *      * 3 or 4 f evaluations avoided with nsp_zsearch 
 *      * nsp_zsearch code is hopefully better understandable
 *      * in some cases nsp_zsearch can find a zero where dinvr
 *        fails.
 *
 *   The main difference is the following: dinvr searches on the
 *   interval [small, big] and evals f on x = small and x = big
 *   to determine the "monotonicity" of the function. Then it
 *   starts from xinit and depending on f(xinit) and on the "monotonicity"
 *   searches on the left or on the right from xinit trying to bracket 
 *   a zero. When a bracket interval [xlb, xhi] is found  dinvr calls dzror but 
 *   without using the fact that the function is known at xlb and xhi.
 *
 *   nsp_zsearch can be called by specifying the function monotonicity
 *   (INCREASING, DECREASING, UNKNOWN). In case the monotonicity is
 *   known (INCREASING, DECREASING), the search direction can be determined 
 *   once f(xinit) has been computed. Otherwise a first positive step is 
 *   done and f(xinit) and f(xinit + step) are used to determine the 
 *   (somewhat local) monotonicity. In case the function is such that 
 *   f(small) and f(big) are together positive or negative, dinvr stops 
 *   while nsp_zsearch can at least begin a search. So can work when f 
 *   is not a monotone function. dinvr was sometimes used this way, in
 *   particular:
 * 
 *   [p,q]=cdff("PQ", 7.460435, 5, 7);
 *    f=cdff("F", 5, 7, p, q);
 *    cdff("Dfn", 7, p, q, f)  // => bug
 *
 *   The explanation is that the function is not monotone:
 *
 *    dfn = linspace(0.00001,7,5000)';
 *    v = ones(size(dfn));
 *    [p,q]=cdff("PQ", 7.460435*v, dfn, 7*v);
 *    plot2d(dfn,p,style=2)
 *
 *   and there are 2 possible values for dfn. On this particular
 *   case nsp_zsearch finds one of the 2 roots (5).    
 *
 *   how to use this stuff:
 *   ----------------------
 *
 *   ZsearchStruct S;
 *   double x = xinit;
 *   stat = nsp_zsearch_init(x, small, big, absstp, relstp, stpmul, 
 *		             abstol, reltol, monotonicity, &S)
 *   do
 *     {
 *        fx = f(x);  compute f on x
 *        ret_val = nsp_zsearch(&x, fx, &S);
 *     }
 *    while ( ret_val == EVAL_FX );
 *    switch( ret_val )
 *      {
 *      case SUCCESS:
 *         x is the approximated root (additionnal information in S)
 *         break;
 *      case LEFT_BOUND_EXCEEDED:
 *         the search (on the left from xinit) was not able to bracket a zero
 *         (which doesn't mean that there is not a zero...)
 *         break;
 *      case RIGHT_BOUND_EXCEEDED:
 *         the search (on the right from xinit) was not able to bracket a zero
 *         (which doesn't mean that there is not a zero...)
 *         break;
 *      }
 *       
 */
int nsp_zsearch_init(double xinit, double small, double big, double absstp, double relstp, double stpmul, 
		     double abstol, double reltol, zsearch_monotonicity monotonicity, ZsearchStruct *S)
{
  if ( ! ( small < xinit && xinit < big ) )
    return -1;
  S->xinit = xinit;
  S->small = small;
  S->big = big;
  S->absstp = absstp;
  S->relstp = relstp;
  S->stpmul = stpmul;
  S->abstol = abstol;
  S->reltol = reltol;
  S->monotonicity = monotonicity;
  S->state = START;
  return 0;
}


zsearch_ret static init_dzror(ZsearchStruct *S, double *x)
{
  /* this routine inits the dzror parameters and uses the fact 
   * that the two first f evaluations for dzror
   * are for xlb and xhi but f(xlb) and f(xhi) are known. 
   */
  double fx;
  int status = 0, dum1, dum2;
  cdf_dstzr (&(S->xlb), &(S->xhi), &(S->abstol), &(S->reltol));
  cdf_dzror (&status, x, &fx, &(S->xlb), &(S->xhi), &dum1, &dum2);
  fx = S->fxlb;
  cdf_dzror (&status, x, &fx, &(S->xlb), &(S->xhi), &dum1, &dum2);
  fx = S->fxhi;
  cdf_dzror (&status, x, &fx, &(S->xlb), &(S->xhi), &dum1, &dum2);
  return EVAL_FX;
}     


zsearch_ret nsp_zsearch(double *x, double fx, ZsearchStruct *S)
{
  switch ( S->state )
    {
    case START:
      S->fxinit = fx;
      S->step = S->absstp + S->relstp*fabs(S->xinit);
      if ( S->monotonicity == UNKNOWN )
	{
	  S->state = DETERMINE_SEARCH_DIR;
	  *x = Min( S->xinit + S->step, S->big);
	}
      else if ( ( fx <= 0  &&  S->monotonicity == INCREASING ) ||
		( fx >= 0  &&  S->monotonicity == DECREASING ) )
	{
	  S->state = SEARCH_RIGHT;
	  S->xlb =  S->xinit; S->fxlb = S->fxinit;
	  *x = Min(S->xinit + S->step, S->big);
	}
      else
	{
	  S->state = SEARCH_LEFT;
	  S->xhi = S->xinit; S->fxhi =  S->fxinit;
	  *x = Max(  S->xinit - S->step, S->small);
	}
      return EVAL_FX;

    case DETERMINE_SEARCH_DIR:
      S->monotonicity = fx >= S->fxinit ? INCREASING : DECREASING;
      if ( (fx <= 0 && S->fxinit >= 0) || (fx >= 0 && S->fxinit <= 0) )
	{
	  S->state = BRACKETED;
	  if ( S->monotonicity == INCREASING )
	    {
	      S->xlb = S->xinit; S->fxlb = S->fxinit; S->xhi = *x; S->fxhi = fx;
	    }
	  else
	    {
	      S->xlb = *x; S->fxlb = fx; S->xhi = S->xinit; S->fxhi = S->fxinit;
	    }	      
	  return init_dzror(S, x);
	}
      else
	{
	  S->step *= S->stpmul;
	  if ( ( fx <= 0  &&  S->monotonicity == INCREASING ) ||
	       ( fx >= 0  &&  S->monotonicity == DECREASING ) )
	    {
	      S->state = SEARCH_RIGHT;
	      S->xlb = *x; S->fxlb = fx;
	      *x = Min( *x + S->step, S->big);
	    }
	  else
	    {
	      S->state = SEARCH_LEFT;
	      S->xhi = S->xinit; S->fxhi = S->fxinit;
	      *x = Max( S->xinit - S->step, S->small);
	    }
	  return EVAL_FX;
	}
      
    case SEARCH_RIGHT:
      S->step *= S->stpmul;
      if ( ((S->monotonicity == INCREASING) && ( fx >= 0)) ||
	   ((S->monotonicity == DECREASING) && ( fx <= 0)) )
	{
	  S->state = BRACKETED;
	  S->xhi = *x; S->fxhi = fx;
	  return init_dzror(S, x);
	}
      else
	{
	  if ( *x == S->big )
	    return RIGHT_BOUND_EXCEEDED;
	  /* ici on pourrait tester la non monotonie */
	  S->xlb = *x; S->fxlb = fx;
	  *x = Min( *x + S->step, S->big);
	  return EVAL_FX;
	}
      
    case SEARCH_LEFT:
      S->step *= S->stpmul;
      if ( ((S->monotonicity == INCREASING) && ( fx <= 0)) ||
	   ((S->monotonicity == DECREASING) && ( fx >= 0)) )
	{
	  S->state = BRACKETED;
	  S->xlb = *x; S->fxlb = fx;
	  return init_dzror(S, x);
	}
      else
	{
	  if ( *x == S->small )
	    return LEFT_BOUND_EXCEEDED;
	  /* ici on pourrait tester la non monotonie */
	  S->xhi = *x; S->fxhi = fx;
	  *x = Max( *x - S->step, S->small);
	  return EVAL_FX;
	}
      
    case BRACKETED:
      {
	int qdum1, qdum2, status; 
	cdf_dzror (&status, x, &fx,  &(S->xlb), &(S->xhi), &qdum1, &qdum2);
	if ( status != 1 )
	  {
	    *x = S->xlb;
	    if ( S->xlb > S->xhi )
	      {
		double temp = S->xlb;
		S->xlb = S->xhi;  S->xhi = temp;
	      }
	    return SUCCESS;
	  }
	else
	  return EVAL_FX; 
      }
    }
  return OTHER_FAILURE;  /* we should never pass here */   
}



