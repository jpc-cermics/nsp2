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
 *   cdflib (translated in C by Jpc). dinvr is a search routine 
 *   for bracketing a zero of a function f and when done it uses 
 *   dzror to find accurately a zero of f (Notes that nsp_zsearch 
 *   contains internally the "dzror part"). This second part 
 *   (find accurately a bracketed zero of f) is done using :
 *     Algorithm R of the paper 'Two Efficient Algorithms with
 *     Guaranteed Convergence for Finding a Zero of a Function'
 *     by J. C. P. Bus and T. J. Dekker in ACM Transactions on
 *     Mathematical Software, Volume 1, no. 4 page 330
 *     (Dec. '75)
 *
 *   Both dinvr and nsp_zsearch work using "reverse communication"
 *   which means that they don't compute internally f(x) (no
 *   function pointer is provided to the routine) but
 *   asks a caller loop to compute f on x until success or failure.
 *   (see "how to use this stuff" for an example).
 *    
 *   There are slight differences between this new routine 
 *   (nsp_zsearch) and dinvr which results in :
 *      * 3 or 4 f evaluations avoided with nsp_zsearch
 *        (moreover nsp_zsearch is also faster due to its
 *         structure).
 *      * nsp_zsearch code is hopefully better understandable
 *      * in some cases nsp_zsearch can find a zero where dinvr
 *        fails. 
 *
 *   The main difference is the following: dinvr searches on the
 *   interval [small, big] and first evaluates f on x = small and x = big
 *   to determine the "monotonicity" of the function. Then it
 *   starts from x = xinit and depending on f(xinit) and on the "monotonicity"
 *   searches on the left or on the right trying to bracket a zero. 
 *   When a bracket interval [b, c] is found  dinvr calls dzror but 
 *   without using the fact that the function is known at b and c
 *   (so there are 2 supplementary call to f).
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
 *   The explanation is that the function is not monotone
 *   and that 
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
 *   stat = nsp_zsearch_init(x, left_bound, right_bound, absstp, relstp, stpmul, 
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
 *         the search (on the left from a) was not able to bracket a zero
 *         (which doesn't mean that there is not a zero...)
 *         break;
 *      case RIGHT_BOUND_EXCEEDED:
 *         the search (on the right from a) was not able to bracket a zero
 *         (which doesn't mean that there is not a zero...)
 *         break;
 *      }
 *       
 */
int nsp_zsearch_init(double xinit, double left_bound, double right_bound, double absstp, double relstp, double stpmul, 
		     double abstol, double reltol, zsearch_monotonicity monotonicity, ZsearchStruct *S)
{
  if ( ! ( left_bound < xinit && xinit < right_bound ) )
    return -1;
  S->a = xinit;
  S->left_bound = left_bound;
  S->right_bound = right_bound;
  S->absstp = absstp;
  S->relstp = relstp;
  S->stpmul = stpmul;
  S->abstol = abstol;
  S->reltol = reltol;
  S->monotonicity = monotonicity;
  S->state = START;
  return 0;
}

zsearch_ret static init_bracketed_state(ZsearchStruct *S, double *x)
{
  /* to be documented
   */
  double tol, p, q, m, mb, w;
  S->ext = 0;
  if ( fabs(S->fc) < fabs(S->fb) )
    {
      /* swap b and c */
      double temp;
      temp = S->b; S->b = S->c, S->c = temp;
      temp = S->fb; S->fb = S->fc, S->fc = temp;
    }
  S->a = S->c; S->fa = S->fc;
  tol = 0.5* Max ( fabs(S->b)*S->reltol, S->abstol );
  m = 0.5*(S->b + S->c);
  mb = m - S->b;
  if ( mb < 0.0 ) tol = -tol;
  p = (S->b - S->a)*S->fb;
  q = S->fa - S->fb;
  if ( p < 0.0) { p = -p; q = -q; }
  w =  (p == 0.0 || p <= q*tol) ? tol : p < mb*q ? p/q : mb;
  S->middle_step = w == mb;
  S->d = S->a; S->fd = S->fa; 
  S->a = S->b; S->fa = S->fb;
  S->b += w;
  *x = S->b;
  return EVAL_FX;
}     


zsearch_ret nsp_zsearch(double *x, double fx, ZsearchStruct *S)
{
  switch ( S->state )
    {
    case START:
      S->fa = fx;
      S->step = S->absstp + S->relstp*fabs(S->a);
      if ( S->monotonicity == UNKNOWN )
	{
	  S->state = DETERMINE_SEARCH_DIR;
	  *x = Min( S->a + S->step, S->right_bound);
	}
      else if ( ( fx <= 0  &&  S->monotonicity == INCREASING ) ||
		( fx >= 0  &&  S->monotonicity == DECREASING ) )
	{
	  S->state = SEARCH_RIGHT;
	  S->b =  S->a; S->fb = S->fa;
	  *x = Min(S->a + S->step, S->right_bound);
	}
      else
	{
	  S->state = SEARCH_LEFT;
	  S->c = S->a; S->fc =  S->fa;
	  *x = Max(  S->a - S->step, S->left_bound);
	}
      return EVAL_FX;

    case DETERMINE_SEARCH_DIR:
      S->monotonicity = fx >= S->fa ? INCREASING : DECREASING;
      if ( (fx <= 0 && S->fa >= 0) || (fx >= 0 && S->fa <= 0) )
	{
	  S->state = BRACKETED;
	  if ( S->monotonicity == INCREASING )
	    {
	      S->b = S->a; S->fb = S->fa; S->c = *x; S->fc = fx;
	    }
	  else
	    {
	      S->b = *x; S->fb = fx; S->c = S->a; S->fc = S->fa;
	    }	      
	  return init_bracketed_state(S, x);
	}
      else
	{
	  S->step *= S->stpmul;
	  if ( ( fx <= 0  &&  S->monotonicity == INCREASING ) ||
	       ( fx >= 0  &&  S->monotonicity == DECREASING ) )
	    {
	      S->state = SEARCH_RIGHT;
	      S->b = *x; S->fb = fx;
	      *x = Min( *x + S->step, S->right_bound);
	    }
	  else
	    {
	      S->state = SEARCH_LEFT;
	      S->c = S->a; S->fc = S->fa;
	      *x = Max( S->a - S->step, S->left_bound);
	    }
	  return EVAL_FX;
	}
      
    case SEARCH_RIGHT:
      S->step *= S->stpmul;
      if ( ((S->monotonicity == INCREASING) && ( fx >= 0)) ||
	   ((S->monotonicity == DECREASING) && ( fx <= 0)) )
	{
	  S->state = BRACKETED;
	  S->c = *x; S->fc = fx;
	  return init_bracketed_state(S, x);
	}
      else
	{
	  if ( *x == S->right_bound )
	    return RIGHT_BOUND_EXCEEDED;
	  /* ici on pourrait tester la non monotonie */
	  S->b = *x; S->fb = fx;
	  *x = Min( *x + S->step, S->right_bound);
	  return EVAL_FX;
	}
      
    case SEARCH_LEFT:
      S->step *= S->stpmul;
      if ( ((S->monotonicity == INCREASING) && ( fx <= 0)) ||
	   ((S->monotonicity == DECREASING) && ( fx >= 0)) )
	{
	  S->state = BRACKETED;
	  S->b = *x; S->fb = fx;
	  return init_bracketed_state(S, x);
	}
      else
	{
	  if ( *x == S->left_bound )
	    return LEFT_BOUND_EXCEEDED;
	  /* ici on pourrait tester la non monotonie */
	  S->c = *x; S->fc = fx;
	  *x = Max( *x - S->step, S->left_bound);
	  return EVAL_FX;
	}
      
    case BRACKETED:
      {
	double tol, m, mb, p, q, w, fdb, fda;
  
	S->fb = fx;
	if ( (S->fc >= 0 && S->fb >= 0) || (S->fc <= 0 && S->fb <= 0) )
	  {
	    S->c = S->a; S->fc = S->fa; S->ext = 0; /* interpolate */
	  }
	else
	  {
	    S->ext = S->middle_step ? 0 : S->ext+1;
	  }
	
	if ( fabs(S->fc) <  fabs(S->fb) )
	  {
	    if ( S->c != S->a ) { S->d = S->a; S->fd = S->fa; }
	    S->a = S->b; S->fa = S->fb;
	    S->b = S->c; S->fb = S->fc;
	    S->c = S->a; S->fc = S->fa;
	  }
	tol = 0.5* Max ( fabs(S->b)*S->reltol, S->abstol );
	m = 0.5*(S->b + S->c); mb = m - S->b;
	
	if ( fabs(mb) <= tol )
	  {
	    *x = S->b;
	    if ( S->b > S->c )
	      {
		S->a = S->b; S->fa = S->fb;
		S->b = S->c; S->fb = S->fc;
		S->c = S->a; S->fc = S->fa;
	      }
	    return SUCCESS;
	  }
	
	if ( S->ext > 3 )
	  w = mb;
	else
	  {
	    if ( mb < 0.0 ) tol = -tol;
	    p = (S->b - S->a)*S->fb;
	    fdb = (S->fd - S->fb)/(S->d - S->b);
	    fda = (S->fd - S->fa)/(S->d - S->a);	
	    p = fda*p; q = fdb*S->fa - fda*S->fb;
	    if ( p < 0.0 ) { p = -p; q = -q; }
	    if ( S->ext == 3 ) p*= 2.0;
	    w = (p==0 || p <= q*tol) ? tol : p < mb*q ? p/q : mb;
	  }
	S->middle_step = w == mb;
	S->d = S->a; S->fd = S->fa; 
	S->a = S->b; S->fa = S->fb;
	S->b += w;
	*x = S->b;
	return EVAL_FX;
      }
    }
  return OTHER_FAILURE;  /* we should never pass here */   
}



