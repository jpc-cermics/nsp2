#include "integ.h"

/*
 * Simulation of non linear recurrence equations of type 
 * x[k+1]=f(k,xk) 
 *
 * first provide a subroutine of the form.. 
 *              subroutine f (neq, t, y, ydot) 
 *              dimension y(neq), ydot(neq) 
 * which supplies the vector function f by loading ydot(i) with f(i). 
 * 
 *f      = name of subroutine for right-hand side vector f. 
 *         this name must be declared external in calling program. 
 *neq    = number of first order ode-s. 
 *y      = array of initial values, of length neq. 
 *t      = the initial value of the independent variable. 
 *tout   = first point where output is desired 
 *istate = 2  if lsdisc was successful, negative otherwise. 
 * 
 */

int
nsp_ode_lsdisc (ode_f f, int *neq, double *y, double *t, double *tout,
		   double *rwork, int *lrw, int *istate,void * param)
{
  double tt;
  int i1,  j,  itout, it;
  int c1 = 1;

  --y;
  --rwork;

  it = (int) (*t);
  itout = (int) (*tout);

  ierode_1.iero = 0;
  if (itout < it)
    {
      *istate = -3;
      return 0;
    }
  else if (itout == it)
    {
      *istate = 2;
      return 0;
    }
  else
    {
      i1 = itout - 1;
      for (j = it; j <= i1; ++j)
	{
	  tt = (double) j;
	  (*f) (neq, &tt, &y[1], &rwork[1], param);
	  if (ierode_1.iero > 0)
	    {
	      *istate = -4;
	      return 0;
	    }
	  C2F (dcopy) (neq, &rwork[1], &c1, &y[1], &c1);
	}
      *t = *tout;
      *istate = 2;
      return 0;
    }
  return 0;
}

