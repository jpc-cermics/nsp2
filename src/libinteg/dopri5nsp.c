#include <stdlib.h>
#include <string.h>
#include <float.h>

#include "nsp/math.h"
#include "nsp/ode_solvers.h"

/*
 * This file correspond to a translation (in C language)/modifications
 * by Bruno Pincon of the code dopri5.f by Ernst Hairer. The original 
 * file is subject to the following license:
 *
 * Copyright (c) 2004, Ernst Hairer
 *
 * Redistribution and use in source and binary forms, with or without 
 * modification, are permitted provided that the following conditions are 
 * met:
 * 
 * - Redistributions of source code must retain the above copyright 
 * notice, this list of conditions and the following disclaimer.
 * 
 * - Redistributions in binary form must reproduce the above copyright 
 * notice, this list of conditions and the following disclaimer in the 
 * documentation and/or other materials provided with the distribution.
 * 
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS -Y´AS 
 * IS¡ AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED 
 * TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A 
 * PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE REGENTS OR 
 * CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, 
 * EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, 
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR 
 * PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF 
 * LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING 
 * NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS 
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 *   
 * Modifications (by B. Pincon) are essentially:
 *
 *   - some simplifications: 
 *        * some statistics (like the nb of rejected steps, the nb of
 *          rhs function calls , etc..) are not managed; 
 *        * when a solution is interpolated all the solution components are 
 *          calculated (interpolation coef are computed only if needed).
 *  
 *   - the nsp_dopri5 routine manages itself the recording of the solution depending
 *     of the parameter task (see nsp_dopri5 comments).
 *
 *   - internal memory needs are managed by nsp_dopri5
 *
 * The new resulting file are subject to the same license.
 * Copyright (c) 2004, Ernst Hairer
 * Copyright (c) 2009, Bruno Pincon
 *     
 */

static void coef_dopri5(double *c2, double *c3, double *c4, double *c5, 
		 double *e1, double *e3, double *e4, double *e5, double *e6, double *e7,
                 double *a21, double *a31, double *a32, double *a41, double *a42, double *a43, 
		 double *a51, double *a52, double *a53, double *a54,
                 double * a61, double *a62, double *a63, double *a64, double *a65, 
		 double *a71, double *a73, double *a74, double *a75, double *a76,
		 double *d1, double *d3, double *d4, double *d5, double *d6, double *d7)
{
  /* ----------------------------------------------------------
   *    runge-kutta coefficients of dormand and prince (1980)
   * ----------------------------------------------------------*/
  *c2=0.2;
  *c3=0.3;
  *c4=0.8;
  *c5=8.0/9.0;
  *a21=0.2;
  *a31=3.0/40.0;
  *a32=9.0/40.0;
  *a41=44.0/45.0;
  *a42=-56.0/15.0;
  *a43=32.0/9.0;
  *a51=19372.0/6561.0;
  *a52=-25360.0/2187.0;
  *a53=64448.0/6561.0;
  *a54=-212.0/729.0;
  *a61=9017.0/3168.0;
  *a62=-355.0/33.0;
  *a63=46732.0/5247.0;
  *a64=49.0/176.0;
  *a65=-5103.0/18656.0;
  *a71=35.0/384.0;
  *a73=500.0/1113.0;
  *a74=125.0/192.0;
  *a75=-2187.0/6784.0;
  *a76=11.0/84.0;
  *e1=71.0/57600.0;
  *e3=-71.0/16695.0;
  *e4=71.0/1920.0;
  *e5=-17253.0/339200.0;
  *e6=22.0/525.0;
  *e7=-1.0/40.0;  
  /* ---- dense output of Shampine (1986)  */
  *d1=-12715105075.0/11282082432.0;
  *d3=87487479700.0/32700410799.0;
  *d4=-10690763975.0/1880347072.0;
  *d5=701980252875.0/199316789632.0;
  *d6=-1453857185.0/822651844.0;
  *d7=69997945.0/29380423.0;
}


/* ----------------------------------------------------------
 * ----  computation of an initial step size guess
 * ----------------------------------------------------------*/
static int hinit(int n, ode_f fcn, double x, const double *y, double xend, double posneg,
		 const double *f0, double *f1, double *y1, double hmax, 
		 const double *atol, double rtol, int itol, void *param, double *hinit)
{
  double h, h1, dnf = 0.0, dny = 0.0, sk, temp, der2, der12;
  int iord = 5, i;
  /*
   *  compute a first guess for explicit euler as  h = 0.01 * norm (y0) / norm (f0)
   *  the increment for explicit euler is small compared to the solution
   */
  if ( itol == 0 )
    {
      double atoli = atol[0];
      for ( i = 0 ; i < n ; i++ )
	{
	  sk = atoli + rtol*fabs(y[i]);
	  temp = f0[i]/sk; dnf += temp*temp;
	  temp = y[i]/sk; dny += temp*temp;
	}
    }
  else
    {
      for ( i = 0 ; i < n ; i++ )
	{
	  sk = atol[i] + rtol*fabs(y[i]);
	  temp = f0[i]/sk; dnf += temp*temp;
	  temp = y[i]/sk; dny += temp*temp;
	}
    }

  if ( dnf <= 1.e-10 || dny <= 1.e-10 )
    h = 1e-6;
  else
    h = sqrt(dny/dnf)*0.01;

  h = Min(h,hmax);
  h *= posneg;
 
  /* perform an explicit euler step */
  for ( i = 0 ; i < n ; i++ )
    y1[i] = y[i] + h*f0[i];

  temp = x + h;
  if ( fcn(&n, &temp, y1, f1, param) == FAIL ) 
    return FAIL;
 
  /* estimate the second derivative of the solution */
  der2 = 0.0;
  if ( itol == 0 )
    {
      double atoli=atol[0];
      for ( i = 0 ; i < n ; i++ )
	{
	  sk = atoli + rtol*fabs(y[i]);
	  temp = (f1[i]-f0[i])/sk; der2 += temp*temp;
	}
    }
  else
    {
      for ( i = 0 ; i < n ; i++ )
	{
	  sk = atol[i] + rtol*fabs(y[i]);
	  temp = (f1[i]-f0[i])/sk; der2 += temp*temp;
	}
    }

  der2 = sqrt(der2)/h;

  /* ---- step size is computed such that
   * ----  h^iord * max ( norm (f0), norm (der2)) = 0.01
   */
  der12 = Max(fabs(der2), sqrt(dnf));
  if (der12 <= 1.e-15)
    h1 = Max(1.0e-6,fabs(h)*1e-3);
  else
    h1 = pow( 0.01/der12 , 1.0/iord ); 
  h = Min( 100*fabs(h), Min(h1,hmax));

  *hinit = posneg*h;
  return OK;
}


static double stiffness_test(double *y1, double *ysti, double *k2, double *k6, double h, int n)
{
  double hlamb = 0.0, stnum = 0.0, stden = 0.0, temp;
  int i;
  for ( i = 0 ; i < n ; i++ )
    {
      temp = k2[i] - k6[i];
      stnum += temp*temp;
      temp = y1[i] - ysti[i];
      stden += temp*temp;
    }
  if ( stden > 0.0) 
    hlamb = h*sqrt(stnum/stden); 

  return hlamb;
}

static double error_estimation(double *y, double *y1, double *k4, const double *atol, double rtol, int itol, int n)
{
  int i;
  double err=0.0, sk, temp;
  
  if ( itol == 0 )
    {
      double atoli = atol[0];
      for ( i = 0 ; i < n ; i++ )
	{
	  sk = atoli + rtol*Max(fabs(y[i]),fabs(y1[i]));
	  temp = k4[i]/sk;
	  err += temp*temp;
	}
    }
  else
    for ( i = 0 ; i < n ; i++ )
      {
	sk = atol[i] + rtol*Max(fabs(y[i]),fabs(y1[i]));
	temp = k4[i]/sk;
	err += temp*temp;
      }
  
  return sqrt(err/n);
}

static void contd5(double xinterp, double x, double h, double *cont, int n, double *yinterp)
{
  /* compute solution at xinterp by interpolation between x and x+h */
  double u = (xinterp - x)/h, v = 1.0 - u;
  int i;

  for ( i = 0 ; i < n ; i++ )
    yinterp[i] = cont[i] + u*(cont[n+i] + v*(cont[2*n+i] + u*(cont[3*n+i] + v*cont[4*n+i])));
}

/*
 *   on return:
 *
 *    MALLOC_FAIL
 *    COMPLETED
 *    FCT_EVAL_FAIL
 *    STIFFNESS_DETECTED
 *    TIME_STEP_BECOME_TOO_SMALL
 *    MORE_THAN_MAXSTEP_NEEDED
 */

/**
 * nsp_dopri5:
 * @n: system dimension
 * @x: initial time
 * @y: initial solution
 * @xend: final time of integration
 * @hmax: maximum step size
 * @h0: initial step size on entry (if 0 it is computed internally), last step size on output
 * @rtol: relative error tolerance
 * @atol: absolute error tolerance (should be a scalar (itol=0) of a vector of size n (itol=1))
 * @itol: flag for scalar/vector atol
 * @maxstep: maximum number of steps
 * @nstiff: 
 * @safe: safety factor in step size prediction (default is 0.9)
 * @beta: parameter for stabilized step size control (default 0.04)
 * @fac1, @fac2: parameters for step size selection, the new step size is chosen subject 
 *               to the restriction fac1 <= hnew/hold <= fac2. default: fac1=0.2, fac2=10.
 * @task: the solver integrates the ode from x0 to xend taking steps hk such that the local
 *        error estimate is under the local error tolerance. So what is got is something
 *        like:
 *             (x0, y0) -> (x1=x0+h1, y1) -> ......(xk=x(k-1)+hk, yk) -> ...-> (xfinal, yfinal) 
 *        with xfinal the first time instant such that xfinal >= xend if xend > x0 or
 *        such that xfinal <= xend if xend < x0.
 * 
 *        task=1 means that the solution at the time instants xout[0..nout-1] should be 
 *        computed (by interpolation from y0, y1, ..., yfinal) and saved in yout (in this
 *        case xend should be set to xout[nout-1]).
 *        task=2 or 3 means that the solution at each step is saved in yout, together
 *        with the time instant xk in xout (moreover the initial solution and initial
 *        time are also saved in yout, xout). For task = 3 the last solution is obtained 
 *        by interpolation such that the final time is exactly xend.
 *        
 *   
 * 
 **/


int nsp_dopri5(int n, ode_f fcn, void *param, double x, double *y, double xend, double hmax,
	       double *h0, double rtol, const double *atol, int itol, int maxstep, 
	       int nstiff, double safe, double beta, double fac1, double fac2,  
	       int task, Boolean warning, double *yout, double *xout, int nout, int *noutrel)
{
  Boolean reject=FALSE, last=FALSE, interp;
  double c2, c3, c4, c5, e1, e3, e4, e5, e6, e7,
    a21, a31, a32, a41, a42, a43, a51, a52, a53, a54,
    a61, a62, a63, a64, a65, a71, a73, a74, a75, a76,
    d1, d3, d4, d5, d6, d7;
  double h=*h0, hnew, err, posneg, facold=1e-4, expo1, fac, facc1, facc2, fac11, xph, temp;
  double *work=NULL, *y1, *k1, *k2, *k3, *k4, *k5, *k6, *ysti, *cont = NULL;
  int nstep=0, i, jout=0, iasti=0, nonsti=0, naccpt=0, status, wsize;
  
  *noutrel = 0;
  /*  mem alloc  */
  if ( task == 2 )  /* no need of interpolation, so no need of cont array */
    wsize = 8*n;
  else              /* task == 1 or 3 need interpolation (when task=3 only for the last step) */
    wsize = 13*n;
  if ( ( work = malloc(wsize*sizeof(double)) ) == NULL )
    return MALLOC_FAIL;
  y1 = work; k1 = work + n; k2 = k1 + n; k3 = k2 + n; 
  k4 = k3 + n; k5 = k4 + n; k6 = k5 + n; ysti = k6 + n;
  if ( task != 2 ) cont = ysti + n;

  /*  initialisations  */
  coef_dopri5(&c2, &c3, &c4, &c5, &e1, &e3, &e4, &e5, &e6, &e7,
	      &a21, &a31, &a32, &a41, &a42, &a43, &a51, &a52, &a53, &a54,
	      &a61, &a62, &a63, &a64, &a65, &a71, &a73, &a74, &a75, &a76,
	      &d1, &d3, &d4, &d5, &d6, &d7);
  expo1 = 0.20 - beta*0.75;
  facc1 = 1.0/fac1;
  facc2 = 1.0/fac2;
  posneg = xend - x >= 0.0 ? 1.0 : -1.0;

  /*  initial preparations  */
  if ( fcn(&n, &x, y, k1, param) == FAIL )
    {
      status = FCT_EVAL_FAIL; goto err;
    }

  if ( nstiff <= 0 ) 
    nstiff = maxstep + 2;

  hmax = fabs(hmax);     
  if ( h == 0.0)
    {
      if ( hinit(n, fcn, x, y, xend, posneg, k1, k2, k3, hmax, atol, rtol, itol, param, &h) == FAIL )
	{
	  status = FCT_EVAL_FAIL; goto err;
	}
    }

  if ( task != 1 )  /* copy y0 and x0 in yout, tout */
    {
      memcpy(yout, y, n*sizeof(double));
      xout[0] = x;
      jout = 1;
    }

  /* the main loop */
  status = COMPLETED;
  do
    {
      if ( nstep > maxstep ) { status = MORE_THAN_MAXSTEP_NEEDED; break; }

      if ( fabs(h) < fabs(x)*DBL_EPSILON ) { status = TIME_STEP_BECOME_TOO_SMALL; break; }

      xph = x + h;
      if ( (xph - xend)*posneg >= 0.0) last = TRUE; 

      if ( (task == 1  &&  (xph - xout[jout])*posneg >= 0.0)  ||  (task == 3  &&  last) ) 
	interp = TRUE;
      else
	interp = FALSE;

      nstep++;

      /* --- the 6 stages --- */
      for ( i = 0 ; i < n ; i++ )
	y1[i] = y[i] + h*a21*k1[i];
      temp = x + c2*h;
      if ( fcn(&n, &temp, y1, k2, param) == FAIL ) { status = FCT_EVAL_FAIL; break; }

      for ( i = 0 ; i < n ; i++ )
	y1[i] = y[i] + h*(a31*k1[i] + a32*k2[i]);
      temp = x + c3*h;
      if ( fcn(&n, &temp, y1, k3, param) == FAIL ) { status = FCT_EVAL_FAIL; break; }

      for ( i = 0 ; i < n ; i++ )
	y1[i] = y[i] + h*(a41*k1[i] + a42*k2[i] + a43*k3[i]);
      temp = x+c4*h;
      if ( fcn(&n, &temp, y1, k4, param) == FAIL )  { status = FCT_EVAL_FAIL; break; }

      for ( i = 0 ; i < n ; i++ ) 
	y1[i] = y[i] + h*(a51*k1[i] +a52*k2[i] + a53*k3[i] + a54*k4[i]);
      temp = x + c5*h;
      if ( fcn(&n, &temp, y1, k5, param) == FAIL )  { status = FCT_EVAL_FAIL; break; }

      for ( i = 0 ; i < n ; i++ ) 
	ysti[i] = y[i] + h*(a61*k1[i] +a62*k2[i] + a63*k3[i] + a64*k4[i] +a65*k5[i]);

      if ( fcn(&n, &xph, ysti, k6, param) == FAIL )  { status = FCT_EVAL_FAIL; break; }

      for ( i = 0 ; i < n ; i++ ) 
	y1[i] = y[i] + h*(a71*k1[i]+a73*k3[i]+a74*k4[i]+a75*k5[i]+a76*k6[i]);
  
      if ( fcn(&n, &xph, y1, k2, param) == FAIL )  { status = FCT_EVAL_FAIL; break; }

      if ( interp )
	for ( i = 0 ; i < n ; i++ )
	  cont[4*n+i] = h*(d1*k1[i] + d3*k3[i] + d4*k4[i] + d5*k5[i] + d6*k6[i] + d7*k2[i]);
 
      for ( i = 0 ; i < n ; i++ )
	k4[i] = h*(e1*k1[i] + e3*k3[i] + e4*k4[i] + e5*k5[i] + e6*k6[i] + e7*k2[i]);


      /*  error estimation  */
      err = error_estimation(y, y1, k4, atol, rtol, itol, n);
      fac11 = pow(err,expo1);
  
      if (err <= 1.0)   /*  step is accepted  */  
	{
	  /*  computation of hnew  */
	  fac = fac11/pow(facold,beta);         /*  lund-stabilization  */
	  fac = Max(facc2,Min(facc1,fac/safe)); /*  we require fac1 <= hnew/h <= fac2  */
	  hnew=h/fac;

	  facold = Max(err,1e-4);
	  naccpt++;

	  if ( interp )  /* fill the array cont (a first part cont[4*n+i] has already been recorded) */
	    for ( i = 0 ; i < n ; i++ )
	      {
		cont[i] = y[i];
		cont[n+i] = y1[i] - y[i];
		cont[2*n+i] = h*k1[i] - cont[n+i];
		cont[3*n+i] = -h*k2[i] + cont[n+i] - cont[2*n+i];
	      }
	  
	  /*  write solution  */
	  if ( task == 2  ||  (task == 3  && ! last) )  
	    {
	      memcpy(yout + jout*n, y1, n*sizeof(double));
	      xout[jout] = xph;
	      jout++;
	    }
	  else if ( interp )
	    {
	      if ( task == 1 )
		while ( jout < nout  && (xph - xout[jout])*posneg >= 0.0 )
		  { 
		    contd5(xout[jout], x, h, cont, n, yout+n*jout); 
		    jout++; 
		  }
	      else  /* ( task == 3  &&  last ) */
		{
		  contd5(xend, x, h, cont, n, yout+n*jout); xout[jout] = xend;
		  jout++;
		}
	    }
		  
	  /*  stiffness detection  */
	  if ( naccpt % nstiff == 0  || iasti > 0 )
	    {
	      if ( stiffness_test(y1, ysti, k2, k6, h, n) > 3.25 )
		{
		  nonsti = 0; iasti++;
		  if (iasti == 15)
		    {
		      status = STIFFNESS_DETECTED;
		      break;
		    }
		}
	      else
		{
		  nonsti++;
		  if ( nonsti == 6 ) iasti = 0;
		}
	    }

	  memcpy(k1, k2, n*sizeof(double));
 	  memcpy(y, y1, n*sizeof(double));
	  x = xph;

	  if ( fabs(hnew) > hmax) 
	    hnew = posneg*hmax;
  
	  if( reject )  /* if last step was rejected take care to don't increase the magnitude of the time step */
	    {
	      hnew = posneg*Min(fabs(hnew),fabs(h));
	      reject = FALSE;
	    }
	}
      
      else   /*  step is rejected  */
	{   
	  /*  computation of hnew  */
	  fac = Min(facc1,fac11/safe);
	  hnew=h/fac;
	  reject = TRUE;  
	  last = FALSE;
	}
      
      h = hnew;
    }
  while ( ! last );
  
  *noutrel = jout;
  *h0 = h;
  free(work);

  if ( warning && status != COMPLETED )
    {
      switch ( status )
	{
	case MORE_THAN_MAXSTEP_NEEDED:
	  Sciprintf("dopri5: more than mxstep = %d are needed (integration stops at t = %g)\n",maxstep,x);
	  break;
	case TIME_STEP_BECOME_TOO_SMALL:
	  Sciprintf("dopri5: step size too small h = %g (integration stops at t = %g)\n",h,x);
	  break;
	case STIFFNESS_DETECTED:
	  Sciprintf("dopri5: the problem seems to become stiff at t = %g (integration stops here)\n",x);
	  break;
	case FCT_EVAL_FAIL:
	  Sciprintf("dopri5: a problem occurs when evaluating the rhs function (integration stops at t = %g)\n",x);
	  break;
	}
    }
  return status;

 err:
  free(work);
  return status;
}

