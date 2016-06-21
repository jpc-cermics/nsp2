/* IB01OY.f -- translated by f2c (version 19961017).
 *   
 *   -lf2c -lm (in that order)
 */

#include "slicot.h"

/* Subroutine */ int
nsp_slicot_ib01oy (int *ns, int *nmax, int *n, double *sv, int *info)
{
  /* System generated locals */
  int i__1;
  /* Local variables */
  char ans[2];
  int yes;

  /* 
   *    SLICOT RELEASE 5.0. 
   * 
   *    Copyright (c) 2002-2009 NICONET e.V. 
   * 
   *    This program is free software: you can redistribute it and/or 
   *    modify it under the terms of the GNU General Public License as 
   *    published by the Free Software Foundation, either version 2 of 
   *    the License, or (at your option) any later version. 
   * 
   *    This program is distributed in the hope that it will be useful, 
   *    but WITHOUT ANY WARRANTY; without even the implied warranty of 
   *    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the 
   *    GNU General Public License for more details. 
   * 
   *    You should have received a copy of the GNU General Public License 
   *    along with this program.  If not, see 
   *    <http://www.gnu.org/licenses/>. 
   * 
   *    PURPOSE 
   * 
   *    To ask for user's confirmation of the system order found by 
   *    SLICOT Library routine IB01OD. This routine may be modified, 
   *    but its interface must be preserved. 
   * 
   *    ARGUMENTS 
   * 
   *    Input/Output Parameters 
   * 
   *    NS      (input) INT 
   *            The number of singular values.  NS > 0. 
   * 
   *    NMAX    (input) INT 
   *            The maximum value of the system order.  0 <= NMAX <= NS. 
   * 
   *    N       (input/output) INT 
   *            On entry, the estimate of the system order computed by 
   *            IB01OD routine.  0 <= N <= NS. 
   *            On exit, the user's estimate of the system order, which 
   *            could be identical with the input value of  N. 
   *            Note that the output value of  N  should be less than 
   *            or equal to  NMAX. 
   * 
   *    SV      (input) DOUBLE PRECISION array, dimension ( NS ) 
   *            The singular values, in descending order, used for 
   *            determining the system order. 
   * 
   *    Error Indicator 
   * 
   *    INFO    INT 
   *            = 0:  successful exit; 
   *            < 0:  if INFO = -i, the i-th argument had an illegal 
   *                  value. 
   * 
   *    CONTRIBUTORS 
   * 
   *    V. Sima, Research Institute for Informatics, Bucharest, Aug. 1999. 
   * 
   *    REVISIONS 
   * 
   *    - 
   * 
   *    KEYWORDS 
   * 
   *    Identification, parameter estimation, singular values, structure 
   *    identification. 
   * 
   * ********************************************************************* 
   * 
   *    .. Parameters .. 
   *       INTRMN is the unit number for the (terminal) input device. 
   *       OUTRMN is the unit number for the (terminal) output device. 
   *    .. 
   *    .. Scalar Arguments .. 
   *    .. 
   *    .. Array Arguments .. 
   *    .. 
   *    .. Local Scalars .. 
   *    .. 
   *    .. External Functions .. 
   *    .. 
   *    .. External Subroutines .. 
   * 
   *    .. Executable Statements .. 
   * 
   *    Check the scalar input parameters. 
   * 
   */
  /* Parameter adjustments */
  --sv;

  /* Function Body */
  *info = 0;
  if (*ns <= 0)
    {
      *info = -1;
    }
  else if (*nmax < 0 || *nmax > *ns)
    {
      *info = -2;
    }
  else if (*n < 0 || *n > *ns)
    {
      *info = -3;
    }
  /* 
   */
  if (*info != 0)
    {
      i__1 = -(*info);
      C2F (xerbla) ("IB01OY", &i__1, 6L);
      return 0;
    }
  {
    int i;
    Sciprintf("Singular values (in descending order) used to estimate the system order:\n");
    for ( i = 1; i <= *ns ; i++)
      {
	Sciprintf("%15.6f ",sv[i]);
      }
    Sciprintf("\nEstimated order of the system,  n =%d\n", *n);
    Sciprintf("Do you want this value of n to be used to determine the system matrices?\n");
    
  L10:
    Sciprintf("Type \"yes\" or \"no\": ");
    strcpy(ans,"Y"); /* XXXX */
    yes = C2F (lsame) (ans, "Y", 1L, 1L);
    if (yes)
      {
	if (*n <= *nmax)
	  {
	    /* 
	     *             The value of n is adequate and has been confirmed. 
	     */
	    return 0;
	  }
	else
	  {
	    /* 
	     *             The estimated value of n is not acceptable. 
	     */
	    Sciprintf(" n  should be less than or equal to %d\n",*nmax);
	    goto L20;
	  }
	/* 
	 */
      }
    else if (C2F (lsame) (ans, "N", 1L, 1L))
      {
	goto L20;
      }
    else
      {
	/* 
	 *          Wrong answer should be re-entered. 
	 * 
	 */
	goto L10;
      }
    /* 
     *    Enter the desired value of n. 
     * 
     */
  L20:
    Sciprintf("Enter the desired value of n (n <= %s); n= ", *nmax);
    *n = 0; /* XXXXX: to be done */
    if (*n < 0)
      {
	/* 
	 *          The specified value of n is not acceptable. 
	 * 
	 */
	Sciprintf("n  should be larger than zero.\n");
	goto L20;
      }
    else if (*n > *nmax)
      {
	/* 
	 *          The specified value of n is not acceptable. 
	 * 
	 */
	Sciprintf("n should be less than or equal to %d", *nmax);
	goto L20;
      }
    /* 
     */
    return 0;
    /* 
  **** Last line of IB01OY *** 
  */
  }				/* nsp_slicot_ib01oy */
}
