/* SB04RX.f -- translated by f2c (version 19961017).
 *   
 *   -lf2c -lm (in that order)
 */

#include "slicot.h"

/* Table of constant values */

static int c__2 = 2;
static double c_b6 = 0.;
static int c__1 = 1;

/* Subroutine */ int
nsp_slicot_sb04rx (char *rc, char *ul, int *m, double *a, int *lda,
		   double *lambd1, double *lambd2, double *lambd3,
		   double *lambd4, double *d__, double *tol,
		   int *iwork, double *dwork, int *lddwor,
		   int *info, long int rc_len, long int ul_len)
{
  /* System generated locals */
  int a_dim1, a_offset, dwork_dim1, dwork_offset, i__1, i__2, i__3;

  /* Local variables */
  double c__;
  int j;
  double r__, s;
  double rcond;
  char trans[1];
  int j1, j2, m2;
  int mj, ml;

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
   *    To solve a system of equations in quasi-Hessenberg form 
   *    (Hessenberg form plus two consecutive offdiagonals) with two 
   *    right-hand sides. 
   * 
   *    ARGUMENTS 
   * 
   *    Mode Parameters 
   * 
   *    RC      CHARACTER*1 
   *            Indicates processing by columns or rows, as follows: 
   *            = 'R':  Row transformations are applied; 
   *            = 'C':  Column transformations are applied. 
   * 
   *    UL      CHARACTER*1 
   *            Indicates whether A is upper or lower Hessenberg matrix, 
   *            as follows: 
   *            = 'U':  A is upper Hessenberg; 
   *            = 'L':  A is lower Hessenberg. 
   * 
   *    Input/Output Parameters 
   * 
   *    M       (input) INT 
   *            The order of the matrix A.  M >= 0. 
   * 
   *    A       (input) DOUBLE PRECISION array, dimension (LDA,M) 
   *            The leading M-by-M part of this array must contain a 
   *            matrix A in Hessenberg form. 
   * 
   *    LDA     INT 
   *            The leading dimension of array A.  LDA >= MAX(1,M). 
   * 
   *    LAMBD1, (input) DOUBLE PRECISION 
   *    LAMBD2, These variables must contain the 2-by-2 block to be 
   *    LAMBD3, multiplied to the elements of A. 
   *    LAMBD4 
   * 
   *    D       (input/output) DOUBLE PRECISION array, dimension (2*M) 
   *            On entry, this array must contain the two right-hand 
   *            side vectors of the quasi-Hessenberg system, stored 
   *            row-wise. 
   *            On exit, if INFO = 0, this array contains the two solution 
   *            vectors of the quasi-Hessenberg system, stored row-wise. 
   * 
   *    Tolerances 
   * 
   *    TOL     DOUBLE PRECISION 
   *            The tolerance to be used to test for near singularity of 
   *            the triangular factor R of the quasi-Hessenberg matrix. 
   *            A matrix whose estimated condition number is less 
   *            than 1/TOL is considered to be nonsingular. 
   * 
   *    Workspace 
   * 
   *    IWORK   INT array, dimension (2*M) 
   * 
   *    DWORK   DOUBLE PRECISION array, dimension (LDDWOR,2*M+3) 
   *            The leading 2*M-by-2*M part of this array is used for 
   *            computing the triangular factor of the QR decomposition 
   *            of the quasi-Hessenberg matrix. The remaining 6*M elements 
   *            are used as workspace for the computation of the 
   *            reciprocal condition estimate. 
   * 
   *    LDDWOR  INT 
   *            The leading dimension of array DWORK. 
   *            LDDWOR >= MAX(1,2*M). 
   * 
   *    Error Indicator 
   * 
   *    INFO    INT 
   *            = 0:  successful exit; 
   *            = 1:  if the quasi-Hessenberg matrix is (numerically) 
   *                  singular. That is, its estimated reciprocal 
   *                  condition number is less than or equal to TOL. 
   * 
   *    NUMERICAL ASPECTS 
   * 
   *    None. 
   * 
   *    CONTRIBUTORS 
   * 
   *    D. Sima, University of Bucharest, May 2000. 
   * 
   *    REVISIONS 
   * 
   *    - 
   * 
   *    Note that RC, UL, M, LDA, and LDDWOR must be such that the value 
   *    of the INT variable OK in the following statement is true. 
   * 
   *     OK = ( ( UL.EQ.'U' ) .OR. ( UL.EQ.'u' ) .OR. 
   *            ( UL.EQ.'L' ) .OR. ( UL.EQ.'l' ) ) 
   *          .AND. 
   *          ( ( RC.EQ.'R' ) .OR. ( RC.EQ.'r' ) .OR. 
   *            ( RC.EQ.'C' ) .OR. ( RC.EQ.'c' ) ) 
   *          .AND. 
   *          ( M.GE.0 ) 
   *          .AND. 
   *          ( LDA.GE.MAX( 1, M ) ) 
   *          .AND. 
   *          ( LDDWOR.GE.MAX( 1, 2*M ) ) 
   * 
   *    These conditions are not checked by the routine. 
   * 
   *    KEYWORDS 
   * 
   *    Hessenberg form, orthogonal transformation, real Schur form, 
   *    Sylvester equation. 
   * 
   *    ****************************************************************** 
   * 
   *    .. Scalar Arguments .. 
   *    .. Array Arguments .. 
   *    .. Local Scalars .. 
   *    .. External Functions .. 
   *    .. External Subroutines .. 
   *    .. Intrinsic Functions .. 
   *    .. Executable Statements .. 
   * 
   */
  /* Parameter adjustments */
  a_dim1 = *lda;
  a_offset = a_dim1 + 1;
  a -= a_offset;
  --d__;
  --iwork;
  dwork_dim1 = *lddwor;
  dwork_offset = dwork_dim1 + 1;
  dwork -= dwork_offset;

  /* Function Body */
  *info = 0;
  /* 
   *    For speed, no tests on the input scalar arguments are made. 
   *    Quick return if possible. 
   * 
   */
  if (*m == 0)
    {
      return 0;
    }
  /* 
   */
  m2 = *m << 1;
  if (C2F (lsame) (ul, "U", 1L, 1L))
    {
      /* 
       */
      i__1 = *m;
      for (j = 1; j <= i__1; ++j)
	{
	  j2 = j << 1;
	  /*Computing MIN 
	   */
	  i__2 = *m, i__3 = j + 1;
	  ml = Min (i__2, i__3);
	  C2F (dlaset) ("Full", &m2, &c__2, &c_b6, &c_b6,
			&dwork[(j2 - 1) * dwork_dim1 + 1], lddwor, 4L);
	  C2F (dcopy) (&ml, &a[j * a_dim1 + 1], &c__1,
		       &dwork[(j2 - 1) * dwork_dim1 + 1], &c__2);
	  C2F (dscal) (&ml, lambd1, &dwork[(j2 - 1) * dwork_dim1 + 1], &c__2);
	  C2F (dcopy) (&ml, &a[j * a_dim1 + 1], &c__1,
		       &dwork[(j2 - 1) * dwork_dim1 + 2], &c__2);
	  C2F (dscal) (&ml, lambd3, &dwork[(j2 - 1) * dwork_dim1 + 2], &c__2);
	  C2F (dcopy) (&ml, &a[j * a_dim1 + 1], &c__1,
		       &dwork[j2 * dwork_dim1 + 1], &c__2);
	  C2F (dscal) (&ml, lambd2, &dwork[j2 * dwork_dim1 + 1], &c__2);
	  C2F (dcopy) (&ml, &a[j * a_dim1 + 1], &c__1,
		       &dwork[j2 * dwork_dim1 + 2], &c__2);
	  C2F (dscal) (&ml, lambd4, &dwork[j2 * dwork_dim1 + 2], &c__2);
	  /* 
	   */
	  dwork[j2 - 1 + (j2 - 1) * dwork_dim1] += 1.;
	  dwork[j2 + j2 * dwork_dim1] += 1.;
	  /* L20: */
	}
      /* 
       */
      if (C2F (lsame) (rc, "R", 1L, 1L))
	{
	  *(unsigned char *) trans = 'N';
	  /* 
	   *          A is an upper Hessenberg matrix, row transformations. 
	   * 
	   */
	  i__1 = m2 - 1;
	  for (j = 1; j <= i__1; ++j)
	    {
	      mj = m2 - j;
	      if (j % 2 == 1 && j < m2 - 2)
		{
		  if (dwork[j + 3 + j * dwork_dim1] != 0.)
		    {
		      C2F (dlartg) (&dwork[j + 2 + j * dwork_dim1],
				    &dwork[j + 3 + j * dwork_dim1], &c__, &s,
				    &r__);
		      dwork[j + 2 + j * dwork_dim1] = r__;
		      dwork[j + 3 + j * dwork_dim1] = 0.;
		      C2F (drot) (&mj, &dwork[j + 2 + (j + 1) * dwork_dim1],
				  lddwor,
				  &dwork[j + 3 + (j + 1) * dwork_dim1],
				  lddwor, &c__, &s);
		      C2F (drot) (&c__1, &d__[j + 2], &c__1, &d__[j + 3],
				  &c__1, &c__, &s);
		    }
		}
	      if (j < m2 - 1)
		{
		  if (dwork[j + 2 + j * dwork_dim1] != 0.)
		    {
		      C2F (dlartg) (&dwork[j + 1 + j * dwork_dim1],
				    &dwork[j + 2 + j * dwork_dim1], &c__, &s,
				    &r__);
		      dwork[j + 1 + j * dwork_dim1] = r__;
		      dwork[j + 2 + j * dwork_dim1] = 0.;
		      C2F (drot) (&mj, &dwork[j + 1 + (j + 1) * dwork_dim1],
				  lddwor,
				  &dwork[j + 2 + (j + 1) * dwork_dim1],
				  lddwor, &c__, &s);
		      C2F (drot) (&c__1, &d__[j + 1], &c__1, &d__[j + 2],
				  &c__1, &c__, &s);
		    }
		}
	      if (dwork[j + 1 + j * dwork_dim1] != 0.)
		{
		  C2F (dlartg) (&dwork[j + j * dwork_dim1],
				&dwork[j + 1 + j * dwork_dim1], &c__, &s,
				&r__);
		  dwork[j + j * dwork_dim1] = r__;
		  dwork[j + 1 + j * dwork_dim1] = 0.;
		  C2F (drot) (&mj, &dwork[j + (j + 1) * dwork_dim1], lddwor,
			      &dwork[j + 1 + (j + 1) * dwork_dim1], lddwor,
			      &c__, &s);
		  C2F (drot) (&c__1, &d__[j], &c__1, &d__[j + 1], &c__1, &c__,
			      &s);
		}
	      /* L40: */
	    }
	  /* 
	   */
	}
      else
	{
	  *(unsigned char *) trans = 'T';
	  /* 
	   *          A is an upper Hessenberg matrix, column transformations. 
	   * 
	   */
	  i__1 = m2 - 1;
	  for (j = 1; j <= i__1; ++j)
	    {
	      mj = m2 - j;
	      if (j % 2 == 1 && j < m2 - 2)
		{
		  if (dwork[mj + 1 + (mj - 2) * dwork_dim1] != 0.)
		    {
		      C2F (dlartg) (&dwork[mj + 1 + (mj - 1) * dwork_dim1],
				    &dwork[mj + 1 + (mj - 2) * dwork_dim1],
				    &c__, &s, &r__);
		      dwork[mj + 1 + (mj - 1) * dwork_dim1] = r__;
		      dwork[mj + 1 + (mj - 2) * dwork_dim1] = 0.;
		      C2F (drot) (&mj, &dwork[(mj - 1) * dwork_dim1 + 1],
				  &c__1, &dwork[(mj - 2) * dwork_dim1 + 1],
				  &c__1, &c__, &s);
		      C2F (drot) (&c__1, &d__[mj - 1], &c__1, &d__[mj - 2],
				  &c__1, &c__, &s);
		    }
		}
	      if (j < m2 - 1)
		{
		  if (dwork[mj + 1 + (mj - 1) * dwork_dim1] != 0.)
		    {
		      C2F (dlartg) (&dwork[mj + 1 + mj * dwork_dim1],
				    &dwork[mj + 1 + (mj - 1) * dwork_dim1],
				    &c__, &s, &r__);
		      dwork[mj + 1 + mj * dwork_dim1] = r__;
		      dwork[mj + 1 + (mj - 1) * dwork_dim1] = 0.;
		      C2F (drot) (&mj, &dwork[mj * dwork_dim1 + 1], &c__1,
				  &dwork[(mj - 1) * dwork_dim1 + 1], &c__1,
				  &c__, &s);
		      C2F (drot) (&c__1, &d__[mj], &c__1, &d__[mj - 1], &c__1,
				  &c__, &s);
		    }
		}
	      if (dwork[mj + 1 + mj * dwork_dim1] != 0.)
		{
		  C2F (dlartg) (&dwork[mj + 1 + (mj + 1) * dwork_dim1],
				&dwork[mj + 1 + mj * dwork_dim1], &c__, &s,
				&r__);
		  dwork[mj + 1 + (mj + 1) * dwork_dim1] = r__;
		  dwork[mj + 1 + mj * dwork_dim1] = 0.;
		  C2F (drot) (&mj, &dwork[(mj + 1) * dwork_dim1 + 1], &c__1,
			      &dwork[mj * dwork_dim1 + 1], &c__1, &c__, &s);
		  C2F (drot) (&c__1, &d__[mj + 1], &c__1, &d__[mj], &c__1,
			      &c__, &s);
		}
	      /* L60: */
	    }
	  /* 
	   */
	}
    }
  else
    {
      /* 
       */
      i__1 = *m;
      for (j = 1; j <= i__1; ++j)
	{
	  j2 = j << 1;
	  /*Computing MAX 
	   */
	  i__2 = j - 1;
	  j1 = Max (i__2, 1);
	  /*Computing MIN 
	   */
	  i__2 = *m - j + 2;
	  ml = Min (i__2, *m);
	  C2F (dlaset) ("Full", &m2, &c__2, &c_b6, &c_b6,
			&dwork[(j2 - 1) * dwork_dim1 + 1], lddwor, 4L);
	  C2F (dcopy) (&ml, &a[j1 + j * a_dim1], &c__1,
		       &dwork[(j1 << 1) - 1 + (j2 - 1) * dwork_dim1], &c__2);
	  C2F (dscal) (&ml, lambd1,
		       &dwork[(j1 << 1) - 1 + (j2 - 1) * dwork_dim1], &c__2);
	  C2F (dcopy) (&ml, &a[j1 + j * a_dim1], &c__1,
		       &dwork[(j1 << 1) + (j2 - 1) * dwork_dim1], &c__2);
	  C2F (dscal) (&ml, lambd3, &dwork[(j1 << 1) + (j2 - 1) * dwork_dim1],
		       &c__2);
	  C2F (dcopy) (&ml, &a[j1 + j * a_dim1], &c__1,
		       &dwork[(j1 << 1) - 1 + j2 * dwork_dim1], &c__2);
	  C2F (dscal) (&ml, lambd2, &dwork[(j1 << 1) - 1 + j2 * dwork_dim1],
		       &c__2);
	  C2F (dcopy) (&ml, &a[j1 + j * a_dim1], &c__1,
		       &dwork[(j1 << 1) + j2 * dwork_dim1], &c__2);
	  C2F (dscal) (&ml, lambd4, &dwork[(j1 << 1) + j2 * dwork_dim1],
		       &c__2);
	  /* 
	   */
	  dwork[j2 - 1 + (j2 - 1) * dwork_dim1] += 1.;
	  dwork[j2 + j2 * dwork_dim1] += 1.;
	  /* L80: */
	}
      /* 
       */
      if (C2F (lsame) (rc, "R", 1L, 1L))
	{
	  *(unsigned char *) trans = 'N';
	  /* 
	   *          A is a lower Hessenberg matrix, row transformations. 
	   * 
	   */
	  i__1 = m2 - 1;
	  for (j = 1; j <= i__1; ++j)
	    {
	      mj = m2 - j;
	      if (j % 2 == 1 && j < m2 - 2)
		{
		  if (dwork[mj - 2 + (mj + 1) * dwork_dim1] != 0.)
		    {
		      C2F (dlartg) (&dwork[mj - 1 + (mj + 1) * dwork_dim1],
				    &dwork[mj - 2 + (mj + 1) * dwork_dim1],
				    &c__, &s, &r__);
		      dwork[mj - 1 + (mj + 1) * dwork_dim1] = r__;
		      dwork[mj - 2 + (mj + 1) * dwork_dim1] = 0.;
		      C2F (drot) (&mj, &dwork[mj - 1 + dwork_dim1], lddwor,
				  &dwork[mj - 2 + dwork_dim1], lddwor, &c__,
				  &s);
		      C2F (drot) (&c__1, &d__[mj - 1], &c__1, &d__[mj - 2],
				  &c__1, &c__, &s);
		    }
		}
	      if (j < m2 - 1)
		{
		  if (dwork[mj - 1 + (mj + 1) * dwork_dim1] != 0.)
		    {
		      C2F (dlartg) (&dwork[mj + (mj + 1) * dwork_dim1],
				    &dwork[mj - 1 + (mj + 1) * dwork_dim1],
				    &c__, &s, &r__);
		      dwork[mj + (mj + 1) * dwork_dim1] = r__;
		      dwork[mj - 1 + (mj + 1) * dwork_dim1] = 0.;
		      C2F (drot) (&mj, &dwork[mj + dwork_dim1], lddwor,
				  &dwork[mj - 1 + dwork_dim1], lddwor, &c__,
				  &s);
		      C2F (drot) (&c__1, &d__[mj], &c__1, &d__[mj - 1], &c__1,
				  &c__, &s);
		    }
		}
	      if (dwork[mj + (mj + 1) * dwork_dim1] != 0.)
		{
		  C2F (dlartg) (&dwork[mj + 1 + (mj + 1) * dwork_dim1],
				&dwork[mj + (mj + 1) * dwork_dim1], &c__, &s,
				&r__);
		  dwork[mj + 1 + (mj + 1) * dwork_dim1] = r__;
		  dwork[mj + (mj + 1) * dwork_dim1] = 0.;
		  C2F (drot) (&mj, &dwork[mj + 1 + dwork_dim1], lddwor,
			      &dwork[mj + dwork_dim1], lddwor, &c__, &s);
		  C2F (drot) (&c__1, &d__[mj + 1], &c__1, &d__[mj], &c__1,
			      &c__, &s);
		}
	      /* L100: */
	    }
	  /* 
	   */
	}
      else
	{
	  *(unsigned char *) trans = 'T';
	  /* 
	   *          A is a lower Hessenberg matrix, column transformations. 
	   * 
	   */
	  i__1 = m2 - 1;
	  for (j = 1; j <= i__1; ++j)
	    {
	      mj = m2 - j;
	      if (j % 2 == 1 && j < m2 - 2)
		{
		  if (dwork[j + (j + 3) * dwork_dim1] != 0.)
		    {
		      C2F (dlartg) (&dwork[j + (j + 2) * dwork_dim1],
				    &dwork[j + (j + 3) * dwork_dim1], &c__,
				    &s, &r__);
		      dwork[j + (j + 2) * dwork_dim1] = r__;
		      dwork[j + (j + 3) * dwork_dim1] = 0.;
		      C2F (drot) (&mj, &dwork[j + 1 + (j + 2) * dwork_dim1],
				  &c__1, &dwork[j + 1 + (j + 3) * dwork_dim1],
				  &c__1, &c__, &s);
		      C2F (drot) (&c__1, &d__[j + 2], &c__1, &d__[j + 3],
				  &c__1, &c__, &s);
		    }
		}
	      if (j < m2 - 1)
		{
		  if (dwork[j + (j + 2) * dwork_dim1] != 0.)
		    {
		      C2F (dlartg) (&dwork[j + (j + 1) * dwork_dim1],
				    &dwork[j + (j + 2) * dwork_dim1], &c__,
				    &s, &r__);
		      dwork[j + (j + 1) * dwork_dim1] = r__;
		      dwork[j + (j + 2) * dwork_dim1] = 0.;
		      C2F (drot) (&mj, &dwork[j + 1 + (j + 1) * dwork_dim1],
				  &c__1, &dwork[j + 1 + (j + 2) * dwork_dim1],
				  &c__1, &c__, &s);
		      C2F (drot) (&c__1, &d__[j + 1], &c__1, &d__[j + 2],
				  &c__1, &c__, &s);
		    }
		}
	      if (dwork[j + (j + 1) * dwork_dim1] != 0.)
		{
		  C2F (dlartg) (&dwork[j + j * dwork_dim1],
				&dwork[j + (j + 1) * dwork_dim1], &c__, &s,
				&r__);
		  dwork[j + j * dwork_dim1] = r__;
		  dwork[j + (j + 1) * dwork_dim1] = 0.;
		  C2F (drot) (&mj, &dwork[j + 1 + j * dwork_dim1], &c__1,
			      &dwork[j + 1 + (j + 1) * dwork_dim1], &c__1,
			      &c__, &s);
		  C2F (drot) (&c__1, &d__[j], &c__1, &d__[j + 1], &c__1, &c__,
			      &s);
		}
	      /* L120: */
	    }
	  /* 
	   */
	}
    }
  /* 
   */
  C2F (dtrcon) ("1-norm", ul, "Non-unit", &m2, &dwork[dwork_offset], lddwor,
		&rcond, &dwork[(m2 + 1) * dwork_dim1 + 1], &iwork[1], info,
		6L, 1L, 8L);
  if (rcond <= *tol)
    {
      *info = 1;
    }
  else
    {
      C2F (dtrsv) (ul, trans, "Non-unit", &m2, &dwork[dwork_offset], lddwor,
		   &d__[1], &c__1, 1L, 1L, 8L);
    }
  /* 
   */
  return 0;
  /**** Last line of SB04RX *** 
   */
}				/* nsp_slicot_sb04rx */
