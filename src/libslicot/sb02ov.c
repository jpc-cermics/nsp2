/* SB02OV.f -- translated by f2c (version 19961017).
 *   
 *   -lf2c -lm (in that order)
 */

#include "slicot.h"

int
nsp_slicot_sb02ov (double *alphar, double *alphai, double *beta)
{
  /* System generated locals */
  int ret_val;

  /* Local variables */

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
   *    To select the unstable generalized eigenvalues for solving the 
   *    discrete-time algebraic Riccati equation. 
   * 
   *    ARGUMENTS 
   * 
   *    Input/Output Parameters 
   * 
   *    ALPHAR  (input) DOUBLE PRECISION 
   *            The real part of the numerator of the current eigenvalue 
   *            considered. 
   * 
   *    ALPHAI  (input) DOUBLE PRECISION 
   *            The imaginary part of the numerator of the current 
   *            eigenvalue considered. 
   * 
   *    BETA    (input) DOUBLE PRECISION 
   *            The (real) denominator of the current eigenvalue 
   *            considered. 
   * 
   *    METHOD 
   * 
   *    The function value SB02OV is set to .TRUE. for an unstable 
   *    eigenvalue (i.e., with modulus greater than or equal to one) and 
   *    to .FALSE., otherwise. 
   * 
   *    REFERENCES 
   * 
   *    None. 
   * 
   *    NUMERICAL ASPECTS 
   * 
   *    None. 
   * 
   *    CONTRIBUTOR 
   * 
   *    Release 3.0: V. Sima, Katholieke Univ. Leuven, Belgium, Sep. 1997. 
   *    Supersedes Release 2.0 routine SB02CX by P. Van Dooren, Philips 
   *    Research Laboratory, Brussels, Belgium. 
   * 
   *    REVISIONS 
   * 
   *    - 
   * 
   *    KEYWORDS 
   * 
   *    Algebraic Riccati equation, closed loop system, continuous-time 
   *    system, optimal regulator, Schur form. 
   * 
   *    ****************************************************************** 
   * 
   *    .. Scalar Arguments .. 
   *    .. External Functions .. 
   *    .. Intrinsic Functions .. 
   *    .. Executable Statements .. 
   * 
   */
  ret_val = C2F (dlapy2) (alphar, alphai) >= Abs (*beta);
  /* 
   */
  return ret_val;
  /**** Last line of SB02OV *** 
   */
}				/* nsp_slicot_sb02ov */
