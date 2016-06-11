/* SB02MV.f -- translated by f2c (version 19961017).
 *   
 *   -lf2c -lm (in that order)
 */

#include "slicot.h"

int
nsp_slicot_sb02mv (double *reig, double *ieig)
{
  /* System generated locals */
  int ret_val;

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
   *    To select the stable eigenvalues for solving the continuous-time 
   *    algebraic Riccati equation. 
   * 
   *    ARGUMENTS 
   * 
   *    Input/Output Parameters 
   * 
   *    REIG    (input) DOUBLE PRECISION 
   *            The real part of the current eigenvalue considered. 
   * 
   *    IEIG    (input) DOUBLE PRECISION 
   *            The imaginary part of the current eigenvalue considered. 
   * 
   *    METHOD 
   * 
   *    The function value SB02MV is set to .TRUE. for a stable eigenvalue 
   *    and to .FALSE., otherwise. 
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
   *    V. Sima, Katholieke Univ. Leuven, Belgium, Aug. 1997. 
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
   *    .. Parameters .. 
   *    .. Scalar Arguments .. 
   *    .. Executable Statements .. 
   * 
   */
  ret_val = *reig < 0.;
  /* 
   */
  return ret_val;
  /**** Last line of SB02MV *** 
   */
}				/* nsp_slicot_sb02mv */
