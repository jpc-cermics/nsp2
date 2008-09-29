function x = bdiv_sp_m(A,b)
//
// Copyright (C) 2008 Bruno Pinçon, Jean-Philippe Chancelier
//
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
//
//  Purpose
//  -------
//    a first version for A\b for sparse A and full b
//    possible improvments:
//     - use the banded solver when bandwidths are small and/or near full
//       (but computation of the condition number tends to annihilate
//        the gain...)
//     - use a sparse qr or the lsqr iterative method in the case n!=m
//       when the conditionning of A'A (m>n) or AA' (m<n) is bad.
//
   [m,n] = size(A)
   if m == n then
      [kl,ku] = lower_upper_bandwidths(A)
      if ku == 0 then
	 x = solve_tri(A,b,"l")
      elseif kl == 0 then
	 x = solve_tri(A,b,"u")
      else
	 if ~%umfpack then
	    error("feature not enable (needs umfpack)")
	 end
	 LU = umfpack_create(A);
	 rc1 = LU.rcond[];
	 if ( rc1 < m*%eps )
	    printf(" Warning: matrix is badly conditionned, solution is dubtious (rcond = %e)\n", rc1)
	 end
	 x = LU.solve[b];
      end
   else
      if ~%cholmod then
	 error("feature not enable (needs cholmod)")
      end
      if m > n then
	 // printf(" Warning: solving overdetermined sparse linear system using normal equations...\n")
	 [C,p] = cholmod_create(A'*A)
	 // we could use also [C,p] = cholmod_create(A,type="col")
         // but this seems slower on different examples tested
	 if ( p ~= 0 ) then, error("matrix is not full rank"), end 
	 rc1 = C.get_rcond[];
	 if ( rc1 < n*%eps )
	    printf(" Warning: matrix is badly conditionned, solution is dubtious (rcond = %e)\n", rc1)
	 end
	 x = C.solve[pmult(A,b)];
      else
	 // printf(" Warning: solving underdetermined sparse linear system using x = A''*(A*A'')^-1 b ...\n")
	 [C,p] = cholmod_create(A*A')
	 // we could use also [C,p] = cholmod_create(A,type="row")
         // but this seems slower on different examples tested
	 if ( p ~= 0 ) then, error("matrix is not full rank"), end 
	 rc1 = C.get_rcond[];
	 if ( rc1 < m*%eps )
	    printf(" Warning: matrix is badly conditionned, solution is dubtious (rcond = %e)\n", rc1)
	 end
	 x = pmult(A,C.solve[b]);
      end
   end
endfunction
