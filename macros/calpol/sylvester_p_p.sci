function M=sylvester_p_p(p,q)
// Copyright  2010 Jean-Philippe Chancelier 
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
// Build a sylvester matrix 
// given two polynomials p and q 
  
  if size(p,'*')<>1 || size(q,'*') <> 1 then 
    error('polynomial matrices should be of size 1x1\n");
    return;
  end
  if p.degree[]== 0 && q.degree[]== 0 then 
    error('polynomial should not be both of degree zero\n");
  end
  cp = p.coeffs{1}; cp = cp($:-1:1); p0 = zeros(1,p.degree[]-1)
  cq = q.coeffs{1}; cq = cq($:-1:1); q0 = zeros(1,q.degree[]-1)
  
  M = toeplitz([cp(1),q0], [cp,q0]);
  M = [M; toeplitz([cq(1),p0], [cq,p0])];
endfunction


