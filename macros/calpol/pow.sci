function r=pow_p(p,n)
// overload of ^ operation for polynomial 
// matrices. p^n is computed by repeated squaring 
// 
// Copyright  2010 Jean-Philippe Chancelier Cermics/Enpc 
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
    
  r=m2p([1]);
  q=p;
  while  n > 1 
    if modulo(n,2) == 1 then 
      r = r * q;
    end
    n = idiv(n,2);
    q = q*q;
  end
  r = q*r;
endfunction
