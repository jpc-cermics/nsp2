function f=polfact(p)
  // Scilab ( http://www.scilab.org/ ) - This file is part of Scilab
  // Copyright (C) 1987-2017 - F. Delebecque et all (INRIA)
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

  // Minmal factors of p
  // p : polynomila
  // f : vector [f0 f1 ... fn] such that p=prod(f) 
  // - f0  constant
  // - fi polynomial
  // 
  if size(p,'*')<>1 then error('polynomial argument required'),end
  if ~isreal(p) then error('polynomial argument should be real'),end
  n=p.degree[];f=coeff(p,n);
  if n==0 then return,end
  var=p.get_var[];
  r=roots(p);[s,k]=sort(abs(r));r=r(k)
  k=1;
  while k<=n do
    if imag(r(k))<>0 then
      f=[f,real(poly(r(k:k+1),var))]
      k=k+2
    else
      f=[f,poly(r(k),var)]
      k=k+1
    end
  end
endfunction
