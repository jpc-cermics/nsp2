function [a,u]=hermit(a)
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

  //[A,U]=hermit(A)
  //Hermite form: U is an unimodular matrix such that A*U is
  //triangular. The output value of A is A*U.
  //Warning: Experimental version
  //!
  //
  [m,n]=size(a);if m<>n then error('square matrix only!'),end
  [a,u]=htrianr(a)
  for l=n-1: -1: 1 do
    dl(l:n)=a(l,l:n).degree[];
    for k=l+1:n do
      if dl(k)>=dl(l) then
	all=a(l,l);
	if norm(coeff(all),1)>1.E-10 then
	  [r,q]=pdiv(a(l,k),a(l,l))
	  if l>1 then a(1:l-1,k)=a(1:l-1,k)-a(1:l-1,l)*q;end
	  a(l,k)=r
	  u(:,k)=u(:,k)-u(:,l)*q
	end
      end
    end
  end
endfunction
