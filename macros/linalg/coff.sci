function [n,d]=coff(m,var)
// Scilab ( http://www.scilab.org/ ) - This file is part of Scilab
// Copyright (C) 1987-2016 - F. Delebecque (Inria)
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
// [n,d]=coff(M [,var]) computes n/d= (s*eye-M)^-1 
// n: numerator polynomial matrix 
// d: common denominator
// var character string ('s' if omitted)
// See also : coffg
  
  if %f then 
    m=rand(2,2);
    [n,d]=coff(m,"s");
    clean((poly(0,"s")*eye(2,2)-m )*n -d*eye(2,2))
  end
  
  if type(m,"short")<>"m" then 
    error("Error: first argument should be a real or complex matrix"),
  end
  if isempty(m) then n=[];d=1;return;end
  if nargin <= 1 then var="s",end
  d=clean(poly(m,var)); // denominator
  [m1,n1]=size(m);
  if m1<>n1 then error("Error: matrix should be square");end
  n=m2p([],var=var);
  for k=1:n1 do 
    for l=1:n1 do 
      mlk=-m(l,k);
      if abs(mlk)<1 then mlk=1,end
      m(l,k)=m(l,k)+mlk;
      n(k,l)=-(poly(m,var)-d)/mlk;
      m(l,k)=m(l,k)-mlk
    end
  end
  if norm(imag(m),1)==0 then n=real(n);d=real(d);end
  n=clean(n);
endfunction
