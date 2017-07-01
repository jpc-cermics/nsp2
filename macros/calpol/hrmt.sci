function [pg,U]=hrmt(v)
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

  // Syntax: [pg,U]=hrmt(v)
  // Finds unimodular U and pg = gcd of a row of polynomials v
  // such that v*U = [0,pg]
  //!
  // 
  [n,m]=size(v)
  if n>1 then error("Error: argument should be a row vector");end
  pg=v(1)
  U= m2p(eye(m,m),var=v.get_var[],dim=".");
  for k=2:m do
    [pg,uk]=bezout(pg,v(k))
    //[pg,uk]=euclide(pg,v(k))
    U(:,k-1:k)=U(:,k-1:k)*uk(:,[2,1])
  end
endfunction

function [x,U]=gcd_p(p)
  // Given a polynomial vector p, [pgcd,u]=gcd(p) computes the gcd 
  // of components and a unimodular matrix (with polynomial inverse) u, 
  // with minimal degree such that [p1 p2]*u=[0 ... 0 pgcd]

  [m,n]=size(p)
  mn=m*n
  x=p(1);
  U= m2p(eye(m,m),var=p.get_var[],dim=".");
  for l=2:mn do
    [x,u]=bezout(x,p(l)),
    if nargout ==2 then
      U=[U(:,1:l-2), U(:,l-1)*u(1,[2,1])];U(l,l-1:l)=u(2,[2,1]);
    end
  end,
  if nargout ==1 then return,end
  for l=mn:-1:2 do
    pivot=U(l,l-1);
    for k=l:mn do
      [r,q]=pdiv(U(l,k),pivot)
      if coeff(q)<>0 then
	U(1:l-1,k)=U(1:l-1,k)-q*U(1:l-1,l-1)
	U(l,k)=r;
      end
    end
  end
endfunction
