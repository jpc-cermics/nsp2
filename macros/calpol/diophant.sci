function [x,err]=diophant(p1p2,b)
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

  //solves diophantine equation p1*x1+p2*x2=b
  //with  p1p2 a polynomial vector [p1 p2]
  //b polynomial
  //x polynomial vector [x1;x2]
  //if the equation is uncompatible err=||p1*x1+p2*x2-b||/||b||
  //else err=0
  // 
  p1=p1p2(1);p2=p1p2(2)
  [x,u]=bezout(p1,p2)
  p1=u(2,2);p2=u(1,2)
  if x.degree[] ==0 then
    x=b*u(:,1)
    err=0
  else
    [r,q]=pdiv(b,x)
    err=norm(coeff(b-x*q),2)/norm(coeff(b),2)
    x=q*u(:,1)
  end
endfunction
