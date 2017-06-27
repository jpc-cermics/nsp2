function [x,dim,dima]=spanplus(a,b,tol)
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
//[X,dim,dima]=spanplus(A,B,tol) computes an orthogonal basis of
// a+b such that : the first dima columns of x span Range(A)
// and the following (dim-dima) columns make a basis of a+b
// relative to a. tol is an optional argument.
// The dim first columns of x make a basis for A+B.
//
  [na,ma]=size(a);[nb,mb]=size(b);
  if na*ma==0 then 
    dima=0;[x,dim]=rowcomp(b);x=x';return;
  end
  if nb*mb==0 then [x,dima]=rowcomp(a);dim=dima;x=x';return;end
  if nargin == 2 then tol=%eps*na*nb*ma*mb;end
  if na<>nb then error("uncompatible dimensions!"),end
  [x,dima]=rowcomp(a);
  b=x*b;x=x'; //update b,x
  if dima == na then dim=dima,return,end
  low=(dima+1):na;
  blow=b(low,:);
  if norm(blow,1) <= tol*norm(b,1) then dim=dima,return,end
  [u2,r2]=rowcomp(blow);
  dim=dima+r2;
  x(:,low)=x(:,low)*u2';    //update
endfunction



