function [A,U,rk]=htrianr(A)
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

  //[A,U,rk]=htrianr(a)
  //triangularization of polynomial matrix A.  A is [m,n], m<=n.
  //U=right unimodular basis
  //the output value of A equals  A*U
  //rk=normal rank of A
  //Warning: there is an elimination of neglectable terms
  //!
  //
  A=clean(A);
  [m,n]=size(A);
  U=m2p(eye(n,n),var = A.get_var[],dim = ".");
  l1=n+1;
  for l=m: -1: max((m-n),1) do
    l1=l1-1;
    if l1<>0 then
      Al=A(l,1:l1);
      if norm(coeff(Al),1)>1.E-10 then
	[pg,Ul]=hrmt(Al);
	Ul=clean(Ul,1.E-10);
	A(l,1:l1)=[0*ones(1,l1-1),pg];
	U(:,1:l1)=U(:,1:l1)*Ul;
	if l>1 then
	  A(1:l-1,1:l1)=A(1:l-1,1:l1)*Ul;
	end
      else
	l1=l1+1
      end
    end
  end
  U=clean(U,1.E-10);
  k0=0;k1=0;tol=norm(coeff(A),1);
  v=[];w=[];
  for k=1:n do
    if max(abs(coeff(A(:,k))))<=sqrt(%eps)*tol then
      k0=k0+1;v=[v,k];
    else
      k1=k1+1,w=[w,k];
    end
  end
  ww=[v,w];
  A=A(:,ww);U=U(:,ww);
  rk=n-k0;
endfunction
