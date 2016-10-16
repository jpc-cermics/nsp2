function [x0,kerA]=linsolve(A,b,x0)
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
  %tol=1.E-10;
  // Finds all x solution to Ax+b=0; 
  // x0=particular solution; kerA=nullspace of A
  // Any x=x0+kerA*w with arbitrary w solves A*x+b=0
  Ab=[A,b];[ma,na]=size(Ab);
  [W,rk]=colcomp(Ab);
  W=W(:,1:na-rk);last=W(na,:);
  [W2,rk1]=colcomp(last);
  if rk1==0 then 
    printf("Conflicting linear constraints!\n");x0=[];kerA=[];return;
  end
  W=W*W2;
  kerA=W(1:na-1,1:na-rk-1);
  if nargin==3 then
    if norm(A*x0+b,1)<%tol then 
      return;
    end
    printf("linsolve: recomputing initial guess\n");
  end
  piv=W(na,na-rk);x0=W(1:na-1,na-rk)/piv;
endfunction 

function [x0,kerA]=linsolve_sp(A,b,x0)
  %tol=1.E-10;
  // Finds all x solution to Ax+b=0; 
  // x0=particular solution; kerA=nullspace of A
  // Any x=x0+kerA*w with arbitrary w solves A*x+b=0
  Ab=[A,b];[ma,na]=size(Ab);
  [W,rk]=colcomp(Ab);
  W=W(:,1:na-rk);last=W(na,:);
  [W2,rk1]=colcomp(last);
  if rk1==0 then 
    printf("Conflicting linear constraints!\n");x0=[];kerA=[];return;
  end
  W=W*W2;
  kerA=W(1:na-1,1:na-rk-1);
  if nargin==3 then
    if norm(A*x0+b,1)<%tol then 
      return;
    end
    printf("linsolve: recomputing initial guess\n");
  end
  piv=W(na,na-rk);x0=W(1:na-1,na-rk)./piv;
endfunction 

if %f then
  [ma,na]=size(A);
  %tol=full(1.E-10*max(abs(A))*max(ma,na));
  if ma<na then 
    A=[A;sparse([],[],[na-ma,na])];b=[b;zeros(na-ma,1)];
  end
  if ma>na then
    //A=[A,sparse([],[],[ma,ma-na])];x0=[x0;zeros(ma-na,1)];
    b=A'*b;A=A'*A;ma=na;
  end
  As= umfpack_create(A);
  [L,U,p,q,r]=As.luget[];
  rkA = length(find(full(diag(U)) >= %tol))
  if nargin ==3 then
    if norm(A*x0+b,1)>%tol then 
      x0= As.solve[-b];
      printf("linsolce: recomputing initial guess\n");
    end
  end
  if nargin==2 then  
    x0=As.solve[-b];
  end
  nma=max(na,ma);
  printf("Error: computation of kernel is to be done for umfpack_create \n");
  Y=[sparse([],[],[rkA,nma-rkA]);speye(nma-rkA,nma-rkA)];
  kerA=[];
  for k=1:na-rkA do
    bb=full(Y(:,k));
    res= As.solve[bb,mode="U"]; // solve Ux=bb;
    ww=sparse(res);
    kerA=[kerA,ww];
  end     
  if na<>rkA then 
    kerA=clean(Q'*kerA);
  end
  if norm(A*x0+b,1)>%tol then
    printf("Possible Conflicting linear constraints, error in the order of "+...
	   string(norm(A*x0+b,1)));
  end
  if ma>na then kerA=kerA(1:na,:);x0=x0(1:na,1);end     
end

