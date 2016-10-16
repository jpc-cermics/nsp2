function [A,b]=aff2ab(lme,dimX,D,flag)
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
// Y,X,D are lists of matrices. 
// Y=lme(X,D)= affine fct of Xi's; 
// [A,b]=matrix representation of lme in canonical basis.
// if flag=='sp' A matrix is return in sparse storage.
  
  function l=vec2list(vect,sizes)
  // vect: a vector 
  // sizes a kx2 matrix giving sizes 
    msizes = prod(sizes,"c")
    n = sum(msizes);
    if isempty(vect) then vect=zeros(1,n);end
    if size(vect,"*") < n then 
      error(sprintf("Error: first argument of vec2list is too small %d, expecting %d",...
		    size(vect,"*"),n));
      return 
    end
    l=list();
    start=1;
    for k=1:size(sizes,"r") do
      m= vect(start:start + msizes(k)-1);
      start.add[ msizes(k)];
      l.add_last[matrix(m,sizes(k,1),sizes(k,2))];
    end
  endfunction

  function V=list2vec(L)
  // L: a list of matrices 
    V=[]; for i=1:length(L) do V.concatd[L(i)(:)];end
  endfunction 

  // main code 
  if nargin <= 3 then flag="f";end

  nvars=0;
  for k=dimX' do
    nvars=nvars+prod(k);
  end
  if part(flag,1)=="f" then
    x0=zeros(nvars,1);
    b=list2vec(lme(vec2list(x0,dimX),D));
    [p,un]=size(b);
    A=zeros(p,nvars);
    for k=1:nvars do
      xi=x0;xi(k)=1;
      A(:,k)=list2vec(lme(vec2list(xi,dimX),D))-b;
    end
  end

  if part(flag,1)=="s" then
    x0=zeros(nvars,1);
    b=list2vec(lme(vec2list(x0,dimX),D));
    A=[];
    for k=1:nvars do
      xi=x0;xi(k)=1;
      A=[A,sparse(list2vec(lme(vec2list(xi,dimX),D))-b)];
    end
  end
endfunction

function aff2ab_test()
// Lyapunov equation solver (one unknown variable, one constraint)

  function Y=lyapunov(X,D) [A,Q]=D(:);Xm=X(:); Y=list(A'*Xm+Xm*A-Q); endfunction;

  A=rand(3,3);Q=rand(3,3);Q=Q+Q';D=list(A,Q);dimX=[3,3];
  [Aly,bly]=aff2ab(lyapunov,dimX,D);
  [Xl,kerA]=linsolve(Aly,bly); Xv=vec2list(Xl,dimX); lyapunov(Xv,D)
  Xm=Xv(:); A'*Xm+Xm*A-Q

  // Lyapunov equation solver with redundant constraint X=X'
  // (one variable, two constraints) D is global variable

  function Y=ly2(X,D) [A,Q]=D(:);Xm=X(:); Y=list(A'*Xm+Xm*A-Q,Xm'-Xm); endfunction;
  A=rand(3,3);Q=rand(3,3);Q=Q+Q';D=list(A,Q);dimX=[3,3];
  [Aly,bly]=aff2ab(ly2,dimX,D);
  [Xl,kerA]=linsolve(Aly,bly); Xv=vec2list(Xl,dimX); ly2(Xv,D)

  // Francis equations
  // Find matrices X1 and X2 such that:
  // A1*X1 - X1*A2 + B*X2 -A3 = 0
  // D1*X1 -D2 = 0 

  function Y=bruce(X,D) 
    [A1,A2,A3,B,D1,D2]=D(:);
    [X1,X2]=X(:);
    Y=list(A1*X1-X1*A2+B*X2-A3,D1*X1-D2);
  endfunction

  A1=[-4,10;-1,2];A3=[1;2];B=[0;1];A2=1;D1=[0,1];D2=1;
  D=list(A1,A2,A3,B,D1,D2);
  [n1,m1]=size(A1);[n2,m2]=size(A2);[n3,m3]=size(B);
  dimX=[[m1,n2];[m3,m2]];
  [Af,bf]=aff2ab(bruce,dimX,D);
  [Xf,KerAf]=linsolve(Af,bf);Xsol=vec2list(Xf,dimX)
  bruce(Xsol,D)

  // Find all X which commute with A
  function y=f(X,D) y=list(D(:)*X(:)-X(:)*D(:)); endfunction

  A=rand(3,3);dimX=[3,3];[Af,bf]=aff2ab(f,dimX,list(A));
  [Xf,KerAf]=linsolve(Af,bf);[p,q]=size(KerAf);
  Xsol=vec2list(Xf+KerAf*rand(q,1),dimX);
  C=Xsol(:); A*C-C*A
  
endfunction
