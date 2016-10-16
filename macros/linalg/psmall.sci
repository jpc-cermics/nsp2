function [Q,M]=psmall(A,thres,flag)
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
// [Q,M]=psmall(A,thres,flag) 
//Projection on eigensubspace associated with eigenvalues
//with real part < thres (case flag='c') or with modulus <thres (flag='d')
//Projection is defined by Q*M. Eigenvalues of M*A*Q = eigenvalues
//of A with real part < thres (case flag='c',...).
//If [Q1,M1]== full rank factorization (fullrf) of eye-Q*M then evals of 
// M1*A*Q1 =evals of A with real part >= thres (case flag='d',...).
// See also pbig

  [m,n]=size(A);
  if m<>n then error("Error: matrix should be square");end
  thres=real(thres);
  if flag=="c" then 
    function flag=%smallei(x,values) flag=real(x) < values(1); endfunction 
    function flag=%bigeig(x,values) flag=real(x) >= values(1); endfunction 
    

  elseif flag=="d" then 
    function flag=%smallei(x,values) flag=abs(x) < values(1); endfunction 
    function flag=%bigeig(x,values) flag=abs(x) >= values(1); endfunction 
  else
    error("Invalid flag value, it must be ""c"" or ""d"" ")
  end
  // 
  [X,dsmall] = schur(A,sort=%smallei,args=list(thres));
  [Y,dbig] = schur(A,sort=%bigeig,args=list(thres));
  Q=X(:,1:dsmall);
  if isempty(Q) then M=[];return;end
  Y1=Y';
  M1=Y1(dbig+1:n,:);
  E=M1*Q;
  if rcond(E)>1.E-6 then
    M=E\M1;
  else
    //warning("bad conditionning--> balancing")
    [Ab,X0]=balanc(A);
    [X,dsmall] = schur(Ab,sort=%smallei,args=list(thres));
    X1=X*X0;Q=X1(:,1:dsmall);
    [Y,dbig] = schur(Ab,sort=%bigeig,args=list(thres));
    Y1=inv(X0)*Y';M=Y1(dbig+1:n,:);
    E=M*Q;
    M=E\M;
  end
endfunction
