function [S,P,D,index]=projspec(A,tol)
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
//[S,P,D,index]=projspec(A)
//Spectral characteristics of A at 0
//S = reduced resolvent at 0 (S=-Drazin_inverse(A))
//P = spectral projection at 0
//D = Nilpotent operator at 0
//index = index of the 0 eigenvalue
//
  [m,n]=size(A);
  if m<>n then error("Error: matrix should be square");end
  if nargin <= 1 then tol=1.E-6;end
  // A=0 ?
  if norm(A,1) < %eps*n*n then
    P=eye(size(A)),D=A,S=0*P;index=1;
  end
  //     nonsingular A:  index 0
  if rcond(A) > tol then
    S=inv(A),P=0*eye(size(A));D=P;index=0;return;
  end
  //write(%io(2),'    rank A^k    rcond')
  //                    index 1
  index=1;
  [B,C,dim]=fullrf(A);
  if dim==0 then
    P=eye(n,n);S=0*P;D=A;return
  end
  Ck=C;Bk=B;//write(%io(2),[dim,rcond(C*B)],'(7x,f3.0,6x,e9.3)');
  tst=rcond(Ck*Bk);
  if size(Ck,1)==1 then tst=norm(Ck*Bk,1);end
  if tst > tol then
    M=inv(C*B);P=eye(size(A))-B*M*C;S=B*M*M*C;D=0*A;return
  end
  //                  Higher index
  for k=2:n do
    [B,C,dim]=fullrf(C*B);
    if dim==0 then
      P=eye(n,n);S=0*P;D=A;index=k;return;
    end
    Bk=Bk*B;Ck=C*Ck;      // Bk*Ck = A^k  (Full rank factorization)
    index=k;
    //write(%io(2),[dim,rcond(C*B)],'(7x,f3.0,6x,e9.3)');
    if norm(C*B)*rcond(C*B) > tol then
      M=inv((C*B)^index);   //  M=inv(Ck*Bk);    (Alternative computation)
      P=eye(n,n)-Bk*M*Ck;   // P=eye(n,n)-Bk*inv(Ck*Bk)*Ck;  
      S=Bk*M*inv(C*B)*Ck;   // S=inv(A-P-D)+P   
      D=0.5*(A*P+P*A);return;
    end
  end
  P=eye(n,n);S=0*P;D=A;index=k;return;
endfunction

