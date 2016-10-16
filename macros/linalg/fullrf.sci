function [Q,M,rk]=fullrf(A,tol)
// Copyright (C) 2007-2016 François Delebecque (GPL, scilab INRIA)
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
// [Q,M,rk]=fullrf(A)
// Full rank factorization : A=Q.M
// with range(Q)=range(A) and ker(M)=ker(A),
// Q full column rank , M full row rank
// rk = rank(A) = #columns(Q) = #rows(M)
  
  na1=norm(A,1);
  if na1 < 1.E-10 then Q=[];M=[];rk=0;return;end
  // optional arguments for svd
  args=hash(1);
  if nargin <= 1 then args=hash(tol=sqrt(%eps)*na1);end
  [U,s,V,rk]=svd(A,args(:));
  sq=sqrt(diag(s));// s is a vector in nsp
  Q=U*sq;M=sq*V';
  Q=Q(:,1:rk);M=M(1:rk,:);
endfunction


