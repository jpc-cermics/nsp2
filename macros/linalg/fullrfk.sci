function [Bk,Ck]=fullrfk(A,k)
// Copyright (C) 2007-2016 François Delebecque (GPL, scilab INRIA)
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
// This macro computes the full rank factorization of A^k i.e.
// Bk*Ck=A^k where Bk is full column rank and Ck full row rank.
// One has Range(Bk)=Range(A^k) and ker(Ck)=ker(A^k).
// For k=1 fullrfk is the same as fullrf.
//

  if nargin <= 1 then k=1;end 
  [n,m]=size(A);
  if m<>n then error("fullrfk: matrix should be square");return;end
  if k==0 then Bk=eye(n,n);Ck=Bk; return; end
  if k==1 then [Bk,Ck]=fullrf(A); return; end 
  [Bk,Ck]=fullrf(A);B=Bk;C=Ck;
  for l=2:k do
    [B,C,dim]=fullrf(C*B);
    Bk=Bk*B;Ck=C*Ck;     // Bk*Ck = A^k  (Full rank factorization)
  end
endfunction
