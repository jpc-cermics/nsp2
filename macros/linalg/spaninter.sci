function [x,dim]=spaninter(a,b,tol)
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
//[X,dim]=spaninter(a,b [,tol])  computes intersection of Range(A)
// and Range(B)
//
// x(:,1:dim) is an orthogonal basis for A inter B. 
//            In the X basis A and B are respectively:
//            X'*A and X'*B.
// dim        dimension of subspace A inter B.
// tol        threshold (sqrt(%eps) is the default value).
  [na,ma]=size(a);[nb,mb]=size(b);
  if ma*na==0 then dim=0;x=eye(nb,nb);return;end
  if mb*nb==0 then dim=0;x=eye(na,na);return;end
  if nargin <= 2 then tol=sqrt(%eps);end
  if na <> nb then error("Uncompatible dimensions"),end
  if mb > ma then [x,dim]=spaninter(b,a,tol),return,end
  [xp,ra]=rowcomp(a);x=xp'
  //test  trivial cases :
  //a surjective a inter b is b
  if ra == na then [xp,dim]=rowcomp(b),x=xp',return,end
  //a is zero
  if ra == 0 then dim=0,return,end
  b=xp*b;      //update
  // b2=vectors in b which are in a
  up=1:ra;low=ra+1:na;
  [v1,k2]=colcomp(b(low,:));
  b1=b*v1;     //update
  bup=b1(up,:);blow=b1(low,:)
  if norm(blow,1) <= tol*norm(bup,1) then k2=0,end
  k1=mb-k2;
  if k1==0 then dim=0;return;end
  b2=b1(:,1:k1);  //intersection in base x
  if norm(b2,1) < tol*norm(b,1)*mb*nb then dim=0,return,end
  [u2p,dim]=rowcomp(b2(up,:));
  x(:,up)=x(:,up)*u2p';         //update
endfunction



