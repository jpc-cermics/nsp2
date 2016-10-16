function [W,rk]=rowcomp(A, meth="svd", tol=[])
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
// Row compression of matrix A <--> computation of im(A)
// tol tolerance parameter (sqrt(%eps)*norm(A,1) as default value)
// the rk first (top) rows of W span the row range of A
// the rk first columns of W' span the image of A
// W'*A = [Abar;0] where Abar has rk rows and is full row rank.
//
  if isempty(A) then W=[];rk=0;return;end
  [ma,na]=size(A)
  if norm(A,1) < sqrt(%eps)/10 then rk=0,W=eye(ma,ma),return;end
  if isempty(tol) then tol=sqrt(%eps)*norm(A,1);end 
  select meth
   case "qr" then 
    [q,r,p,rk]=qr(A,tol=tol);W=q';
   case "svd" then 
    [u,s,v,rk]=svd(A,tol=tol);W=u' ;
  else
    error("rowcomp: meth optional argument should be ''qr'' or ''svd''");
  end
endfunction


function [W,rk]=rowcomp_sp(A, tol=[])
// Copyright (C) 2016-2016 François Delebecque 
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
// (jpc 2016: qr also works for nsp is %spqr is present)
// see above 

  if ~%spqr then 
    error("spqr not implemented in your nsp version");
    return;
  end

  if isempty(A) then W=[];rk=0;return;end
  [ma,na]=size(A)
  if norm(A,1) < sqrt(%eps)/10 then rk=0,W=eye(ma,ma),return;end
  if isempty(tol) then tol=sqrt(%eps)*norm(A,1);end 
  [q,r,p,rk]=qr(A,tol=tol);W=q';
endfunction


