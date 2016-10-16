function [W,rk]=colcomp(A, meth="svd", tol=[])
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
// [W,rk]=colcomp(A,  meth='svd'| 'qr' , tol=tol)
// computes a column compression of matrix A 
// i.e. computation of ker(A)
// meth='qr' or 'svd' (defaut 'svd')
// tol tolerance parameter (of order %eps as defaut value)
// the ma-rk first columns of W span the kernel of A when size(A)=(na,ma)
// A*W=[0 Abar] where Abar is full column rank, and has rk columns.
// 
  [ma,na]=size(A)
  if isempty(A) then W=[];rk=0;return;end
  if norm(A,1) < sqrt(%eps)/10 then rk=0,W=eye(na,na),return,end
  if isempty(tol) then tol=sqrt(%eps)*norm(A,1)*max(ma,na),end
  select meth
   case "qr" then 
    [q,r,e,rk]=qr(A',tol=tol);  W=q(:,na:-1:1)
   case "svd" then 
    [u,s,v,rk]=svd(A',tol=tol);  W=u(:,na:-1:1)
  else
    error("rowcomp: meth optional argument should be ''qr'' or ''svd''");
  end
endfunction

function [W,rk]=colcomp_sp(A, tol=[])
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

  if ~%spqr then 
    error("spqr not implemented in your nsp version");
    return;
  end

  [ma,na]=size(A)
  if isempty(A) then W=[];rk=0;return;end
  if norm(A,1) < sqrt(%eps)/10 then rk=0,W=eye(na,na),return,end
  if isempty(tol) then tol=sqrt(%eps)*norm(A,1)*max(ma,na),end
  [q,r,e,rk]=qr(A',tol=tol);  W=q(:,na:-1:1);
endfunction
