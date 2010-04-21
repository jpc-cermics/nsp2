function [W,rk]=colcomp(A,flag,thres)
//Syntaxes : [W,rk]=colcomp(A)
//           [W,rk]=colcomp(A,flag)
//           [W,rk]=colcomp(A,flag,thres)
//column compression of A i.e. computation of ker(A)
//flag and thres are optional parameters
//flag='qr' or 'svd' (defaut 'svd')
//thres tolerance parameter (of order %eps as defaut value)
//the ma-rk first columns of W span the kernel of A when size(A)=(na,ma)
//A*W=[0 Abar] where Abar is full column rank, and has rk columns.
//F.D.
  [ma,na]=size(A)
  if A==[] then W=[];rk=0;return;end
  if norm(A,1) < sqrt(%eps)/10 then rk=0,W=eye(na,na),return,end
  if nargin ==2 then thres=sqrt(%eps)*norm(A,1)*max(ma,na),end
  if nargin==1 then flag='svd',thres=sqrt(%eps)*norm(A,1)*max(ma,na);end
  select flag
   case 'qr' then [q,r,rk,e]=qr(A',thres);
    W=q(:,na:-1:1)
   case 'svd' then [u,s,v,rk]=svd(A',tol=thres);
    W=u(:,na:-1:1)
  end
endfunction
