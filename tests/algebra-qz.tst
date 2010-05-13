// -*- Mode: scilab -*- 

exec('algebra-funs.sce');

function t=sel(R),t=real(R)<0 ,endfunction;

function [ok,pb]=check(A,E,As,Es,Q,Z)
  prec=400;
  if isreal(A,%t) then triu_arg=-1; else triu_arg=0;end 
  ok = %t;
  if norm(triu(As,triu_arg)- As)> %eps then ok = %f,pb=1,return;end;
  if norm(triu(Es,triu_arg)- Es)> %eps then ok = %f,pb=2,return;end;
  if norm(Q*Q'-eye(size(Q)),1) > 400*%eps then ok=%f,pb=3,return;end
  if norm(Z*Z'-eye(size(Z)),1) > 400*%eps then ok=%f,pb=4,return;end
  if norm(As-Q'*A*Z,1) > 400*%eps then ok=%f,pb=5,return;end
  if norm(Es-Q'*E*Z,1) > 400*%eps then ok=%f,pb=6,return;end  
endfunction;

A=randn(0,4);
[As,Es,Q,Z,dim]=qz(A,A);
if ~isempty(As) then pause;end 
if ~isempty(Es) then pause;end 
if ~isempty(Q) then pause;end 
if ~isempty(Z) then pause;end
if dim<>0  then pause;end 

// detect wrong entries 

if execstr('[As,Es]=qz(randn(2,3),randn(2,3))',errcatch=%t)==%t then  pause,end
if execstr('[As,Es,Q,Z]=qz(randn(2,3),randn(2,3))',errcatch=%t)==%t then  pause,end
if execstr('[As,Es,dim]=qz(randn(2,3),randn(2,3),sort=''c'')',errcatch=%t)==%t then  pause,end
if execstr('[Z,dim]=qz(randn(2,3),randn(2,3),sort=sel)',errcatch=%t)==%t then  pause,end

function t=sel(Alpha,Beta),t=real(Alpha) > -0.2*real(Beta) ,endfunction;

for N=[5,50] 
  A=testmat1(1,N);E=testmat1(-2,N) ;
  [As,Es,Q,Z,dim]=qz(A,E);
  if ~check(A,E,As,Es,Q,Z) then pause;end 
  [As,Es,Q,Z]=qz(A,E);
  if ~check(A,E,As,Es,Q,Z) then pause;end 
  [As,Es]=qz(A,E);
  if ~check(A,E,As,Es,Q,Z) then pause;end 
  // Ordered sort='c'
  [As,Es,Q,Z,dim]=qz(A,E,sort='c');
  if ~check(A,E,As,Es,Q,Z) then pause;end 
  if dim<>N then pause;end 
  [As,Es,Q,Z,dim]=qz(A,E,sort='d');
  if ~check(A,E,As,Es,Q,Z) then pause;end 
  if dim<>N then pause;end 
  //ordered sort=sel
  [As,Es,Q,Z,dim]=qz(A,E,sort=sel);
  dim1=size(find( real(spec(As)) > -0.2*spec(Es)))
  if dim<>dim1 then pause;end 
  if ~check(A,E,As,Es,Q,Z) then pause;end 
end

function t=sel(Alpha,Beta),t=imag(Alpha)>0 ,endfunction;

for N=[5,50] do 
  A=testmat1(1+%i,N);E=testmat1(-2-3*%i,N) ;
  [As,Es,Q,Z,dim]=qz(A,E);
  if ~check(A,E,As,Es,Q,Z) then pause;end 
  [As,Es,Q,Z]=qz(A,E);
  if ~check(A,E,As,Es,Q,Z) then pause;end 
  //XX when Q,Z are not requested the result 
  // is correct but the As,Es are not the same
  // so the check will fail.
  //[As,Es]=qz(A,E);
  //if ~check(A,E,As,Es,Q,Z) then pause;end 
  // Ordered sort='c'
  [As,Es,Q,Z,dim]=qz(A,E,sort='c');
  if ~check(A,E,As,Es,Q,Z) then pause;end 
  if dim<>N then pause;end 
  [As,Es,Q,Z,dim]=qz(A,E,sort='d');
  if ~check(A,E,As,Es,Q,Z) then pause;end 
  if dim<>N then pause;end 
  //ordered sort=sel
  [As,Es,Q,Z,dim]=qz(A,E,sort=sel);
  dim1=size(find( imag(spec(As)) > 0))
  if dim<>dim1 then pause;end 
  if ~check(A,E,As,Es,Q,Z) then pause;end 
end



