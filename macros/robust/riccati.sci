function [x1,x2]=riccati(a,b,c,dom,typ)
//[x1,[x2]]=riccati(a,b,c,dom,[typ]) is a Riccati solver
// x=x1/x2 solves:
// a'*x+x*a-x*b*x+c=0 (continuous time case)
// a'*x*a-(a'*x*b1/(b2+b1'*x*b1))*(b1'*x*a)+c-x with b=b1/b2*b1'
// (discrete time case)
// If called with LHS=1 (one ouput argument) riccati returns x.
//
// -- a,b,c real matrices nxn, b and  c symetric.
// -- dom = 'c' or 'd' for the time domain (continuous or discrete)
// -- typ = 'eigen' --->block diagonalization
//    typ = 'schur' --->schur method
// See also ric_desc
//!
// Copyright INRIA
  if nargin == 4 then typ='eigen',end,
  ham=[a -b;-c -a'],
  [m,n]=size(a),
  if part(dom,1)=='c' then
    select  typ,
     case 'schur' then
      [s,d]=schur(ham,sort='c'),
      if d<>n then 
	error('Error: wrong dimension ('+string(d)+') of stable subspace -expecting '+string(n)')
      end
      s=s(:,1:n),
     case 'eigen' then
      [hb,u1]=bdiag(ham),
      [u2,d]=schur(hb,sort='c'),
      u=u1*u2,
      if d<>n then 
	error('Error: wrong dimension ('+string(d)+') of stable subspace -expecting '+string(n)')
      end
      s=u(:,1:n),
    end,
  else
    aa=[eye(n,n) b;0*ones(n,n) a'],bb=[a  0*ones(n,n);-c eye(n,n)],
    // schur(A,B,...) is named qz is nsp and returned arguments are not 
    // the same [bs,as,Q,Z,dim] in nsp [bs,as,Z,dim] in scicoslab
    [bs,as,Q,Z,dim]=qz(bb,aa, sort = 'd');
    if dim<>n then 
      error('Error: wrong dimension ('+string(dim)+') of stable subspace -expecting '+string(n)')
    end
    s=Z(:,1:dim);
  end,
  x1=s(n+1:2*n,:),x2=s(1:n,:),
  if nargout == 1 then x1=x1/x2,end
endfunction
