function [U,dim]=range(A,k)
// Computes the Range of A^k ; 
// the first dim columns of U' span the range of A^k.
//
// Copyright (C) 2007-2016 François Delebecque (GPL, scilab INRIA)
//
  if nargin <= 1 then k=1;end 
  [m,n]=size(A);
  if m<>n then error("range: first argument should be a square matrix");return;end
  if k==0 then dim=m;U=eye(m,m);return; end
  [U,dim]=rowcomp(A);
  if k==1 then return; end
  for l=2:k do B=A*U'; [U,dim]=rowcomp(B(:,1:dim)); end;
endfunction
