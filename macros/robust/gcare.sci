function [X,F]=gcare(Sl)
//[X,F]=gcare(Sl)
//Generalized Control Algebraic Riccati Equation
//X = solution , F = gain
//!
//FD.
// Copyright INRIA
  [A,B,C,D]=abcd(Sl);
  S=eye(size(D'*D))+D'*D;
  R=eye(size(D*D'))+D*D';
  Si=inv(S);
  Ar=A-B*Si*D'*C;
  H=[Ar,-B*Si*B';
     -C'*inv(R)*C,-Ar'];
  X=ric_desc(H);
  F=-Si*(D'*C+B'*X)
endfunction
