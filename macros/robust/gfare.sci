function [Z,H]=gfare(Sl)
//[Z,H]=gfare(Sl)
//Generalized Filter Algebraic Riccati Equation
//X = solution , F = gain
//!
//FD.
// Copyright INRIA
  [A,B,C,D]=abcd(Sl);
  S=eye(size(D'*D))+D'*D;
  R=eye(size(D*D'))+D*D';
  Si=inv(S);Ri=inv(R);
  Ar=A-B*Si*D'*C;
  H=[Ar',-C'*Ri*C;
     -B*Si*B',-Ar];
  Z=ric_desc(H);
  H=-(B*D'+Z*C')*Ri
endfunction
