function [Bfs,Bis,tf]=des2tf(des)
  // des admits a D matrix.
  // Copyright INRIA
  if ~(nargout <= 1 || nargout==3) then
    error('Error: 1 or 3 output args needed');
  end
  // [A,B,C,D,E]=des(2:6);
  [Bfs,Bis,chis]=glever(des.E,des.A);
  if nargout==3 then
    Bfs=des.C*Bfs*des.B;
    Bis=des.C*Bis*des.B+des.D;
    tf=chis;
    return;
  end
  if nargout <= 1 then
    ww=des.C*Bfs*des.B;Bfs=ww/chis-des.C*Bis*des.B+des.D;
    return;
  end
endfunction
