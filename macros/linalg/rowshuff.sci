function [Ws,Fs1]=rowshuff(Fs,alfa)
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
// Shuffle algorithm: Given the pencil Fs=s*E-A, returns Ws=W(s) 
// (square polynomial matrix) such that:
// Fs1 = s*E1-A1 = W(s)*(s*E-A) is a pencil with non singular E1 matrix.
// This is possible iff the pencil Fs = s*E-A is regular (i.e. invertible).
// The poles @ infinity of Fs are put to alfa and the zeros of Ws are @ alfa.
// Note that (s*E-A)^-1 = (s*E1-A1)^-1 * W(s) = (W(s)*(s*E-A))^-1 *W(s)

  if nargin <= 1 then alfa=0; end
  [E,A]=pen2ea(Fs);
  //     E is non singular: --> exit
  if rcond(E) >= 1.E-5 then Ws=eye(size(E));Fs1=Fs;return;end
  //     E is singular:
  s=poly(0,"s"); %zero=poly(0,"s",roots=%f)
  tol=1.E-10*(norm(E,1)+norm(A,1));
  [m,n]=size(E);
  if m<>n then error("Error: matrix should be square");end
  Ws=eye(n,n);
  //     
  rk=0;i=0;
  while rk  < n do
    if i==n then error("rowshuffle: singular pencil!");W=[];end
    [W,rk]=rowcomp(E);
    if rk==n then return;end
    W1=W(1:rk,:);W2=W(rk+1:n,:);
    E=[W1*E; -W2*A];
    A=[W1*A; -alfa*W2*A];
    Fs1=s*E-A;
    //     Update 
    Ws=[%zero + eye(rk,rk), %zero + zeros(rk,n-rk);
        %zero + zeros(n-rk,rk),(s-alfa)*eye(n-rk,n-rk)]*W*Ws;
    i=i+1;
  end
endfunction

