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

function [M,Q]=eigenmarkov(P)
//Returns normalized left and right eigenvectors
//for the eigenvalue 1 of the Markov transition matrix P.
//If the multiplicity of this eigenvalue is m and P
//is N x N, M is a m x N matrix and Q a N x m matrix.
//M(k,:) is the probability distribution vector associated with the kth
//ergodic set (recurrent class). M(k,x) is zero if x is not in the
//k-th recurrent class.
//Q(x,k) is the probability to end in the k-th recurrent class starting
//from x. 
  [perm,L]=classmarkov(P)
  Mn=P(perm,perm);
  perminv= perm;
  perminv(perm)= 1:size(perm,"*");
  rec1= []; for i=1:size(L,"*")-1 do rec1($+1)=size(L{i},"*");end
  nr= sum(rec1); // number of reccurent states 
  T=Mn(nr+1:$,nr+1:$);L=Mn(nr+1:$,1:nr);
  p=0;V=[];
  for k=rec1 do
    v=L(:,p+1:p+k);V=[V,sum(v,"c")];
    p=p+k;
  end
  LL=zeros(nr,size(rec1,"*"));
  p=0;
  for k=1:size(rec1,"*") do
    LL(p+1:p+rec1(k),k)=1;
    p=p+rec1(k);
  end
  Q=[LL;inv(eye(size(T))-T)*V];
  Q=Q(perminv,:);
  p=0;M=[];
  for kk=1:size(rec1,"*") do
    classe=p+1:p+rec1(kk);
    p=p+rec1(kk);
    Mres=Mn(classe,classe);
    w=kernel((Mres-eye(size(Mres)))')';
    M=sysdiag(M,w./sum(w));
  end
  M=[M,zeros(size(M,1),size(P,1)-size(M,2))];
  M=M(:,perminv);
endfunction
