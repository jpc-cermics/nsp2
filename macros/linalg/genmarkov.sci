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

function M=genmarkov(rec,tr,flag)
// returns a random Markov transition probability matrix
// with size(rec,1) recurrent classes with rec(1),...rec($) 
// entries respectively and tr transient states.
// If the optional parameter flag='perm' is entered a random
// permutation of the states is performed.
// 
  if nargin==2 then flag="noperm";end
  M=[];r=sum(rec);
  for k=rec do
    m=rand(k,k);
    m=m./(sum(m,"c")*ones(1,k));
    M=[M # m];
  end
  if type(tr,"short")=="m" then
    n=r+tr;
    MT=rand(tr,n);MT=MT./(sum(MT,"c")*ones(1,n));
    M=[[M,zeros(r,tr)];MT];
  else
    // tr=list(n1,[a1,a2,...ar],n2,[b1,...br],...)
    l=size(tr)/2;   //2*size(rec,2)
    Q=[];
    for kk=1:l do
      nt=tr(1+2*(kk-1));
      Q=sysdiag(Q,rand(nt,nt));
    end
    Nt=size(Q,1);
    L=[];
    nclrec=size(rec,"*");
    for kk=1:l do
      L1=[];indi=tr(2+2*(kk-1));nt=tr(1+2*(kk-1));
      for i=1:nclrec do
	if indi(i)==0 then
	  L1=[L1,zeros(nt,rec(i))]; 
	else
	  L1=[L1,rand(nt,rec(i))];
	end
      end   
      L=[L;L1]
    end
    LQ=[L,Q];
    LQ=LQ./(sum(LQ,"c")*ones(1,size(LQ,2)));
    M=[[M,zeros(size(M,1),size(Q,2))];LQ]; 
  end
  if flag=="perm" then p=grand(1,"perm",n);M=M(p,p);end
endfunction
