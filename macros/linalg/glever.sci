function [Bfs,Bis,chis]=glever(E,A,s)
// Scilab ( http://www.scilab.org/ ) - This file is part of Scilab
// Copyright (C) 1988-2016 - F. Delebecque (Inria)
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
// [Bfs,Bis,chis]=glever(E,A,'s')
// Computation of (s*E-A)^-1 ('s'=character string with default value 's')
// Generelized Leverrier's algorithm for a matrix pencil ;
// (s*E-A)^-1 = (Bfs/chis) - Bis
// chis = characteristic polynomial (up to a multiplicative constant)
// Bfs  = polynomial matrix
// Bis  = polynomial matrix ( - expansion of (s*E-A)^-1 at infinity).
// Caveat: uses clean to simplify Bfs,Bis and chis !
// See also shuffle, determ, invr, coffg
//
  if nargin==1 then s=poly(0,E.get_var[]);[E,A]=pen2ea(E);end 
  if nargin==2 then s=poly(0,"s"),end
  if nargin==3 then s=poly(0,s);end
  [Si,Pi,Di,index]=penlaur(E,A);
  k=round(sum(diag(Si*E)));

  a0=1;
  B=Si;
  SiASi=Si*A*Si;
  chis=a0+0*s;
  Bfs=Si+0*s*Si;
  
  for i=1:k do
    B=SiASi*E*B;
    alfa=-sum(diag(E*B))/i;
    B=B+alfa*Si;
    chis=s*chis+alfa;
    if i<k then Bfs=s*Bfs+B;end
  end
  Bis=Pi;
  AAD=s*A*Di;
  P=eye(size(A));
  
  for nu=1:index+1 do
    P=AAD*P;
    Bis=clean(Bis+Pi*P,1.E-10);
  end
  Bfs=clean(Bfs,1.E-10);
  chis=clean(chis,1.E-10);
endfunction
