function [Q,Z,Ec,Ac,Qd,Zd,numbeps]=quaskro(E,A,tol)
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
// quasi- Kronecker form: s*Ec - Ac = Q*(sE-A)*Z
//
//             | sE(eps)-A(eps) |        X       |      X     |
//             |----------------|----------------|------------|
//             |        O       | sE(inf)-A(inf) |      X     |
//  Q(sE-A)Z = |=================================|============|
//             |                                 |            |
//             |                O                | sE(r)-A(r) |
//
// Ec=Q*E*Z, Ac=Q*A*Z, eps=Qd(1) x Zd(1) ,inf=Qd(2) x Zd(2)
// r = Qd(3) x Zd(3)
// numbeps(1) = # of eps blocks of size 0 x 1
// numbeps(2) = # of eps blocks of size 1 x 2
// numbeps(3) = # of eps blocks of size 2 x 3     etc...
// interface  from Slicot-fstair (F.D.) 
// T. Beelen's routines
//
  if nargin==1 then 
    [E,A]=pen2ea(E);tol=1.E-10;
  end
  if nargin==2 then
    if type(E,"short")=="p" then [E,A]=pen2ea(E);end  //quaskro(pencil,tol)
    if type(E,"short")=="m" then tol=1.E-10;end  //quaskro(E,A);
  end
  [na,ma]=size(A);
  Q=eye(na,na);Z=eye(ma,ma);
  if ~isempty(E) then nE=norm(E,1);else nE=0;end
  [E,Q,Z,stair,rk]=ereduc(E,1000*%eps+tol*nE)
  A=Q*A*Z;
  if ~isempty(A) then
    tol=tol*max([norm(A,"fro"),norm(E,"fro")])+10*tol;
  else
    tol=0
  end
  [Ac,Ec,Q,Z,muk,nuk,muk0,nuk0,mnei]=fstair(A,E,Q,Z,stair,rk,tol)
  numbeps=muk0-nuk0;
  Qd=[mnei(1),mnei(3),na-mnei(1)-mnei(3)];
  Zd=[mnei(2),mnei(3),ma-mnei(2)-mnei(3)];
endfunction
