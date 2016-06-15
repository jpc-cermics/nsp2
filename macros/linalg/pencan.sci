function [Q,M,i1]=pencan(E,A)
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

// [Q,M,i1]=pencan(E,A)
// Given the pencil s*E-A pencan returns matrices Q and M
// such than M*(s*E-A)*Q is in "canonical" form i.e.
// M*E*Q is a block matrix [I,0;
//                         0,N]    with N nilpotent
// and i1 = size of the I matrix above (# of finite zeros of (sE-A)).
// M*A*Q is a block matrix [Ar,0;
//                         0,I ]
// See glever,  penlaur

  if nargin==1 then [E,A]=pen2ea(E);end
  [Si,Pi,Di,index]=penlaur(E,A);
  [Q1,M1]=fullrf(Si);
  [Q2,M2]=fullrf(Pi);
  [i1,i2]=size(M1);
  M=[M1;M2];
  Q=[Q1,Q2];
endfunction
