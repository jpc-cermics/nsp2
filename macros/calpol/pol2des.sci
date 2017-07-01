function [N,B,C]=pol2des(Ds)
  // Scilab ( http://www.scilab.org/ ) - This file is part of Scilab
  // Copyright (C) 1987-2017 - F. Delebecque et all (INRIA)
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

  // Given the polynomial matrix Ds= D_0 +D_1 s +D_2 s^2 + ... +D_k s^k,
  // pol2des returns three  matrices N,B,C  (with N nilpotent) such that
  // Ds = C (sN-Eye)^-1 B 
  //!
  // 
  if type(Ds,'short')=='m' then Ds=Ds+0*poly(0,'s');end
  dg=max(Ds.degree[])+1;
  [nout,nin]=size(Ds);
  Sl=markp2ss(coeff(Ds),dg,nout,nin);
  N=Sl(2);B=-Sl(3);C=Sl(4)
endfunction
