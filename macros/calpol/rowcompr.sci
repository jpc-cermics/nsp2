function [X,rk,Ac]=rowcompr(A)
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

  //[X,rk,Ac]=rowcompr(A)
  //row compression of polynomial matrix A (toward the bottom)
  //X left polynomial unimodular base
  //rk=normal rank of A
  //Ac = X*A
  //Warning: elimination of neglected terms
  //!
  // 
  [n,m]=size(A);
  [Ac,U,rk]=htrianr(A');
  Ac=Ac';
  X=U';
  X=X(n:-1:1,:)
  Ac=Ac(n:-1:1,:)
endfunction
