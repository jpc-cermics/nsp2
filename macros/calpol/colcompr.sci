function [Y,rk,Ac]=colcompr(A)
// Scilab ( http://www.scilab.org/ ) - This file is part of Scilab
// Copyright (C) 1987-2016 - F. Delebecque et all (INRIA)
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

// column compression of polynomial matrix A
// (left compression)
// Y = right unimodular base
// rk = normal rank of A
// Ac = A*Y
// see rowcompr
//!
// 
  [m,n]=size(A);
  [Ac,Y,rk]=htrianr(A);
endfunction
