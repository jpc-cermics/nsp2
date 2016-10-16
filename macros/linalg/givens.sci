function [u,c]=givens(x,y)
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
//Syntax : u=givens(xy)
//         u=givens(x,y)
//
// xy = [x;y], u=givens(xy)
// returns a 2*2 matrix u such that u*xy=[r;0].
// c is equal to u*xy
// givens(x,y)=givens([x;y])
//
  if nargin==2 then x=[x;y];end
  if or(size(x)<>[2,1]) then 
    error("givens: argument must be a column vector")
  end
  if x(2)<>0 then
    r = norm(x);
    u = [x'; -x(2), x(1)]/r;
    c = [r; 0];
  else
    u=eye(2,2)
    c=x
  end
endfunction
