function c=cond(A)
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
// c=cond(A) condition number for 2-norm
  if type(A,"short")=="m" then
    if isempty(A) then c=1,return,end
    if size(A,1)<>size(A,2) then error("Matrix must be square"),end
    s=svd(A)
    if s($)==0 then
      c=%inf
    else
      c=s(1)/s($)
    end
  else
    error(sprintf("Error: cond not defined for type %s\n",type(A,"string")));
  end
endfunction
