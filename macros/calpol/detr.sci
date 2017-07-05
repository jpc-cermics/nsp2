function d=detr(h)
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

  // Determinant of a matrix 
  // using Leverrier's method
  // 
  t=type(h,'short');
  if or(t==['m','p','r']) then 
    [m,n]=size(h);
    if m<>n then error("Error: matrix should be square\n");return;end
    f=eye(n,n);
    for k=1:n-1 do
      b=h*f
      d=-sum(diag(b)) ./ k
      f=b+eye(n,n)*d,
    end
    d=-sum(diag(h*f)) ./ n;
    if 2*int(n/2)<>n then d=-d;end
  else
    error("Error: expecting numerical, polynomial or rational matrix\n");
  end
endfunction
