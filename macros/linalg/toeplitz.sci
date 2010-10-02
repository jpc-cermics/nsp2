function A=toeplitz(c,r)
// Copyright (C) 2010-2010 Jean-Philippe Chancelier 
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
// creates a toeplitz matrix and should work for most matrix types.
  if nargin==1 then 
    r=c;
  else
    if ~type(c).equal[type(r)] then 
     error("Error: the two arguments should have the same type");
    end
    if ~c(1).equal[r(1)] then 
      printf("Warning: incompatible values in c and r using c value\n");
    end
  end 
  nr=size(c,'*');
  nc=size(r,'*');
  A= type(c).new[nr,nc];
  for i=1:nc ; A.set_diag[r(i),i-1];end
  for i=1:nr ; A.set_diag[c(i),-(i-1)];end
endfunction

function A=hankel(c,r)
// Copyright (C) 2010-2010 Jean-Philippe Chancelier 
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
// creates a hankel matrix (Matlab semantics) and should work for most
// matrix types.
  if nargin==1 then 
    A=toeplitz(c($:-1:1),c($:-1:1));
    A=tril(A);
    A=A($:-1:1,:);
  else
    A=toeplitz(c($:-1:1),r);
    A=A($:-1:1,:);
  end 
endfunction

    
