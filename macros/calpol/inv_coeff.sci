function P=inv_coeff(A,d=[],name='x')
  // Scilab ( http://www.scilab.org/ ) - This file is part of Scilab
  // Copyright (C) 2016-2017 - Jean-Philippe Chancelier (Enpc)
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

  // inverse function of coeff
  [m,n]=size(A);
  if isempty(d) then nc = int(n/m); else nc = d+1; end
  if int(n/nc) <> m then
    error(sprintf("Error: incompatible input arguments d=%d",d));
  end
  ce= m2ce(A,[1,m+1],[1:n/nc:n+1]);
  P=m2p(ce{$},var=name,dim=".");
  for i=(size(ce,'*')-1):-1:1 do
    P= poly(0,name)*P + m2p(ce{i},var=name,dim=".");
  end
endfunction
