function y=poly(x,y,name='x',roots=%t)
// Copyright  2010 Jean-Philippe Chancelier Cermics/Enpc 
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
// emulation of the poly scilab function
// the name is not use and should be in the future.
//
  if size(x,1) <> 1 && size(x,2) <> 1 then 
    y=det(eye(size(x))*m2p([0,1])-x*m2p([1]))
    return ;
  end
  if roots then 
    y=m2p(1);
    for i=1:size(x,'*')
      yp=m2p([0,1]);
      y = y*(yp-x(i));
    end
  else
    y = m2p(x);
  end
endfunction

