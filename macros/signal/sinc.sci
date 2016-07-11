function y=sinc(x,fl)
// Scilab ( http://www.scilab.org/ ) - This file is part of Scilab
// Copyright (C) 1987-2016 - C. Bunks
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

//               [  sin(x(i))/x(i) if x(i)~=0
// computes y(i)=[
//               [  1 if x(i)~=0

  if nargin == 2 then 
    // printf("Warning: deprecated use of sinc, use filt_sinc instead\n");
    y=filt_sinc(x,fl)
    return
  end
  y=ones(x)
  kz=find(x<>0)
  y(kz)=sin(x(kz))./(x(kz));
endfunction

