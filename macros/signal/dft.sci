function xf=dft(x,flag);
// Scilab ( http://www.scilab.org/ ) - This file is part of Scilab
// Copyright (C) 1987-2016 - C. Bunks (INRIA)
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
  
//xf=dft(x,flag);
//macro which computes dft of vector x
// x    :input vector
// flag :indicates dft or idft
// xf   :output vector
// Author: C. Bunks  date: 29 Sept 1988
// 

  n=max(size(x));
  arg=(0:n-1);
  am=-2*%pi*%i*arg'*arg/n;
  if flag==1 then
    am=-am;
  end
  xf=exp(am)*matrix(x,n,1);
  if flag==1 then
    xf=xf/n;
  end
endfunction
