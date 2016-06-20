function [n,d]=cmndred(num,den)
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

// Syntax: [num,den]=cmndred(num,den)
// Given the transfert matrix defined by num./den, 
// cmndred computes polynomial matrix n and a common 
// denominator d such that n/d=num./den
//!
// 
  [m1,n1]=size(num)
  d=1;for dk=matrix(den,1,m1*n1),d=lcm([d,dk]),end
  for l=1:m1
    for k=1:n1
      n(l,k)=num(l,k)*pdiv(d,den(l,k));
    end;
  end;
endfunction
