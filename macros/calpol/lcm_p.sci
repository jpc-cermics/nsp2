function [p,fact]=lcm_p(p)
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

  // p=lcm(p) computes the lcm of polynomial vector p
  // [pp,fact]=lcm(p) computes besides the vector fact of factors 
  // such that  p.*fact=pp*ones(p)
  //!
  // 
  [m,n]=size(p),
  p=matrix(p,m*n,1),
  p0=p(1);fact=1;
  for l=2:m*n do
    [u,v]=simp(p0,p(l)),
    p0=p0*v,
    fact=[v*fact,u],
  end,
  fact=matrix(fact,m,n),
  p=p0;
endfunction
