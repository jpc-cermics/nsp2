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
// 

function p=derivat_m(p)
  p=0*p;
endfunction

function p=derivat_p(p)
  p=p.derivative[]
endfunction

function res=derivat_r(r)
  res = r;
  dn= r.num.derivative[];
  dd= r.den.derivative[];
  N = dn .* r.den - r.num .* dd
  D = r.den .^2;
  res=p2r(N,D,simp=%t);
endfunction
