function [q,fact]=gcd(p)
// Copyright  2013-2013 Jean-Philippe Chancelier Cermics/Enpc 
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

  error("gcd not implemented for "+type(p,'string')+'\n');
  return;
endfunction

function [x]=gcd_i(p)
// computes the gcd of an int vector 
// and a unimodular matrix (with polynomial inverse) u, 
// with minimal degree such that [p1 p2]*u=[0 ... 0 pgcd]
// XXXX reste le calcul de u noter que dans euclide on n'a pas 
// directement u.
//
// Copyright INRIA
  mn=size(p,'*');
  if mn == 0 then x=p;uu=p;end
  one=m2i(ones(mn,mn),p(1).itype[]);
  x=p(1);
  for l=2:mn,
    // [x,u]=bezout(x,p(l)),
    [x,b,c]=euclide(x,p(l));
    if nargout==2 then
      // one=[one(:,1:l-2),one(:,l-1)*u(1,[2 1])];one(l,l-1:l)=u(2,[2 1]);
    end
  end,
endfunction


function [x]=gcd_m(p)
// the version with two arguments is hard coded.
// computes the gcd of an int vector 
// and a unimodular matrix (with polynomial inverse) u, 
// with minimal degree such that [p1 p2]*u=[0 ... 0 pgcd]
// XXXX reste le calcul de u noter que dans euclide on n'a pas 
// directement u.
//
// Copyright INRIA
  mn=size(p,'*');
  if mn == 0 then x=p;uu=p;end
  one=1;
  x=p(1);
  for l=2:mn,
    // [x,u]=bezout(x,p(l)),
    [x,b,c]=euclide(x,p(l));
    if nargout==2 then
      // one=[one(:,1:l-2),one(:,l-1)*u(1,[2 1])];one(l,l-1:l)=u(2,[2 1]);
    end
  end,
endfunction

