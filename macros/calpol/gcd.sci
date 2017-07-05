function [q,fact]=gcd(p)
  // Copyright  2013-2017 Jean-Philippe Chancelier Cermics/Enpc
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

  error("Error: gcd not implemented for "+type(p,'string'));
  return;
endfunction

function [g,uu]=gcd_i(p)
  // g: the gcd of an int vector p
  // uu: a unimodular matrix such that
  //     [p1 p2 ... pn ]*uu=[0 ... 0 g]
  // Copyright INRIA (CeCILL)
  if nargin<>1 then
    error("Error: gcd should be called with one argument");
    return;
  end
  mn=size(p,'*');it=p.itype[];
  p.redim[1,-1];// resize to row vector
  g=p(1);
  if nargout<=1 then
    for l=2:mn do g=euclide(g,p(l));end
    return;
  end

  uu=m2i(1,it);
  for l=2:mn do
    [g,U]=euclide(g,p(l));u=U{1,1};
    uu=[uu(:,1:l-2),uu(:,l-1)*u(1,[2,1])];uu(l,l-1:l)=u(2,[2,1]);
  end
endfunction

function [g,uu]=bezout_i_i(i1,i2)
  [g,uu]=gcd_i([i1,i2])
  uu=uu(:,$: -1: 1);
endfunction

function [g,uu]=gcd_m(p)
  // g:  the gcd of a vector p casted to int
  // uu: a unimodular matrix u
  // such that [p1 p2 ... pn ]*uu=[0 ... 0, g]
  //
  // Copyright INRIA (CeCILL)
  if nargin<>1 then
    error("Error: gcd should be called with one argument");
    return;
  end
  mn=size(p,'*');
  p.redim[1,-1];// resize to row vector
  g=p(1);
  if nargout<=1 then
    for l=2:mn do g=euclide(g,p(l));end
    return;
  end
  uu=1;
  for l=2:mn do
    [g,U]=euclide(g,p(l));u=U{1,1};
    uu=[uu(:,1:(l-2)),uu(:,l-1)*u(1,[2,1])];
    uu(l,(l-1):l)=u(2,[2,1]);
  end
endfunction

function [g,uu]=bezout_m_m(i1,i2)
  [g,uu]=gcd_m([i1,i2])
  uu=uu(:,$: -1: 1);
endfunction
