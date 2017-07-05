function [q,fact]=lcm(p)
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

  error("lcm not implemented for "+type(p,'string')+'\n');
  return;
endfunction

function [q,fact]=lcm_i(p)
  // q: lcm of an int vector 
  // fact: the vector of factors i.e 
  //    such that p.*fact == q
  // jpc: Aug 2013 using hard coded internal lcm_m_m 

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
  // Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307
  // USA
  if nargin<>1 then
    error("Error: lcm should be called with one argument");
    return;
  end
  if isempty(p) then q=p;fact=p;return;end 
  q=p(1);
  fact=m2i(1,p(1).itype[][1]);
  for k=2:size(p,'*') do
    [q,U,idet]=lcm(q,p(k)); u = U{1,1};   
    f = [ u(2,2), - u(1,2)]*idet;
    fact=[fact * f(2), f(1)];
  end
endfunction

function [q,fact]=lcm_m(p)
  // q: lcm of  of a double vector (casted to integer)
  // fact: the vector of factors i.e 
  //    such that p.*fact == q
  // jpc: Aug 2013 using the hard coded internal lcm_m_m 

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
  if nargin<>1 then
    error("Error: lcm should be called with one argument");
    return;
  end
  if isempty(p) then q=p;fact=p;return;end 
  q=p(1);
  fact=[1];
  for k=2:size(p,'*') do
    [q,U,idet]=lcm(q,p(k)); u = U{1,1};   
    f = [ u(2,2), - u(1,2)]*idet;
    fact=[fact * f(2), f(1)];
  end
endfunction
