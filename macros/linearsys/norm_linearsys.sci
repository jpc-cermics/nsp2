// Copyright  2010-2015 
// Jean-Philippe Chancelier Cermics/Enpc, François Delebeceque, Serge Steer (Inria)
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

function y=norm_linearsys(A,flag)
  if nargin==1 then flag=2,end
  if flag==2 then
    y=h2norm(A)
  elseif flag==%inf || flag=='inf' then
    y=h_norm(A)
  else
    error('Error: flag must be 2 or inf.')
  end
endfunction
