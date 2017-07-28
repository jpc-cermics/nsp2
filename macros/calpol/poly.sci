function y=poly(x,varargin,varargopt)
  // Copyright  2010-2017 Jean-Philippe Chancelier Cermics/Enpc 
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
  // and extensions using optional named arguments
  //
  opts=hash( var='x',roots=%t);
  if length(varargin) >= 1 then opts.var=varargin(1);end
  if length(varargin) >= 2 then opts.roots= (varargin(2)== 'roots' || varargin(2) == 'r');end
  opts.merge[varargopt];
  
  if size(x,1) <> 1 && size(x,2) <> 1 && size(x,1)==size(x,2) then
    y=det(eye(size(x))*m2p([0,1],var=opts.var)-x);
    return ;
  end
  if opts.roots then 
    y=m2p(1,var=opts.var);
    for i=1:size(x,'*') do
      yp=m2p([0,1],var=opts.var);
      y = y*(yp-x(i));
    end
  else
    y = m2p(x,var=opts.var);
  end
endfunction
