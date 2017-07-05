function y=coeff(p,num)
  // Copyright  2016-2017 Jean-Philippe Chancelier Cermics/Enpc 
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
  // emulates the scicoslab function coeff 
  // in nsp coeffs can be obtained with attributes coeffs 
  //
  if type(p,'short')== 'm' then 
    p = p+0*poly(0,'x');
  end
  if nargin == 1 then 
    C=p.coeffs;
    d=max(p.degree[]);
    y=[];
    if isempty(d) then return;end
    for i=1:d+1 do
      y.concatr[ce2m(C,indice=i,noti=0)];
    end
  else
    C=p.coeffs;
    y=[];
    for i=num do
      y.concatr[ce2m(C,indice=i+1,noti=0)];
    end
  end
endfunction
