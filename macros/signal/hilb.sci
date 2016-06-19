function [xh]=hilb(n,wtype,par)
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
  
//<xh>=hilb(n[,wtype][,par])
//returns the first n points of the
//Hilbert transform centered around the origin.
//That is, xh=(2/(n*pi))*(sin(n*pi/2))**2.
//Window type and window parameters are optional.
// n     :Number of points in filter (must be odd)
// wtype :window type ('re','tr','hn','hm','kr','ch')
//       :     default wtype='re'
// par   :window parameter for wtype='kr' or 'ch'
//       :     default par=<0 0>
//       :see the macro window for more help
// xh    :Hilbert transform
//
//  Author: C. Bunks  date: 3 Jan 1989

   if nargin==1 then
      wtype='re';
      par=[0 0];
   else if nargin==2 then
      par=[0 0];
   end
   end
   if int(n/2)==n/2 then
      error('Error---Filter length must be odd')
   end
   no2=(n-1)/2;
   th=0*ones(1,no2);
   th(1:2:no2)=ones_deprecated(1:2:no2)./(1:2:no2);
   xh=[-th(no2:-1:1),0,th];
   xh=2*xh/%pi;
 
   [win_l,cwp]=window(wtype,n,par);
   xh=xh.*win_l;
endfunction
