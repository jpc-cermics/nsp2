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
// Adapted from Stephane Mottelet scicoslab plotlib toolbox.
// Copyright (C) 2016-2022 - Stephane Mottelet, Jean-Philippe Chancelier

function hold(txt)
  if isempty(winsid()) then return;end
  fig();
  F=get_current_figure();
  select nargin
    case 0 then F.gc.set[auto_clear = ~F.gc.auto_clear];
    case 1 then
     if type(txt,'short')=='s' then
       select txt
         case 'on' then F.gc.set[auto_clear = %f];
         case 'off' then F.gc.set[auto_clear = %t];
         else
           error("hold: argument should be ''on'' or ''off''");
           return;end
     else
       error("hold: argument should be ''on'' or ''off''");
     end
    else
      error("hold: 0 or one argument expected");
      return;
  end
endfunction
