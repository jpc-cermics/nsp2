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
// Copyright (C) 2016-2017 - Stephane Mottelet, Jean-Philippe Chancelier

function colormap(varargin,varargopt)
  [ok,msg,varargin,H]=detect_options('colormap',varargin,varargopt);
  if ~ok then error(msg);return;end
  if ~H.iskey['colormap'] then
    if length(varargin) <> 0 then
      [ok,msg,varargin,H]=detect_options('colormap',list(),hash(colormap = varargin(1)));
    end
  end
  if H.iskey['colormap'] then
    F=get_current_figure();
    A=get_current_axes_or_objs3d(create = %f);
    if type(A,'short')=='objs3d' then
      A.colormap=H.colormap;
      A.invalidate[];
    else
      xset('colormap',H.colormap);
    end
  end
endfunction
