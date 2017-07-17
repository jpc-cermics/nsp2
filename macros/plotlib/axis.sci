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

function y=axis(varargin,varargopt)
  y=[];
  while length(varargin) >= 1 do
    if length(varargin) >= 1 then
      if type(varargin(1),'short')=='m' then
        rect=varargin(1);
        if length(rect)==4 then
          if isempty(winsid()) then fig();end
          winNum=xget('window');
          xsetech(frect = rect([1,3,2,4]));// convert to [xmin xmax ymin ymax]
          varargin.remove_first[];
        end
      elseif type(varargin(1),'short')=='s' then
        select varargin(1)
          case 'manual' then xsetech(fixed = %t);
          case 'auto' then xsetech(fixed = %f);
        end
      else
        error("axis: argument have wrong type (%s)\n");
        return
      end
      varargin.remove_first[];
    end
  end

  [ok,msg,varargin,H]=detect_options('axis',varargin,varargopt);
  if ~ok then error(msg);return;end

  if H.iskey['manual'] then xsetech(fixed = %t);end

  if nargout==1 then
    F=get_current_figure();
    if length(F.children) >= 1 then
      A=F.children(1);
      if type(A,'short')=='objs3d' then
        y=A.get_ebox[];
      else
        y=A.frect;
        y=[y(1),y(3),y(2),y(4)];
      end
    else
      y=[];
    end
  end
endfunction
