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

function tripcolor(T,x,y,z,varargin,varargopt)
  // we use fec 

  if nargin <= 0 then
    S=["exec(''NSP/macros/mottelet/triplot-demo.sce'');";
       "tripcolor(T,pts(:,1),pts(:,2),pts(:,3));"];
    printf("%s\n",S);
    execstr(S);
    return;
  end

  fig();
  F=get_current_figure();
  fig(axes_init = '2d');
  auto_clear=F.gc.auto_clear;

  [ok,msg,varargin,H]=detect_options('tripcolor',varargin,varargopt);
  if ~ok then error(msg);return;end

  options.mesh=%t;
  options.colorbar=%t;
  options.axesflag=0;
  options.paint=%t;

  T1=[ones(size(T,1),1),T,ones(size(T,1),1)]
  xset('colormap',jetcolormap(64));
  fec(x,y,T1,z,options(:));//zminmax=[min(z), max(z)]);

  if H.iskey['Xlabel'] && H.Xlabel <> "" then xlabel(H.Xlabel);end
  if H.iskey['Ylabel'] && H.Ylabel <> "" then xlabel(H.Ylabel);end
  if H.iskey['Title'] && H.Title <> "" then title(H.Title);end
endfunction
