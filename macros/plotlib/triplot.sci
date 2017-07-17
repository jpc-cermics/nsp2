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

function triplot(varargin,varargopt)
  // triplot(tri,x,y) draws triangles defined by m-by-3 matrix tri.
  //   each row of tri define a triangle by the indices of its vertex into 
  //   the vertex arrays given by the vectors x and y. 
  // It is a redirection to the fec function 
  // WIP: need to map options to fec options 

  if length(varargin) <= 0 then
    // load('SCI/macros/mottelet/tridem.nsp')
    // triplot(T,x,y);
    exec('SCI/macros/mottelet/triplot-demo.sce');
    triplot(T,pts(:,1),pts(:,2));
    return;
  end

  select length(varargin)
    case 3 then
     T=varargin(1);x=varargin(2);y=varargin(3);
    case 4 then
     T=varargin(1);x=varargin(2);y=varargin(3);
     // options.colors = varargin(4);
    else
      error("triplot: number of arguments should be 3 or 4 followed by named options");
      return;
  end

  fig();
  F=get_current_figure();
  fig(axes_init = '2d');

  [ok,msg,varargin,H]=detect_options('triplot',varargin,varargopt);
  if ~ok then error(msg);return;end

  options.mesh=%t;
  options.colorbar=%f;
  options.axesflag=2;
  options.paint=%f;

  T1=[ones(size(T,1),1),T,ones(size(T,1),1)]
  z=zeros(size(x));
  fec(x,y,T1,z,options(:));
  if H.iskey['Xlabel'] && H.Xlabel <> "" then xlabel(H.Xlabel);end
  if H.iskey['Ylabel'] && H.Ylabel <> "" then xlabel(H.Ylabel);end
  if H.iskey['Title'] && H.Title <> "" then title(H.Title);end
endfunction
