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

function pcolor(varargin,varargopt)
  // WIP: on peut avoir aussi x,y,f 
  // ou f est une fonction etc.....
  // C'est un peu bizarre 
  // X(i,j), Y(i,j) est dessiné avec la couleur C(i,j) 
  // 
  // A pseudocolor plot is a flat surface plot viewed from above. 
  // pcolor(X,Y,C) is the same as viewing surf(X,Y,zeros(size(X)),C) using view([0 90]).
  // C is a matrix 
  // WIP: options + C as a function 

  if length(varargin)==0 then
    if %f then
      n=20;
      r=linspace(0,1,n);
      theta=linspace(0,2*%pi,n);
      [R,T]=meshgrid(r,theta);
      Z=T .*R;
      pcolor(R .*cos(T),R .*sin(T),Z);
    else
      n=10;
      y=linspace(0,2*%pi,3*n)';Y=y*ones(1,n);
      Xmin=sin(Y);Xmax=2.2+cos(Y);
      X=ones(3*n,1)*linspace(0,1,n);
      X=Xmin+Xmax .*(X-Xmin) ./(Xmax-Xmin);
      pcolor(X,Y,X+Y);
    end
    return
  end

  fig();
  F=get_current_figure();
  if isempty(F.gc.colormap) then F.gc.set[colormap = jetcolormap(32)];end
  fig(axes_init = '2d');
  auto_clear=F.gc.auto_clear;

  [ok,msg,varargin,H]=detect_options('pcolor',varargin,varargopt);
  if ~ok then error(msg);return;end
  if H.iskey['colormap'] then xset('colormap',H.colormap);end

  select length(varargin)
    case 1 then
      A=get_current_axes();
      if auto_clear then A.children=list();end
      // one argument like Matplot 
      Z=varargin(1);[m,n]=size(Z);
      if auto_clear then F.children=list();end
      Matplot(Z,remap = %t,rect = [1,1,m,n]);
    case 3 then
      // Three arguments 
      A=get_current_axes();
      if auto_clear then A.children=list();end
      X=varargin(1);Y=varargin(2);Z=varargin(3);
      if and([size(X)==size(Y),size(X)==size(Z)]) then
	xsetech(frect = [min(X),min(Y),max(X),max(Y)]);
	// remap colors according to colomap size
	ncol=size(F.gc.colormap,1);
	Z=ncol*(Z-min(Z))/(max(Z)-min(Z));
	for i=1:size(X,1)-1 do
          for j=1:size(X,2)-1 do
            xfpoly([X(i,j),X(i+1,j),X(i+1,j+1),X(i,j+1)], ...
                   [Y(i,j),Y(i+1,j),Y(i+1,j+1),Y(i,j+1)],fill_color = Z(i,j));
          end
	end
      elseif length(X)==size(Z,1) && length(Y)==size(Z,2) then
	[X,Y]=meshgrid(X,Y);
	pcolor(X,Y,Z,varargopt(:));
      else
	error("pcolor: X, Y and Z should share the same size or size(Z)=[length(X),length(Y)];");
      end
    else
      error("pcolor: should be called with one or three explicit parameters");
  end
endfunction
