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

function fill3(varargin,varargopt)
  // dessine des polygones 
  // avec shading 

  if length(varargin)==0 then
    X=[0,1,1,2;1,1,2,2;0,0,1,1];
    Y=[1,1,1,1;1,0,1,0;0,0,0,0];
    Z=[1,1,1,1;1,0,1,0;0,0,0,0];
    C=[0.5000,1.0000,1.0000,0.5000;1.0000,0.5000,0.5000,0.1667;0.3330,0.3330,0.5000,0.5000];
    fill3(X,Y,Z,C);
    return;
  end

  nsets=length(varargin)/4;
  if 4*nsets <> length(varargin) then
    error("fill3: expecting 4*n arguments");
    return
  end
  options=hash(back_color = -1,mesh = %t,shade = %t);

  fig();
  F=get_current_figure();
  fig(axes_init = '3d');
  A=get_current_objs3d();
  if F.gc.auto_clear then A.children=list();end

  [ok,msg,varargin,H]=detect_options('fill3',varargin,varargopt);
  if ~ok then error(msg);return;end

  // if no special colormap we set a jetcolormap
  if H.iskey['colormap'] then
    A.colormap=H.colormap;
  elseif isempty(F.gc.colormap) then
    A.colormap=jetcolormap(32);
  end
  if H.iskey['azimuth'] then A.theta=H.azimuth;end
  if H.iskey['elevation'] then A.alpha=90-H.elevation;end
  if H.iskey['axesflag'] && H.axesflag==0 then A.with_box=%f;end
  if H.iskey['rect'] && length(H.rect)==6 then A.ebox=H.rect;A.fixed=%t;end

  val=1;
  for i=1:nsets do
    [X,Y,Z,C]=varargin(1:4);
    varargin=varargin.sublist[4:length(varargin)];
    [nv,np]=size(X);
    if nv==1 then nv=np;np=1;end
    if type(C,'short')=='m' then
      if and(size(C)==[nv,np]) then
        val=C(:);// one color per vertex 
      elseif size(C,1)==1 && size(C,2)==np then
        // one color for each polygon 
        val=ones(nv,1)*C;
        options.shade=%f;
      elseif size(C,2)==1 && size(C,1)==nv then
        val=C*ones(1,np);
      elseif size(C,'*')==1 then
        val=C*ones(nv,np);
        options.shade=%f;
      else
        error(sprintf("fill3: colors should be of size %dx1 or 1x%d or %dx%d",nv,np,nv,np));
      end
    elseif type(C,'short')=='s' then
      val=1;// WIP
    end
    faces=matrix(1:nv*np,nv,np);
    coord=[X(:),Y(:),Z(:)];
    S=spolyhedron_create(options(:),Mcoord = coord,Mface = faces,Mval = val, ...
                         vmin = min(val),vmax = max(val));
    A.children($+1)=S;
  end
  A.invalidate[];
endfunction
