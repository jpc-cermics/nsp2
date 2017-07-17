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

function main_trisurf(typeOfPlot,argList,argOpt,uselight = %f,mesh_only = %f)
  function l=computeLight(n,vect)
    l=n'*vect;
    l=l .*b2m(l > 0);
  endfunction

  function C=trisurf_light_mot(lightVect,coord,nodes)
    lightVect=lightVect/norm(lightVect,2);
    ntri=size(nodes,2);
    nnodes=size(coord,1);
    X=matrix(coord(nodes,1),3,-1);
    Y=matrix(coord(nodes,2),3,-1);
    Z=matrix(coord(nodes,3),3,-1);
    A=[X(2,:)-X(1,:);Y(2,:)-Y(1,:);Z(2,:)-Z(1,:)];
    B=[X(3,:)-X(1,:);Y(3,:)-Y(1,:);Z(3,:)-Z(1,:)];
    N=cross(A,B);
    clear X,Y,Z,A,B;
    // set the norm of each n vector to 1 
    D=1.  ./(%eps+sqrt(sum(N .^2,'r')));
    N=scale_cols(N,D);
    // for each node we compute the 
    // average value of lighting 
    L=computeLight(N,lightVect);
    C=zeros(nnodes,1);
    nV=zeros(nnodes,1);
    for i=1:ntri do
      xnodes=nodes(:,i);
      C(xnodes)=C(xnodes)+L(i);
      nV(xnodes)=nV(xnodes)+1;
    end
    clear L;
    C=C ./(nV+%eps);
  endfunction

  [ok,msg,argList,H]=detect_options(typeOfPlot,argList,argOpt);
  if ~ok then error(msg);return;end
  C=[];
  select length(argList)
    case 2 then
     [Tri,coord]=argList(1:2);
     if size(coord,2)==4 then C=coord(:,4);coord=coord(:,1:3);end
     if size(coord,2) <> 3 then
       error(sprintf("%s: second argument should be of size nx3 or nx4",typeOfPlot))
       return;
     end
    case 4 then
     [Tri,X,Y,Z]=argList(1:4);
     if ~(and(size(X)==size(Y)) && and(size(X)==size(Z))) then
       error(sprintf("%s: X, Y and Z should have the same sizes",typeOfPlot));
       return;
     end
     coord=[X(:),Y(:),Z(:)];
    case 5 then
     [Tri,X,Y,Z,C]=argList(1:5);
     if ~(and(size(X)==size(Y)) && and(size(X)==size(Z)) && and(size(C)==size(Z))) then
       error(sprintf("%s: X, Y, Z and C should have the same sizes"),typeOfPlot);
       return;
     end
     coord=[X(:),Y(:),Z(:)];C=C(:);
    else
      error(sprintf("%s: wrong number of arguments",typeOfPlot));
  end
  if size(Tri,2) <> 3 then
    error(sprintf("%s: first arguments should be of size nx3",typeOfPlot));
    return;
  end
  fig();
  F=get_current_figure();
  fig(axes_init = '3d');
  A=get_current_objs3d();

  if H.iskey['colormap'] then
    A.colormap=H.colormap;
  else
    // if no special colormap we set a jetcolormap
    F=get_current_figure();
    if isempty(F.gc.colormap) then A.colormap=jetcolormap(32);end
  end
  if H.iskey['azimuth'] then A.theta=H.azimuth;end
  if H.iskey['elevation'] then A.alpha=90-H.elevation;end
  if H.iskey['axesflag'] && H.axesflag==0 then A.with_box=%f;end
  if H.iskey['rect'] && length(H.rect)==6 then A.ebox=H.rect;A.fixed=%t;end
  if H.iskey['iso'] then if A.fixed then A.scale_flag=3;else A.scale_flag=4;end;end

  options=hash(back_color = -1,mesh = %t,shade = %f);
  if uselight then
    if H.iskey['lightVect'] then lightVect=H.lightVect;else lightVect=[1;1;1];end
    z=trisurf_light_mot(lightVect,coord,Tri');
    options.Mval=z;
    options.vmin=min(z);options.vmax=max(z);
  elseif ~isempty(C) then
    Z=C;
    options.Mval=Z;options.vmin=min(Z);options.vmax=max(Z);
  else
    Z=coord(:,$);
    options.Mval=Z;options.vmin=min(Z);options.vmax=max(Z);
  end
  S=spolyhedron_create(options(:),Mcoord = coord,Mface = Tri');
  if H.iskey['shade'] then S.shade=H.shade;end
  if mesh_only then S.mesh_only=%t;S.mesh=%t;end
  if F.gc.auto_clear then
    A.children=list(S);
  else
    A.children($+1)=S;
  end
  A.invalidate[];
endfunction

function trisurf(varargin,varargopt)
  if nargin==0 then
    S=["exec(''NSP/macros/mottelet/triplot-demo.sce'');";
       "trisurf(T,pts,shading=''interp'',colorbar=''on'');"];
    printf("%s\n",S);
    execstr(S);
  else
    main_trisurf('trisurf',varargin,varargopt);
  end
endfunction

function trisurfl(varargin,varargopt)
  if nargin==0 then
    S=["exec(''NSP/macros/mottelet/triplot-demo.sce'');";
       "trisurfl(T,pts,shading=''interp'',colorbar=''on'');"];
    printf("%s\n",S);
    execstr(S);
  else
    main_trisurf('trisurfl',varargin,varargopt,uselight = %t)
  end
endfunction

function trimesh(varargin,varargopt)
  if nargin==0 then
    S=["exec(''NSP/macros/mottelet/triplot-demo.sce'');";
       "trimesh(T,pts,shading=''interp'',colorbar=''on'');"];
    printf("%s\n",S);
    execstr(S);
  else
    main_trisurf('mesh',varargin,varargopt,mesh_only = %t);
  end
endfunction
