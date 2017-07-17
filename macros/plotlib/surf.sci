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

function main_surf(typeOfPlot,argList,argOpt,uselight = %f,mesh_only = %f)
  function [X,Y,Z]=generate3dPolygons(X,Y,Z,nbVertices,isParametric)
    //
    // Generates polygons (N=3 trianges or N=4 quadrilaters) from
    // (X,Y,Z) representations of surfaces 
    // see also eval3dp 
    // S. Steer, J.-Ph. Chancelier, S. Mottelet
    // non parametric case 
    //   X 1:p, Y 1:q and Z pxq
    //   [X,Y,Z]=generate3dPolygons(1:2,1:3,rand(2,3),3,%f)
    // parametric case 
    //   X,Y,Z  are thre matrices of the same size 
    //   we will obtain triangles (or quadrangles) of type 
    //       M(i,j),M(i,j+1),M(i+1,j+1),M(i+1,j) 
    //   n=6;
    //   X=cos((0:n-1)*2*%pi/n)';
    //   Y=sin((0:n-1)*2*%pi/n)';
    //   Z=ones(n,1);
    //   
    //   [X1,Y1,Z1]=generate3dPolygons([X,X],[Y,Y],[Z,Z+1],3,%t);
    //   plot3d1(X1,Y1,Z1);
    //   

    [nv,nu]=size(Z);
    if isParametric then
      X=X(:);Y=Y(:);Z=Z(:);
    else
      X=ones(1,nu) .*.matrix(X,1,nv);
      Y=matrix(Y,1,nu) .*.ones(1,nv);
    end
    if nbVertices==4 then
      ind=ones(1,nv-1) .*.[0,1,nv+1,nv]+(1:nv-1) .*.[1,1,1,1];
    elseif nbVertices==3 then
      ind=ones(1,nv-1) .*.[0,1,nv,1,nv+1,nv]+(1:nv-1) .*.[1,1,1,1,1,1];
    else
      error('generate3dPolygons : nb of vertices must be 3 or 4');
    end
    ind2=ones(1,nu-1) .*.ind+((0:nu-2)*nv) .*.ones(size(ind));
    n=prod(size(ind2));
    X=matrix(X(ind2),nbVertices,n/nbVertices);
    Y=matrix(Y(ind2),nbVertices,n/nbVertices);
    Z=matrix(Z(ind2),nbVertices,n/nbVertices);
  endfunction

  function l=computeLight(n,vect)
    l=n'*vect;
    l=l .*b2m(l > 0);
  endfunction

  function [zx,zy]=nonParametricDiffData(x,y,Z)
    nx=length(x);
    ny=length(y);
    zx=sparse(Z(:,2:$)-Z(:,1:$-1))*sparse([1:nx-1;1:nx-1]',1 ./(x(2:$)-x(1:$-1)));
    zy=sparse([1:ny-1;1:ny-1]',1 ./(y(2:$)-y(1:$-1)))*sparse(Z(2:$,:)-Z(1:$-1,:));
    zx(:,$+1)=zx(:,$);
    zy($+1,:)=zy($,:);
    zx=full(zx);
    zy=full(zy);
  endfunction

  function [xu,yu,zu,xv,yv,zv]=parametricDiffData(X,Y,Z)
    [nv,nu]=size(X);
    xu=X(:,2:$)-X(:,1:$-1)+%eps;xu(:,$+1)=xu(:,$);
    yu=Y(:,2:$)-Y(:,1:$-1)+%eps;yu(:,$+1)=yu(:,$);
    zu=Z(:,2:$)-Z(:,1:$-1)+%eps;zu(:,$+1)=zu(:,$);
    xv=X(2:$,:)-X(1:$-1,:)+%eps;
    // XXX buged in nsp xv($+1,:)=xv($,:);
    xv(nv,:)=xv(nv-1,:);
    yv=Y(2:$,:)-Y(1:$-1,:)+%eps;
    yv(nv,:)=yv(nv-1,:);
    zv=Z(2:$,:)-Z(1:$-1,:)+%eps;
    zv(nv,:)=zv(nv-1,:);
  endfunction

  function n=parametricNormals(xu,yu,zu,xv,yv,zv)
    dim=prod(size((xu)));
    n=[yu(:) .*zv(:)-zu(:) .*yv(:),zu(:) .*xv(:)-xu(:) .*zv(:), ...
       xu(:) .*yv(:)-yu(:) .*xv(:)]';
    n=sparse(n)*sparse([1:dim;1:dim]',1 ./sqrt(sum(n .^2,'r')));
    n=full(n);
  endfunction

  function n=nonParametricNormals(zu,zv)
    dim=prod(size(zu));
    n=[-zu(:)';-zv(:)';ones(1,dim)];
    n=sparse(n)*sparse([1:dim;1:dim]',1 ./sqrt(sum(n .^2,'r')));
    n=full(n);
  endfunction

  function C=compute_colors(F,x,y)
    // only works for parametric case 
    [nArgOut,nArgIn]=inout(F);
    C=[];
    select nArgOut
      case 1 then
       ok=execstr('[X1,Y1]=meshgrid(x,y);Cv=F(X1,Y1);',errcatch = %t);
       if ~ok || or(size(Cv) <> [length(Y),length(X)]) then
         C=feval(x,y,F);
       else
         C=Cv;
       end
    end
  endfunction

  fig();
  F=get_current_figure();
  fig(axes_init = '3d');
  A=get_current_objs3d();

  [ok,msg,argList,H]=detect_options(typeOfPlot,argList,argOpt);

  if ~ok then error(msg);return;end

  options=hash(mesh_only = mesh_only);
  if H.iskey['axesflag'] && H.axesflag==0 then options.box_style="none";end
  if H.iskey['rect'] && length(H.rect)==6 then options.ebox=H.rect;end
  // if no special colormap we set a jetcolormap
  if H.iskey['colormap'] then
    options.colormap=H.colormap;
  else
    F=get_current_figure();
    if isempty(F.gc.colormap) then
      options.colormap=jetcolormap(32);
    end
  end
  if H.iskey['shade'] then options.shade=H.shade;end
  if H.iskey['azimuth'] then options.theta=H.azimuth;end
  if H.iskey['elevation'] then options.alpha=90-H.elevation;end
  if H.iskey['lightVect'] then lightVect=H.lightVect;else lightVect=[1;1;1];end
  if H.iskey['mesh'] then options.mesh=H.mesh;end
  if H.iskey['mesh_only'] then options.mesh_only=H.mesh_only;end
  if H.iskey['iso'] then options.iso=H.iso;end
  // Z est a priori une matrice 
  // on peut accepter une fonction 
  // C est a priori une matrice || mxnx3 matrice 
  // where Z is m-by-n.
  select length(argList)
    case 1 then
     // surf(Z)
     Z=argList(1);sZ=size(Z);X=1:s(1);Y=1:s(2);
    case 2 then
     // surf(Z,C)
     Z=argList(1);sZ=size(Z);X=1:s(1);Y=1:s(2);C=argList(2);
    case 3 then
     // surf(X,Y,Z) 
     X=argList(1);Y=argList(2);Z=argList(3);
     // X  specified as a matrix the same size as Z or as a
     //    vector with length n, where [m,n] = size(Z)
     // Y  specified as a matrix the same size as Z or as a
     //    vector with length n, where [m,n] = size(Z)
    case 4 then
     X=argList(1);Y=argList(2);Z=argList(3);C=argList(4);
     if type(C,'short')=='m' then
       options.colors=C
     elseif type(C,'short')=='pl' then
       Cfun = C;
       C=compute_colors(C,X,Y);
       if ~isempty(C) then options.colors=C;end
     else
       error("Error: C should be a function or a matrix\n");return;
     end
    else
      error("Error: wrong number of arguments in %s\n",typeOfPlot);
  end
  // this code is common to surf and surfl 

  function C=light_colors_non_parametric(X,Y,Z)
    nx=length(X);
    ny=length(Y);
    [zx,zy]=nonParametricDiffData(X,Y,Z);
    C=matrix(computeLight(nonParametricNormals(zx,zy),lightVect),ny,nx);
  endfunction

  function C=light_colors_parametric(X,Y,Z)
    [nv,nu]=size(X)
    [xu,yu,zu,xv,yv,zv]=parametricDiffData(X,Y,Z);
    C=matrix(computeLight(parametricNormals(xu,yu,zu,xv,yv,zv),lightVect),nv,nu);
  endfunction

  if type(Z,'short')=='pl' then
    // ---------- function for z 
    [nArgOut,nArgIn]=inout(Z);
    select nArgOut
      case 1 then
	ok=execstr('[X1,Y1]=meshgrid(x,y);Zv=Z(X1,Y1);',errcatch = %t);
	if ~ok || or(size(Zv) <> [length(Y),length(X)]) then
          // we keep Z as a function plot3d1 will do the loop
          // except if light is on 
          if uselight then
            Z=feval(x,y,Z);Z=Z';
            if options.iskey['colors'] then Zc=C,else Zc=Z;end
            options.colors=light_colors_non_parametric(X,Y,Zc);
            // switch to nsp conventions
            Z=Z';
            options.colors=options.colors';
          else
            // switch to nsp conventions 
            if options.iskey['colors'] then options.colors=options.colors';end
          end
	else
          Z=Zv;
          if uselight then
            if options.iskey['colors'] then Zc=C,else Zc=Z;end
            options.colors=light_colors_non_parametric(X,Y,Zc);
            options.colors=options.colors'
          else
            // switch to nsp conventions 
            if options.iskey['colors'] then options.colors=options.colors';end
          end
          // switch to nsp conventions
          Z=Z';
	end
      case 3 then
	// XXX WIP: uselight remains to be done + the case of C 
	[X,Y]=meshgrid(X,Y);
	ok=execstr('[Xv,Yv,Zv]=Z(X,Y);',errcatch = %t);
	if ~ok then
          for i=1:size(X) do
            for j=1:size(Y) do
              [xv,yv,zv]=Z(X(i),Y(j))
              Xv(i,j)=xv;Yv(i,j)=yv;Zv(i,j)=zv;
            end
          end
	end
	[X,Y,Z]=generate3dPolygons(Xv,Yv,Zv,4,%t);
	if exists('Cfun') then
	  ok=execstr('[Cv]=Cfun(X,Y);',errcatch = %t);
	  if ok && size(Cv)==size(X) then
	    options.colors = Cv;
	  else
	    options.delete['colors'];
	  end
	else
	  options.delete['colors'];
	end
    end
  else
    // ---------- matrices 
    // x,y,Z or X,Y,Z with Z a real matrix 
    if and([size(Z)==size(X),size(Z)==size(Y)]) then
      // X,Y,Z or X,Y,Z,C
      if uselight then
        if options.iskey['colors'] then Zc=C,else Zc=Z;end
        options.colors=light_colors_parametric(X,Y,Zc);
      end
      if options.iskey['colors'] then
        // remap colors according to colomap size
        if options.iskey['colormap'] then
          ncol=size(options.colormap,1);
        else
          cmap=xget('colormap');
          ncol=size(cmap,1);
        end
        C=options.colors;
        C=ncol*(C-min(C))/(max(C)-min(C));
        [_X,_Y,C]=generate3dPolygons(X,Y,C,4,%t);
        options.colors=C;
      end
      [X,Y,Z]=generate3dPolygons(X,Y,Z,4,%t);
    else
      // (x,y,Z) or (x,y,Z,C)
      if uselight then
        if options.iskey['colors'] then Zc=C,else Zc=Z;end
        options.colors=light_colors_non_parametric(X,Y,Zc);
      end
      // switch to nsp conventions 
      if options.iskey['colors'] then options.colors=options.colors';end
      // transpositions since nsp and matlab conventions are not the same
      Z=Z';
    end
  end
  plot3d1(X,Y,Z,options(:));
endfunction

function surf(varargin,varargopt)
  if nargin==0 then
    S=["x=-1:0.1:1;y=x;";"function z=f(x,y); z=cos(%pi*x.*y); endfunction;";
       "surf(x,y,f, view=[45 30]);"];
    printf("%s\n",S);
    execstr(S);
  else
    main_surf('surf',varargin,varargopt);
  end
endfunction

function surfl(varargin,varargopt)
  if nargin==0 then
    x=-1:0.1:1;
    y=x;
    function z=f(x,y) z=cos(%pi*x .*y);endfunction
    surfl(x,y,f,view = [45,30]);
  else
    main_surf('surfl',varargin,varargopt,uselight = %t);
  end
endfunction

function mesh(varargin,varargopt)
  if nargin==0 then
    x=-1:0.1:1;
    y=x;
    function z=f(x,y) z=cos(%pi*x .*y);endfunction
    mesh(x,y,f,view = [45,30]);
  else
    main_surf('mesh',varargin,varargopt,mesh_only = %t);
  end
endfunction
