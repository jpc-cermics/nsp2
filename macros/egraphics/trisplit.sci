function [xt,yt,zt,ct]=trisplit(xx,yy,zz,cc)
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
  // Adapted from Enrico Segre scicoslab toolbox.
  // Copyright (C) 1998-2017 - Enrico Segre

  // fonction utile pour passer de rectangles à triangles 
  if %f then 
    x=linspace(-2.5,2.5,10);
    z=exp(-(x'*x).^2);
    [xx,yy,zz]=genfac3d(x,x,z);
    [xt,yt,zt]=trisplit(xx,yy,zz);
    plot3d1(xt,yt,zt);
    // plot3d1(xt,yt,-zt);
  end
    
//list input form, a la oplot3d
  if nargin==1 then
    cc=xx(4); zz=xx(3); yy=xx(2), xx=xx(1);
    nargin=4;
  end

  if size(xx)~=size(yy) | size(xx)~=size(zz) | size(yy)~=size(zz) then
    printf("%s\n",'arguments have wrong sizes!')
    return
  end

  nf=size(xx,2); ns=size(xx,1);

  //if ns==3 then return; end    //nothing to do!

  if nargin==4 then
    if (size(cc)~=[1,nf] & size(cc)~=[ns,nf])
      printf("%s\n",'cc has wrong sizes!')
      return
    end
  end

  //find suitable internal centroids of the given polygons
  // try first middle points (would be ok for plane convex polygons)
  xc=mean(xx,1);
  yc=mean(yy,1);
  zc=mean(zz,1);

  // check if they fall inside the polygon (3d?)

  //if cc is given and vertex-bound:
  //compute an interpolated value of cc at the centroid
  if nargin==4 then if size(cc,1)==ns then
      ccc=mean(cc,1)
  end; end

  //compose triangles
  xt=zeros(3,ns*nf); yt=xt; zt=xt;
  for i=1:ns-1
    xt(:,(0:nf-1)*ns+i)=[xx(i,:);xx(i+1,:);xc];
    yt(:,(0:nf-1)*ns+i)=[yy(i,:);yy(i+1,:);yc];
    zt(:,(0:nf-1)*ns+i)=[zz(i,:);zz(i+1,:);zc];
  end
  xt(:,(1:nf)*ns)=[xx(ns,:);xx(1,:);xc];
  yt(:,(1:nf)*ns)=[yy(ns,:);yy(1,:);yc];
  zt(:,(1:nf)*ns)=[zz(ns,:);zz(1,:);zc];
  zt(:,(1:nf)*ns)=[zz(ns,:);zz(1,:);zc];

  if (nargout==4 | nargout==1) & nargin==4 then 
    if size(cc,1)==1 then
      ct=zeros(1,ns*nf);
      for i=1:ns
	ct((0:nf-1)*ns+i)=cc;
      end
    end
    if size(cc,1)==ns then
      ct=xt;
      for i=1:ns-1
	ct(:,(0:nf-1)*ns+i)=[cc(i,:);cc(i+1,:);ccc]; 
      end
      ct(:,(1:nf)*ns)=[cc(ns,:);cc(1,:);ccc];
    end
  end

  //elimination of null triangles (typical occurrences: from the
  // splitting of quadrangles with two coincident points, e.g. poles
  // of spheres, tips of arrows)
  q=find(~(... 
	   (abs(xt(1,:)-xt(2,:))<%eps & abs(yt(1,:)-yt(2,:))<%eps...
	    & abs(zt(1,:)-zt(2,:))<%eps) | ...
	   (abs(xt(1,:)-xt(3,:))<%eps & abs(yt(1,:)-yt(3,:))<%eps...
	    & abs(zt(1,:)-zt(3,:))<%eps) | ...
	   (abs(xt(3,:)-xt(2,:))<%eps & abs(yt(3,:)-yt(2,:))<%eps...
	    & abs(zt(3,:)-zt(2,:))<%eps) ) ...
	 );
  xt=xt(:,q); yt=yt(:,q); zt=zt(:,q);
  if exists('ct','local') then ct=ct(:,q); end

  //list output form for oplot3d
  if nargout==1 then
    if ~exists('ct','local') then ct=ones(1,ns*nf); end
    xt=list(xt,yt,zt,ct,1)
  end

endfunction
