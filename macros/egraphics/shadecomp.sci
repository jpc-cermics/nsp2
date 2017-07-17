function c=shadecomp(xx,yy,zz,l,shine,m)
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

  // Computes facelet colors for subsequent rendering with
  // plot3d. Having the access to the computed colors allows
  // further transformations, and assignation of faces to different
  // segments of stacked colormaps.
  //
  //  c(1,size(xx,2)) for normal plot3d(xx,yy,list(zz,c)) rendering,
  //  while if m=2 (gouraud) c has the same size of xx,yy,zz

  if nargin==0 then
    return
  end
  if ~exists('l','local') then l=[1,1,1]; end
  if ~exists('shine','local') then shine=1; end
  if ~exists('m','local') then m=1; end

  ns=size(xx,1); nf=size(xx,2)
  i1=1:ns; i2=[2:ns,1]; i3=[3:ns,1,2];

  nn1=-((yy(i1,:)-yy(i2,:)).*(zz(i2,:)-zz(i3,:))-...
	(yy(i2,:)-yy(i3,:)).*(zz(i1,:)-zz(i2,:)));
  nn2=((xx(i1,:)-xx(i2,:)).*(zz(i2,:)-zz(i3,:))-...
       (xx(i2,:)-xx(i3,:)).*(zz(i1,:)-zz(i2,:)));
  nn3=-((xx(i1,:)-xx(i2,:)).*(yy(i2,:)-yy(i3,:))-...
        (xx(i2,:)-xx(i3,:)).*(yy(i1,:)-yy(i2,:)));
  
  // nn1,nn2,nn3 are 3 matrices of size (ns,nf), which store the
  //  3 components of the normal at each vertex.
  // for triangular (ns=3) faces this calculation is redundant,
  // since triangles are always planar and the three vertex
  // normals will be identical
  
  [degf1,degf2]=find(abs(nn1)+abs(nn2)+abs(nn3)<10*%eps);
  // this can happen for instance if there are coincident vertices;
  // it shouldn't, if xx, yy, zz are constructed properly, but
  // I want shadesurf to get through it anyway
  // the following is capable to handle two consecutive coincident
  // vertices. Three or more consecutive coincident vertices are
  // partially handled, and a division by zero will result below.
  if ~isempty(degf1) then
    q1=degf1+(degf2-1)*ns; q2=modulo(degf1+1,ns)+1+(degf2-1)*ns
    nn1(q1)=nn1(q2); nn2(q1)=nn2(q2); nn3(q1)=nn3(q2)
    // I assume that in this case at least the second next vertex of
    // the face is not degenerate (if two vertices coincide, then
    // two consecutive normals are null)
  end

  //nn3(degf1,degf2)=1;  // this is bad but works as last resort
  nn=sqrt(nn1.^2+nn2.^2+nn3.^2); 
  nn(nn==0)=1; 
  // this is a last resort for null faces, with more than
  // two coinciding vertices, or faces degenerate to a
  // segment (e.g., triangles with a zero side)
  
  //cc=sqrt(nn1.^2+nn2.^2+nn3.^2);
  // color proportional to the slope

  //cc=1../max(a*nn1+b*nn2,1);
  // color inversely proportional to the lightened side

  c=(l(1)*nn1+l(2)*nn2+l(3)*nn3)./nn./sqrt(l*l');
  c=((c+1)/2).^shine;
  // color proportional to the cosine of the angle (local normal)^(sun)
  // the direction of the sun is [l(1), l(2), l(3)]

  clear('nn','nn1','nn2','nn3');
  
  if abs(m)==1 then
    c=mean(c,'r')
    // flat shading -> only one c per face is output, the average of
    // all the c values
  end

  if abs(m)==2
    // if facelets are triangular, they are obviously planar
    // I construct a vertex c and try to go gouraud
    //   if ns==4 then
    //// recognize if the vertices are laid on a structured grid:
    ////  try if there is a subset of faces set side to side. 
    //       stru1=find(xx(4,1:(nf-1))==xx(1,2:nf) & ..
    //            yy(4,1:(nf-1))==yy(1,2:nf) & zz(4,1:(nf-1))==zz(1,2:nf) )
    //       stru2=find(xx(2,1:(nf-1))==xx(1,2:nf) & ..
    //            yy(2,1:(nf-1))==yy(1,2:nf) & zz(2,1:(nf-1))==zz(1,2:nf) )
    //   end
    //   if %f then
    //     disp('structured!')
    //   end
    // lex_sort seems the best solution to look for adjacencies
    //  arrays are silently flattened here
    //[xyz,k]=lex_sort([xx(:),yy(:),zz(:)]);
    [xyz,k]=sort([xx(:),yy(:),zz(:)],'lr','i');
    clear xx yy zz
    q=[0,find( or(xyz(1:$-1,:) ~= xyz(2:$,:),dim='c') ), ns*nf];
    for i=1:length(q)-1
      qq=k(q(i)+1:(q(i+1)))
      //c(qq)=mean(c(qq))
      c(qq)=sum(c(qq))/size(c(qq),'*');
    end
  end

  // range expansion
  if m<0 then
    if max(c)-min(c)>2*%eps then
      c=(c-min(c))/(max(c)-min(c))
    else
      // this can happen if the surface is a plane, and I want this to
      //  be handled
      c=ones(size(c,1),size(c,2));
    end
  end
endfunction


