function es_demos()
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

  function rebin3_demo()
  // XXXX à revoir sans hypermat 
    a=hypermat([3,3,3]); a(2,2,2)=-1; x=1:3; a0=-.4;
    [xx,yy,zz]=isosurf3d(x,x,x,a,a0); 
    xbasc(); subwind(1,2,1); 
    shadesurf(xx,yy,zz,.4,1,15,65);
    xn=linspace(1,3,13); yn=linspace(1,3,8); zn=linspace(1,3,7);
    an=rebin3(xn,yn,zn,x,x,x,a);
    [xx,yy,zz]=isosurf3d(xn,yn,zn,an,a0);
    subwind(2); shadesurf(xx,yy,zz,.4,1,15,65); subwind(1,1,1);
  endfunction

  function shaded_demo()
    nc=40; setcmap([2 3 6 15],nc); 
    [xx1,yy1,zz1]=sphere(rand(10,3),0.06); 
    [xx2,yy2,zz2]=sphere(rand(10,3),0.04); 
    s=(0:0.01:1)'; 
    [xx3,yy3,zz3]=spaghetti(cos(4*%pi*s)/4+s,-sin(4*%pi*s)/4,s);
    l=[2 0 3]; c1=shadecomp(xx1,yy1,zz1,l,3);
    c2=shadecomp(xx2,yy2,zz2,l,3);c3=shadecomp(xx3,yy3,zz3,l,3);
    data=list(xx1,yy1,zz1,c1,1,xx2,yy2,zz2,c2,3,xx3,yy3,zz3,c3,2);
    xbasc(); plot3d(data,nc*ones(1,3),theta=45,alpha=80);
  endfunction
  
  function shaded_demo2()
    nc=40; setcmap([2 15],nc);  
    nx=20; ny=20; nz=20; s=hypermat([nx,ny,nz]); 
    x=linspace(-4.5,4.5,nx); xq=x'*ones(1,ny); 
    y=linspace(-4.5,4.5,ny); yq=ones(nx,1)*y; 
    z=linspace(-4.5,4.5,nz); 
    for i=1:nz; s(:,:,i)=1. ./((xq-2).^2+yq.^2+(z(i)-2)^2)+1. ./((xq+2).^2+yq.^2+(z(i)+2)^2); end;
    [xx,yy,zz]=isosurf3d(x,y,z,-s,-.18);  
    l=[2 0 3]; c=shadecomp(xx,yy,zz,l,2); 
    [xx2,yy2,zz2,c2]=slice3d(x,y,z,s,-1,-1,1);  
    data=list(xx,yy,zz,c,1,xx2,yy2,zz2,c2/max(c2),2); 
    xbasc();oplot3d(data,nc*ones(1,2),45,80,'x@y@z',[-1 6 4]);
  endfunction

  function shaded_demo3()
    ns=11; setcmap(16,ns);colors=xget('colormap');
    // nc=tintcmap(colors,23*ns); data=list();
    for i=1:ns 
      [xx,yy,zz]=sphere([cos(i*2*%pi/ns),sin(i*2*%pi/ns),-sin(i*2*%pi/ns)],0.2);
      c=shadecomp(xx,yy,zz,[1,0,2],3,2);
      //data(5*i-4)=xx; data(5*i-3)=yy; data(5*i-2)=zz;
      //data(5*i-1)=c; data(5*i)=i;
      plot3d(xx,yy,zz,theta=70,alpha=45);
    end;
  endfunction
  
  function champ3_demo()
    x=1:4; y=1:3; z=[0 .5 1 2];
    sx=hypermat([4,3,4],rand(48,1));
    sy=hypermat([4,3,4],rand(48,1));
    sz=hypermat([4,3,4],rand(48,1));
    xbasc(); champ3(x,y,z,sx,sy,sz);
  endfunction
  
  demolist=['Colormaps','setcmap()';
	    'Surface plot with lighting','shadesurf()';
	    'Much nicer surface plot with lighting','shadesurf2()';
	    'Colored curve plot','xbasc();cplot()';
	    'Colored dots plot','xbasc();cpoints()';
	    'Histogram of a vector','xbasc();histogram()';
	    'Joint histogram in a bivariate space','xbasc();histogram2()'
	    'Color plot of a matrix and x - y profiles','xbasc();xprofile()';
	    'Rose-of-winds plot of a polar function','xbasc();Mpolarplot()';
	    'Color plot of a matrix in polar form','xbasc();polarmap()';
	    'Rebins a matrix on a new grid using bilinear interpolation','xbasc();rebin()';
	    'Streamlines in 3d space as ribbons','xbasc();ribbon()';
	    'Streamlines in 3d space as tubuli','xbasc();spaghetti()';
	    'Plane cuts of a 3d hypermatrix in 3d space','xbasc();slice3d()';
	    '3D isosurface of constant value','xbasc();isosurf3d()';
	    'Shaded 3d objects of different colors - 1','xbasc();shaded_demo();';
	    'Shaded 3d objects of different colors - 2','xbasc();shaded_demo2();';
	    'Shaded 3d objects of different colors - 3','shaded_demo3();';
	    '3D vector field', 'champ3_demo();'];
  while %t then
    num=x_choose(demolist(:,1),['Click to choose a demo']);
    if num==0 then 
      // lines(oldln(1))
      return
    else
      printf('=====================================\n')
      printf('\n')
      printf('%s\n',demolist(num,1))
      printf('-------------------------------------\n')
      printf('\n')
      printf('%s\n',demolist(num,2))
      execstr(demolist(num,2))
    end,
  end
endfunction

