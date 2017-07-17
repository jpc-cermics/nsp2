function [xx,yy,zz]=shrinkfaces(varargin)
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

  // TO DO: multiple 5-tuple arguments (here it's only single 5ple)
  
  select length(varargin)
   case 0
    // demo
    [xx,yy,zz]=sphere();
    [xx,yy,zz]=shrinkfaces(xx,yy,zz,0.4);
    plot3d1(xx,yy,zz); xtitle("shrinked unity sphere")
    xx=[];yy=[];zz=[];
    return
   case 1
    if type(varargin(1)<>15) then 
      printf("%s\n",'waiting for a 5-tuple of facelets here!\n')
      return
    end
    sf=0.6;
    xx=varargin(1)(1);  yy=varargin(1)(2); zz=varargin(1)(3);
    c=varargin(1)(4);  i=varargin(1)(5); 
   case 2
    if type(varargin(1)<>15) then 
      printf("%s\n",'waiting for a 5-tuple of facelets here!\n')
      return
    end
    xx=varargin(1)(1);  yy=varargin(1)(2); zz=varargin(1)(3);
    c=varargin(1)(4);  i=varargin(1)(5); 
    sf=varargin(2)
   case 3
    xx=varargin(1);  yy=varargin(2); zz=varargin(3);
    c=ones(1,size(xx,2)); i=1;
    sf=0.6
   case 4
    xx=varargin(1);  yy=varargin(2); zz=varargin(3);
    c=ones(1,size(xx,2)); i=1;
    sf=varargin(4)
  else
    printf("%s\n","wrong number of input arguments!\n"); return
  end
  if size(xx)<>size(yy) | size(xx)<>size(zz) | size(yy)<>size(zz) then
    printf("%s\n","inconsistent facelets!\n"); return
  end

  nf=size(xx,1); u=ones(1,nf);
  xc=mean(xx,1); yc=mean(yy,1); zc=mean(zz,1);
  xx=xc(u,:)+(xx-xc(u,:))*sf; 
  yy=yc(u,:)+(yy-yc(u,:))*sf; 
  zz=zc(u,:)+(zz-zc(u,:))*sf; 

  if nargout==1
    xx=list(xx,yy,zz,c,i)
  end
endfunction
