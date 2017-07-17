function cpoints(x,y,a,varargopt)
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

  // plots colored dots of size d at the coordinates x,y
  // x,y,a row or column vectors, all of the same length
  // if d<0 squares are plot instead of dots (some twice as fast)
  // minimal syntax:  cpoints(x,y,a,[d])
  // the other argumens are just passed to plot2d
  // if a= constant * ones(x) then black dots are plotted (color nc+1)
  // varargopt: d,strf,leg,rect,nax;
  if nargin==0 then
    demo=['x=(0:0.1:2*%pi)''';'y=sin(x)';'cpoints(x,y,y,d=4)';
	  'y=sin(x+.2);'; 'cpoints(x,y,-y,d=-3)' ]
    printf("%s\n",demo)
    execstr(demo)
    return
  end

  if ~varargopt.iskey['d'] then 
    d=1; 
  else
    d=varargopt.d;
    varargopt.delete['d'];
  end

  xmx=max(x);xmn=min(x);ymx=max(y);ymn=min(y); 
  varargopt.rect=[xmn,ymn,xmx,ymx]; 
  
  dx=d*(xmx-xmn)/200; dy=d*(ymx-ymn)/200; lx=length(x);
  
  if varargopt.iskey['iso']&& varargopt.iso then 
    dy = dx;
  end
    
  xx=matrix(x,lx,1)-dx/2; yy=matrix(y,lx,1)+dy/2;

  amin=min(a); amax=max(a);
  nc=xget("lastpattern");
  if amin<amax then
    cz=matrix(1+(nc-1)*(a-amin)/(amax-amin),length(x),1);
  else
    cz=(nc+1)*ones(length(x),1)
  end

  plot2d([],[],varargopt(:));
  // this plot two of the points as tiny dots, better than anything
  //  more intrusive; does the job of setting the plot window
  // (leg is not of much sense, it comes out only the legend of a 
  //  single tiny dot, but that's here for compatibility

  xset('clipgrf')
  
  if d<0 then
    xrects([xx'+dx*ones(1,lx);yy'-dy*ones(1,lx);...
	    -dx*ones(1,lx); -dy*ones(1,lx)],cz);
  end

  if d>0 then 
    xfarcs([xx';yy';dx*ones(1,lx);dy*ones(1,lx);zeros(1,lx);...
	    64*360*ones(1,lx)],cz);
  end
  xset('clipoff')
endfunction

  
