function cplot(x,y,varargopt)
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

  // plot of a colored curve
  // just an expanded form of a suggestion to the newsgroup...
  // options: c,cext,strf,leg,rect,nax

  if nargin==0 then
    demo=['  x=(0:0.1:2*%pi)''; y=sin(x); cplot(x,y,c=y)']
    printf("%s\n",demo)
    execstr(demo);
    return
  end

  // strf,leg,rect,nax,cext)
  
  if ~varargopt.iskey['c'] then 
    c=y;y=x;x=(1:length(y))';
  else 
    c= varargopt.c;
    varargopt.delete['c'];
  end 
  if ~varargopt.iskey['cext'] then 
    cext=[min(c),max(c)];  
  else
    cext=varargopt.cext;
    varargopt.delete['cext'];
  end

  if varargopt.iskey['strf'] &&part(varargopt.strf,2)=='0' then 
    [frect,frect1]=xgetech(); 
    xmn=frect(1);
    xmx=frect1(1)+frect1(3);
    ymn=frect(2); ymx=frect1(2)+frect1(4);
  else 
    xmx=max(x);xmn=min(x);ymx=max(y);ymn=min(y); 
  end; 
  
  varargopt.rect=[xmn,ymn,xmx,ymx]; 
    
  nc=xget("lastpattern");
  //plot the frame (I could rather have used plotframe)
  plot2d(x(1),y(1),varargopt(:));
  //plot the curves with current colormap
  xset("clipgrf");
  for j=1:size(x,2)
    X=[x(1:$-1,j)';x(2:$,j)'];
    Y=[y(1:$-1,j)';y(2:$,j)'];
    if cext(2)<>cext(1) then
      C=nc*(c(1:$-1,j)-cext(1))/(cext(2)-cext(1));
    else
      C=ones(max(size(c,1)-1,1),1);
    end
    C(find(isnan(C)))=nc+1;
    xsegs(X,Y,style=C)
  end
  xset("clipoff");
endfunction


