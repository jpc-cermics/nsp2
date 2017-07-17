function polarmap(a,amin,amax,l)
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

  // derived from pixmapl() - similar calling procedure

  if nargin==3 then l=0; end
  if nargin==2 then l=amin;amin=min(a);amax=max(a); end
  if nargin==1 then l=0;amin=min(a);amax=max(a); end

  if nargin==0 then
    // demo
    printf("%s\n",'demo of polarmap\n')
    a=rebin(linspace(0,5,100),linspace(0,5,60),...
	    1:20,1:20,rand(20,20))
    polarmap(a,2)
    return
  end

  // this is for working with 2d cuts of hypermatrices
  idim=find(size(a)<>1);
  if length(idim)==0 then adim=[1,1]; end
  if length(idim)==1 then
    if idim==1 then adim=[size(a,idim),1]; end
    if idim>1 then adim=[1,size(a,idim)]; end
  end;
  if length(idim)==2 then adim=[size(a,idim(1)),size(a,idim(2))]; end
  if length(idim)>2 then
    printf("%s\n",' I can only plot 2d arrays, data is a '+...
	  string(length(idim))+'d hypermatrix')
    return
  end
  a=matrix(a,adim(1),adim(2));

  //if ~isreal(a) then
  if imag(a)<>0 then 
    printf("%s\n",' Data must be real, I''ll take the its real part')
    a=real(a)
  end

  if amin>=amax then amin=min(a);amax=max(a); end
  a(find(a<amin))=amin;a(find(a>amax))=amax;

  ax=size(a,1);ay=size(a,2);
  if ax==0 | ay==0 then return; end

  // colorbar part 
  f=xgetech();
  nc=xget("lastpattern");
  if modulo(l+3,4)<=1 then
    xsetech([f(1)+0.95*f(3),f(2),0.04*f(3),f(4)]);
    grayplot([0,1],amin+(amax-amin)*(0:nc)/nc,[(0:nc);(0:nc)],strf='011',...
	     rect=[0,amin,1,amax],nax=[0,0,10,10])
    // the graduation of the axes of the legend is still ugly;
    //  someday I would be better to implement it at low level
    // xclea(-0.35,amin-0.01*(amax-amin),1.6,0.15*(amax-amin))
    // that's a little of a hack to plot a colorbar, I know
    // xsetech([f(1),f(2),0.94*f(3),f(4)]);
    xsetech(f);
  end
  // 
  plot2d([-ay,ay],[-ay,ay],style=0,strf='040')
  // this is derived from the code of pixmapl, but transposed:
  //pixel map:
  rett=matrix([(1:ay)'*ones(1,ax)-0.5,ones(ay,1)*(1:ax),...
	       [ones(1,ax);ones(ay-2,ax);ones(min(1,ay-1),ax)],...
	       [ones(ay,1),ones(ay,ax-2),ones(ay,min(1,ax-1))]],ax*ay,4)';
  ffill=round(matrix(1+(nc-1)*(a'-amin)/(amax-amin),1,ax*ay));
  //reduction  of the number of rectangles
  ired=find([%t,~((ffill(2:$)==ffill(1:($-1))) ...
		  & rett(2,2:$)==rett(2,1:($-1))) ]);
  nred=length(ired);
  ffill=ffill(ired);
  rett(3,ired(1:($-1)))=rett(3,ired(1:($-1)))+...
      ired(2:$)-ired(1:($-1))-1;
  rett(3,ired($))=ay-rett(1,ired($))+0.5;
  rett=rett(:,ired);
  //printf(string(ax*ay)+' rects;'+string(nred)+' uniq')
  //  xset("clipgrf");  // this is necessary for pretty zooming
  //  it's undocumented in help xset... (2.4.1)
  rpols=(rett([1,1,1,1],:)-0.5+...
	 [zeros(1,nred);rett([3,3],:);zeros(1,nred)])
  ppols=(rett([2,2,2,2],:)-[zeros(2,nred);rett([4,4],:)])*2*%pi/ax
  clear rett

  xfpolys(rpols.*cos(ppols),rpols.*sin(ppols),-ffill)

  // xfpolys is still imperfect in one respect: colinear adjacent sides
  //  of different length may not overlap neatly
  //  xset("clipoff")
  // grid & legends (sketchy so far)

  xset('pattern',nc+1)
  if l>1 then
    xsegs([-ay,0;ay,0],[0,-ay;0,ay])
    [xi,m,np]=graduate(0,ay);
    arc=zeros(6,np);arc(1,:)=-(m/np:m/np:m); arc(2,:)=-arc(1,:);
    arc(3,:)=-2*arc(1,:);arc(4,:)=arc(3,:); arc(6,:)=64*360*ones(1,np);
    xarcs(arc,(nc+1)*ones(1,np))
    for i=1:np;
      xstring(i*m/(sqrt(2)*np),i*m/(sqrt(2)*np),string(i*m/np));
    end
    xstring(ay*1.1,0,'E');xstring(-1.1*ay,0,'O');
    xstring(0,1.05*ay,'N');xstring(0,-1.15*ay,'S');
  else
    xarcs([-ay;ay;2*ay;2*ay;0;64*360],nc+1)
  end
  //restore plotting area if it was split by colorbar+map
  if modulo(l+3,4)<=1 then xsetech(f); end
endfunction
