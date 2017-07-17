function [xx,yy,zz]=ribbon(x,y,z,w)
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
  
  if nargin==0 then
    printf("%s\n",'demo of ribbon(x,y,z,w)\n');
    comm=['h=(-25:25)''/20; s=(tanh(h.^2/2)+1)/2; alpha=(-%pi:0.8:%pi);';
	  'nl=length(s); na=length(alpha);';
	  'x=zeros(nl,na); y=x; z=x; w=zeros(nl,3*na);';
	  'for i=1:na;';
	  '   phi=alpha(i);';
	  '   x(:,i)=s*cos(phi); y(:,i)=h; z(:,i)=s*sin(phi);';
	  '   w(:,3*(i-1)+(1:3))=[-ones(nl,1)*sin(phi),zeros(nl,1),ones(nl,1)*cos(phi)];';
	  'end';
	  '[xx,yy,zz]=ribbon(x,y,z,w*0.08); plot3d(xx,yy,zz,back_color=-2);'];
    printf("%s\n",comm)
    execstr(comm)
    xtitle('demo for ribbon()')
    return
  end

  xx=[];yy=[];zz=[];
  
  if nargin < 3 then 
    printf("%s\n",'ribbon() wants one or more trajectories x,y,z !\n')
    return
  end

  if nargin==3 then w=(max(y)-min(y))/20; end

  if length(x)~=length(y) | length(z)~=length(y) | length(x)~=length(z) then
    printf("%s\n",'inconsistent in x,y,z lengths\n')
    return
  end

  np=size(x,1); nv=size(x,2);
  if np==1 then 
    printf("%s\n",'cannot make ribbons of single points!\n')
    return
  end

  if length(w)<3 then w=w*[0 1 0]; end
  if size(w,1)<>np | size(w,2)<>3*nv then 
    //   w=[w(1)*ones(nv,np);w(2)*ones(nv,np);w(3)*ones(nv,np)]';
    ww=ones(np,1)*matrix(w(1:3),1,3); 
    w=ww(:,modulo(0:(3*nv-1),3)+1);
  end

  if np==2 then
    // "arrows3d" mode: I invent a middle point, so the following
    //  assignments work
    x=[x(1,:); (x(1,:)+x(2,:))/2; x(2,:)];
    y=[y(1,:); (y(1,:)+y(2,:))/2; y(2,:)];
    z=[z(1,:); (z(1,:)+z(2,:))/2; z(2,:)];
    np=3
  end

  xx=zeros(4,(np-1)*nv); yy=xx; zz=yy;

  for i=1:nv
    xx(:,(i-1)*(np-1)+(1:(np-2)))=[x(1:(np-2),i)-w(1:(np-2),3*(i-1)+1)/2,...
				   x(1:(np-2),i)+w(1:(np-2),3*(i-1)+1)/2,...
				   x(2:(np-1),i)+w(2:(np-1),3*(i-1)+1)/2,...
				   x(2:(np-1),i)-w(2:(np-1),3*(i-1)+1)/2]'
    xx(:,(i-1)*(np-1)+np-1)=[x(np-1,i)-3*w(np-1,3*(i-1)+1)/2,...
			     x(np-1,i)+3*w(np-1,3*(i-1)+1)/2,...
			     x(np,i),x(np,i)]'
    yy(:,(i-1)*(np-1)+(1:(np-2)))=[y(1:(np-2),i)-w(1:(np-2),3*(i-1)+2)/2,...
				   y(1:(np-2),i)+w(1:(np-2),3*(i-1)+2)/2,...
				   y(2:(np-1),i)+w(2:(np-1),3*(i-1)+2)/2,...
				   y(2:(np-1),i)-w(2:(np-1),3*(i-1)+2)/2]'
    yy(:,(i-1)*(np-1)+np-1)=[y(np-1,i)-3*w(np-1,3*(i-1)+2)/2,...
			     y(np-1,i)+3*w(np-1,3*(i-1)+2)/2,...
			     y(np,i),y(np,i)]'
    zz(:,(i-1)*(np-1)+(1:(np-2)))=[z(1:(np-2),i)-w(1:(np-2),3*(i-1)+3)/2,...
				   z(1:(np-2),i)+w(1:(np-2),3*(i-1)+3)/2,...
				   z(2:(np-1),i)+w(2:(np-1),3*(i-1)+3)/2,...
				   z(2:(np-1),i)-w(2:(np-1),3*(i-1)+3)/2]'
    zz(:,(i-1)*(np-1)+np-1)=[z(np-1,i)-3*w(np-1,3*(i-1)+3)/2,...
			     z(np-1,i)+3*w(np-1,3*(i-1)+3)/2,...
			     z(np,i),z(np,i)]'
  end
endfunction
