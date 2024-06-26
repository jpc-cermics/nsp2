function hist3d(f,T,A,leg,flags,ebox)
// Copyright INRIA
  nep=8
  def=list(35,45,'X@Y@Z',[2, 1, 4])
  if nargin <=0 then  //demo
    s_mat=['hist3d(10*rand(10,10));']
    write(%io(2),s_mat);execstr(s_mat);
    return;
  end
  if type(f,'short')=='l' then 
    [f,x,y]=f(1:3);
    sx=prod(size(x));
    sy=prod(size(y));
    if [sx-1,sy-1]<>size(f) then 
      write(%io(2),'f: Incompatible length ');
      return;
    end
    dx=(x(2)-x(1))/nep;
    dy=(y(2)-y(1))/nep;
    bnds=[x(1), x($), y(1), y($),min(0,min(f)), max(f)]
    x=(x(1:sx-1)+x(2:sx))/2;
    y=(y(1:sy-1)+y(2:sy))/2;
    [nl,nc]=size(f);
  else
    [nl,nc]=size(f);
    x=(1:nl)-(0.5)*ones_new(1,nl);
    y=(1:nc)-(0.5)*ones_new(1,nc);
    dx=1/nep; dy=1/nep;
    bnds=[0, nl,0, nc,min(0,min(f)), max(f)]
  end
  x=x.*.[1,1] + dx*ones_new(size(x)).*.[0,1] - dx*ones_new(size(x)).*.[1,0];
  y=y.*.[1,1] + dy*ones_new(size(y)).*.[0,1] - dy*ones_new(size(y)).*.[1,0];
  a=[0;0;1;1]
  b=[0;1;1;0]
  c=[0;0;0;0]
  d=[1;1;1;1]
  ix=[b,b,a,a,c,d];
  iy=[a,a,c,d,b,b];
  indx=ones_new(1,nc) .*. (ones_new(1,nl).*.ix +(1:2:2*nl-1).*.ones_new(size(ix)));
  iy=matrix(iy,24,1);
  //indy=(ones_new(1,nl).*.iy+(1:2:2*nl-1).*.ones_new(size(iy))) .*. ones_new(1,nc);
  indy=(ones_new(1,nc).*.iy+(1:2:2*nc-1).*.ones_new(size(iy))) .*. ones_new(1,nl);
  indy=matrix(indy,4,6*nc*nl);
  [nnl,nnc]=size(indx);

  xx=matrix(x(matrix(indx,1,nnl*nnc)),nnl,nnc);
  yy=matrix(y(matrix(indy,1,nnl*nnc)),nnl,nnc);
  zz=matrix(f,1,nl*nc).*.[c,d,b,b,a,a];

  select nargin
   case 1 then plot3d(xx,yy,zz,def(1),def(2),def(3),def(4),bnds) 
   case 2 then plot3d(xx,yy,zz,T,def(2),def(3),def(4),bnds) 
   case 3 then plot3d(xx,yy,zz,T,A,def(3),def(4),bnds) 
   case 4 then plot3d(xx,yy,zz,T,A,leg,def(4),bnds) 
   case 5 then plot3d(xx,yy,zz,T,A,leg,flags,bnds) 
   case 6 then plot3d(xx,yy,zz,T,A,leg,flags,ebox) 
  end
endfunction
