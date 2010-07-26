function contourf(x,y,z,nv=[],style=[],strf="121",leg="",rect=[0,0,1,1],nax=[1,10,1,10])
  
  if nargin==0,
    s_mat=['t=-%pi:0.1:%pi;m=sin(t)''*cos(t);contourf(t,t,m);'];
    print(s_mat);
    execstr(s_mat);
    return;
  end;
  
  if nargin <= 0 then x=1:10;end 
  if nargin <= 1 then y=1:10;end 
  if nargin <= 2 then z=rand(size(x,'*'),size(y,'*'));end 
  if isempty(nv) then zmin=min(z);zmax=max(z);nv = zmin + (1:10)*(zmax-zmin)./(11);end 
  
  if isempty(x) then x=1:size(z,'r');end 
  if isempty(y) then y=1:size(z,'c');end 
  nvs=size(nv,'*') ;
  if nvs==1 then nvs=nv;zmin=min(z);zmax=max(z);nv = zmin + (1:nvs)*(zmax-zmin)./(nvs+1);end;
  if nargin <= 4 then style = -1*ones_new(1,nvs);end
  if nargin <= 7 then rect=[min(x),min(y),max(x),max(y)]; end 
    
  nv1=nv
  [mz,nz] = size(z);
  minz = min(z);
  maxz = max(z);
  
  if new_graphics() then 
    lp=xget('lastpattern');
    if nvs > lp ; printf('Colormap is too small\n');return ;end 
    min_nv=min(nv);
    max_nv=max(nv);
    // fill the contours 
    grayplot(x,y,z,shade=%t,colminmax=[1,nvs-1],colout=[0,nvs],...
	     zminmax=[min(nv),max(nv)], strf=strf,rect=rect,nax=nax);
    // draw the boundaries 
    contour2d(x,y,z,nv,style=style,strf="000",leg=leg,rect=rect,nax=nax);
    return 
  end
    
  // Surround the matrix by a very low region to get closed contours, and
  // replace any NaN with low numbers as well.
  zz=[ %nan*ones_new(1,nz+2); %nan*ones_new(mz,1),z,%nan*ones_new(mz,1);%nan*ones_new(1,nz+2)];
  kk=find(isnan(zz(:)));
  zz(kk)=minz-1e4*(maxz-minz)+zeros_new(size(kk));
  
  xx = [2*x(1)-x(2); x(:); 2*x(mz)-x(mz-1)];
  yy = [2*y(1)-y(2); y(:); 2*y(nz)-y(nz-1)];
  
  // Internal call to get the contours 
  [x1,y1]=contour2di(xx,yy,zz,nv);
  CS=[x1;y1];
  // Find the indices of the curves in the c matrix, and get the
  // area of closed curves in order to draw patches correctly. 
  ii = 1;
  ncurves = 0;
  I = [];
  Area=[];
  
  while (ii < size(CS,2)),
    nl=CS(2,ii);
    ncurves = ncurves + 1;
    I(ncurves) = ii;
    xp=CS(1,ii+(1:nl));  // First patch
    yp=CS(2,ii+(1:nl));
    dxp=xp(2:$)-xp(1:$-1);
    Area(ncurves)=sum(dxp.*(yp(1:nl-1)+yp(2:nl))./2 );
    ii = ii + nl + 1;
  end

  lp=xget('lastpattern');

  if nv > lp ; write(%io(2),'Colormap too small');return ;end 
	
  min_nv=min(nv);
  max_nv=max(nv);

  plot2d([min(xx);max(xx)],[min(yy);max(yy)],style=0,strf=strf,leg=leg,rect=rect,nax=nax);
    
  // Plot patches in order of decreasing size. This makes sure that
  // all the levels get drawn, not matter if we are going up a hill or
  // down into a hole. When going down we shift levels though, you can
  // tell whether we are going up or down by checking the sign of the
  // area (since curves are oriented so that the high side is always
  // the same side). Lowest curve is largest and encloses higher data
  // always.

  draw_min=%t;
  H=[];
  [FA,IA]=sort(abs(Area));

  pat=xget('pattern');
  for jj=IA,
    nl=CS(2,I(jj));
    lev1=CS(1,I(jj));
    if (lev1 ~= minz | draw_min),
      xp=CS(1,I(jj)+(1:nl));  
      yp=CS(2,I(jj)+(1:nl)); 
      pat=size(find( nv <= lev1),'*');
      xset("pattern",pat);
      xfpoly(xp,yp)
    end;
  end
  xset('pattern',pat);
  if style(1)<>-1 then 
    contour2d(xx,yy,zz,nv,style=style,strf="000",leg=leg,rect=rect,nax=nax);
  end
  

endfunction

