function polarplot(theta,rho)
  if nargin <=0 then
    theta=0:.01:2*%pi;rho=sin(2*theta).*cos(2*theta)
    xclear();polarplot(theta,rho)
    return
  end
  if size(theta,1)==1 then theta=theta(:),end
  if size(rho,1)==1 then rho=rho(:),end
  rm=max(rho)
  x=rho.*cos(theta)
  y=rho.*sin(theta)
  rect=[-rm,-rm,rm,rm];
  plot2d(x,y,rect=rect,strf='030');
  step=rm/5
  r=step;dr=0.02*r;
  for k=1:4
    xset("color",3)
    xarc(-r,r,2*r,2*r,0,360*64)
    xset("color",1)
    xstring((r+dr)*cos(5*%pi/12),(r+dr)*sin(5*%pi/12),sprintf('%5.2f',r));
    r=r+step;
  end
  xset("color",1)
  xarc(-r,r,2*r,2*r,0,360*64)
  xstring((r+dr)*cos(5*%pi/12),(r+dr)*sin(5*%pi/12),sprintf('%5.2f',r));
  rect=xstringl(0,0,'360');w=rect(3);h=rect(4);d=sqrt(w.^2+h.^2)/1.8
  r=rm+d
  for k=0:11
    xset("color",3)
    xsegs([0;rm*cos(k*(%pi/6))],[0;rm*sin(k*(%pi/6))])
    xset("color",1)
    xstring(r*cos(k*(%pi/6))-w/2,r*sin(k*(%pi/6))-h/2,sprintf('%.0f',k*30));
  end
  xset("color",1)
endfunction
