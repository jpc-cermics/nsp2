function polarplot(theta,rho,color=1,grid_color=3,rect=[])

  function polarplot_old(theta,rho,color=1,grid_color=3,rect=[])
    if size(theta,1)==1 then theta=theta(:),end
    if size(rho,1)==1 then rho=rho(:),end
    c_color=xget('color');
    rm=max(rho)
    x=rho.*cos(theta)
    y=rho.*sin(theta);
    if isempty(rect) then rect=[-rm,-rm,rm,rm];end
    plot2d(x,y,rect=rect,strf='030');
    step=rm/5
    r=step;dr=0.02*r;
    for k=1:4
      xset("color",grid_color)
      xarc(-r,r,2*r,2*r,0,360*64)
      xset("color",color)
      xstring((r+dr)*cos(5*%pi/12),(r+dr)*sin(5*%pi/12),sprintf('%5.2f',r));
      r=r+step;
    end
    xset("color",color)
    xarc(-r,r,2*r,2*r,0,360*64)
    xstring((r+dr)*cos(5*%pi/12),(r+dr)*sin(5*%pi/12),sprintf('%5.2f',r));
    rect=xstringl(0,0,'360');w=rect(3);h=rect(4);d=sqrt(w.^2+h.^2)/1.8
    r=rm+d
    for k=0:11
      xset("color",grid_color)
      xsegs([0;rm*cos(k*(%pi/6))],[0;rm*sin(k*(%pi/6))])
      xset("color",color)
      xstring(r*cos(k*(%pi/6))-w/2,r*sin(k*(%pi/6))-h/2,sprintf('%.0f',k*30));
    end
    xset("color",c_color);
  endfunction

  function polarplot_new(theta,rho,color=1,grid_color=3,rect=[])
  // polarplot
    if size(theta,1)==1 then theta=theta(:),end
    if size(rho,1)==1 then rho=rho(:),end
    rm=max(rho)
    x=rho.*cos(theta)
    y=rho.*sin(theta)
    if isempty(rect) then rect=[-rm,-rm,rm,rm];end
    xsetech(frect=[-rm,-rm,rm,rm],clip=%f,iso=%t,axesflag=0);
    //plot2d(x,y,rect=rect,strf='030');
    step=rm/5
    r=step;dr=0.02*r;
    for k=1:4
      xarc(-r,r,2*r,2*r,0,360*64,color=grid_color);
      xstring((r+dr)*cos(5*%pi/12),(r+dr)*sin(5*%pi/12),sprintf('%5.2f',r));
      r=r+step;
    end
    xarc(-r,r,2*r,2*r,0,360*64,color=1)
    xstring((r+dr)*cos(5*%pi/12),(r+dr)*sin(5*%pi/12),sprintf('%5.2f',r));
    rect=xstringl(0,0,'360');w=rect(3);h=rect(4);d=sqrt(w.^2+h.^2)/1.8
    r=rm+d
    for k=0:11
      xsegs([0;rm*cos(k*(%pi/6))],[0;rm*sin(k*(%pi/6))],style=3)
      xstring(r*cos(k*(%pi/6))-w/2,r*sin(k*(%pi/6))-h/2,sprintf('%.0f',k*30));
    end
    xpoly(x,y,color=color);
  endfunction

  if nargin <=0 then
    theta=0:.01:2*%pi;rho=sin(2*theta).*cos(2*theta)
    xclear();polarplot(theta,rho,color=9);
    return;
  end

  if new_graphics() then
    polarplot_new(theta,rho,color=color,grid_color=grid_color,rect=rect);
  else
    polarplot_old(theta,rho,color=color,grid_color=grid_color,rect=rect);
  end
endfunction
