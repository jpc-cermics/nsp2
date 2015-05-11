function graypolarplot(theta,rho,z,rect=[],grid_color=3)
//
  if nargin <= 0 then
    rho=1:0.2:4;theta=(0:0.02:1)*2*%pi;
    z=30+round(theta'*(1+rho.^2));
    xset('colormap',hotcolormap(max(z)));
    graypolarplot(theta,rho,z)
    return
  end

  R=max(rho)
  if isempty(rect) then rect=[-R -R R R]*1.1; end
  xsetech(frect=rect,iso=%t,axesflag=0);
  [rho,k]=sort(rho);z=z(:,k);
  nt=size(theta,'*');theta=matrix(theta,1,-1)*180/%pi
  for k=1:size(rho,'*')-1
    r=rho(k)
    xfarcs([-r*ones(1,nt-1);
	    r*ones(1,nt-1)
	    2*r*ones(1,nt-1)
	    2*r*ones(1,nt-1)
	    theta(1:$-1)*64;
	    (theta(2:$)-theta(1:$-1))*64],(z(1:$-1,k)+z(2:$,k)+z(1:$-1,k+1)+z(2:$,k+1))/4)
  end

  step=R/5;
  r=step;dr=0.02*r;
  for k=1:4
    xarc(-r,r,2*r,2*r,0,360*64,color=grid_color)
    xstring((r+dr)*cos(5*%pi/12),(r+dr)*sin(5*%pi/12),string(round(10*r)/10))
    r=r+step
  end
  xarc(-r,r,2*r,2*r,0,360*64,color=grid_color)
  xstring((r+dr)*cos(5*%pi/12),(r+dr)*sin(5*%pi/12),string(round(10*r)/10))
  rect=xstringl(0,0,'360');w=rect(3);h=rect(4);d=sqrt(w^2+h^2)/1.8
  r=R+d
  for k=0:11
    xsegs([0;R*cos(k*(%pi/6))],[0;R*sin(k*(%pi/6))],style=grid_color);
    xstring(r*cos(k*(%pi/6))-w/2,r*sin(k*(%pi/6))-h/2,string(k*30));
  end
endfunction
