// following a parametric 3d curve 
// with a tube 
// jpc 2004 
//---------------------------------

t=4*%pi*(0:20)/20;
ptc=[t.*sin(t);t.*cos(t);0*ones(t)];

for i=1:(size(ptc,'c')-1)
  pt=ptc(:,i);
  ptn=ptc(:,i+1);
  u= ptn-pt;
  u= u / sqrt(sum(u.*u))
  // trouver un vecteur ds le plan orthogonal
  I=find(u==0.0);
  if I<>[] then 
    v=0*u;v(I(1))=1;
  else 
    v=[u(2);-u(1);0];
    v= v / sqrt(sum(v.*v))
  end
  w=[u(2)*v(3)-u(3)*v(2);
     -(u(1)*v(3)-u(3)*v(1))
     u(1)*v(2)-u(2)*v(1)];

  n=10;
  alpha=2*%pi*(0:n)/n;
  r=t(i);

  pts=r*v*cos(alpha)+r*w*sin(alpha);
  if i==1 then 
    ptg=pts+pt*ones(alpha);
  else
    ptg=ptd;
  end
  ptd=pts+ptn*ones(alpha);

  xpol=[ptg(1,1:$-1);ptd(1,1:$-1);ptd(1,2:$);ptg(1,2:$)];
  ypol=[ptg(2,1:$-1);ptd(2,1:$-1);ptd(2,2:$);ptg(2,2:$)];
  zpol=[ptg(3,1:$-1);ptd(3,1:$-1);ptd(3,2:$);ptg(3,2:$)];
  plot3d1(xpol,ypol,zpol);
end 



  
