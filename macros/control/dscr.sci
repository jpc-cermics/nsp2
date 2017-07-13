function [f,r]=dscr(a,dt,m)
  // Copyright INRIA
  lst=0
  if type(dt,'short') <> 'm' then
    error('Error: sampling period must be a positive real scalar')
  end
  if size(dt,'*') <> 1 || ~isreal(dt) || dt <= 0 then
    error('Error: sampling period must be a positive  real scalar')
  end
  select type(a,'short')
    case 'r' then
     a=tf2ss(a);
     //[a,b,c,d,x0,dom]=a(2:7);
    case 'linearsys' then
     //[a,b,c,d,x0,dom]=a(2:7)
    else
      error("Error: fisrt argument cannot be used as linear system");
  end
  if a.dom <> 'c' then
    printf('dscr: time domain assumed to be continuous\n'),
  end
  [n1,m1]=size(a.B),
  s=expm([a.A,a.B;0*ones(m1,n1+m1)]*dt),
  f=s(1:n1,1:n1);g=s(1:n1,n1+1:n1+m1);
  if nargin==3 then
    s=expm([-a.A,m;0*a.A,a.A']*dt),
    r=f*s(1:n1,n1+1:n1+n1),
  end
  f=linear_system(f,g,a.C,a.D,a.X0,dom = 's',sample = dt);
endfunction
