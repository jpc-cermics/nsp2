function [y,x]=csim(u,dt,sl,x0,tol)
  //Syntax:
  //  [y [,x]]=csim(u,dt,sl,[x0]) 
  // simulation of the controlled linear system sl.
  // sl is assumed to be a continuous-time system.
  // u is the control and x0 the initial state.
  //
  // u can be:
  // - a function 
  //    [inputs]=u(t)
  // - a list
  //    list(ut,parameter1,....,parametern) such that
  //    inputs=ut(t,parameter1,....,parametern)
  // - the character string 'impuls' for impulse response calculation
  //    (here sl is assumed SISO without direct feedthrough and x0=0)
  // - the character string 'step' for step response calculation 
  //    (here sl is assumed SISO without direct feedthrough and x0=0)
  // dt is a vector of instants with dt(1) = initial time
  //                   that is:           x0=x
  //                                          dt(1)
  //
  //y matrix such that:
  //  y=[y       y  ...  y     ]
  //      dt(1)   dt(2)   dt(n)
  //x matrix such that:
  //  x=[x       x  ...  x     ]
  //      dt(1)   dt(2)   dt(n)
  //
  //See also:
  // dsimul flts ltitr rtitr ode impl
  //!
  //
  if nargin < 3 then error("Error: expecting at least three arguments");return;end
  if and(type(sl,'short') <> ['linearsys','r']) then
    error("csim: Invalid third parameter (should be rational or linear system)")
  end
  if type(sl,'short')=='r' then sl=tf2ss(sl),end
  if sl.dom <> 'c' then
    printf("Warning: assuming that linear system is continuous\n");
  end
  //
  [a,b,c,d]=abcd(sl);
  if type(d,'short')=='p' && d.degree[] > 0 then
    d=coeff(d,0);
    printf("Warning: direct feedthrough D set to its zero degree coefficient.\n");
  end
  [maxx,mb]=size(d);
  [ma,mbxx]=size(a);
  //
  imp=0;step=0
  //
  select type(u,'short')
    case 's' then
     // u is a string 
     if mb <> 1 then error("Error: d has incompatible size");return;end
     if part(u,1)=='i' then
       //impuse response
       imp=1;
       if norm(d,1) <> 0 then
         printf("Warning: Direct feedthrough set to zero.\n");
         d=0*d;
       end
     elseif part(u,1)=='s' then
       step=1
       //      if norm(d,1)<>0 then
       //	printf("Warning: msprintf(("%s: Direct feedthrough set to zero.\n"),"csim"));
       //	d=0*d;
       //      end
     else
       error("Error: first argument when string must be ""step"" or ""impuls""");
     end
     function y=u(t) if t==0 then y=0,else y=1,end;endfunction;
    case 'pl' then
     %t
     //input given by a function of time
    case 'm' then
     //input given by a vector of data
     [mbu,ntu]=size(u);
     if mbu <> mb | ntu <> size(dt,'*') then
       error("Error: first and second argument should have same number of columns");
       return;
     end
    case 'l' then
     //input given by a list: function of time with
     //parameters
     %t
    else
      error("Error: first argument has wrong type");return;
  end
  //
  if nargin==3 then x0=sl(6),end
  if imp==1 | step==1 then x0=0*x0,end
  nt=size(dt,'*');x=0*ones(ma,nt)

  [a,v]=balanc(a);
  v1=v;//save for backward transformation

  //apply transformation u without matrix inversion
  [k,l]=find(v <> 0);//get the permutation

  //apply right transformation 
  v=v(k,l);//diagonal matrix
  c=c(:,k)*v;
  //apply left transformation 
  v=diag(1 ./diag(v));b=v*b(k,:);x0=v*x0(k)

  [a,v2,bs]=bdiag(a,1);b=v2\b;c=c*v2;x0=v2\x0;
  //form the differential equation function
  if type(u,'short')=='m' then
    //form a continuuous time interpolation of the given data
    ut=u;
    if min(size(ut))==1 then ut=matrix(ut,1,-1),end

    function y=u(t)
      ind=find(dt <= t);nn=ind($);
      if (t==dt(nn) | nn==nt) then
        y=ut(:,nn)
      else
        y=ut(:,nn)+(t-dt(nn))/(dt(nn+1)-dt(nn))*(ut(:,nn+1)-ut(:,nn))
      end
    endfunction
    function ydot=%sim2(%tt,%y) ydot=ak*%y+bk*u(%tt);endfunction
  elseif type(u,'short') <> 'l' then
    function ydot=%sim2(%tt,%y) ydot=ak*%y+bk*u(%tt);endfunction
    ut=ones(mb,nt);for k=1:nt do ut(:,k)=u(dt(k)),end
  else
    %sim2=u;
    tx=' ';for l=2:size(u) do tx=tx+',%'+string(l-1);end
    text='function [ydot]=sk(%tt,%y,u'+tx+');ydot=ak*%y+bk*u(%tt'+tx+');endfunction';
    execstr(text);
    %sim2(0)=sk;u=u(1)
    text=['function [ut]=uu(t)';
          '['+part(tx,3:length(tx))+']=%sim2(3:'+string(size(%sim2))+');';
          'ut=ones(mb,nt);for k=1:nt, ut(:,k)=u(t(k)'+tx+'),end';"endfunction"];
    execstr(text);
    ut=uu(dt);
  end

  //simulation
  k=1;
  for n=bs do
    kk=k:k+n-1
    ak=a(kk,kk)
    bk=b(kk,:)
    nrmu=max([norm(bk*ut,1),norm(x0(kk))])
    if nrmu > 0 then
      if nargin < 5 then
        atol=1.E-12*nrmu;rtol=atol/100
      else
        atol=tol(1);rtol=tol(2)
      end
      x(kk,:)=ode(x0(kk),dt(1),dt,%sim2,rtol = rtol,atol = atol,type = "adams")
      if imp==1 then x(kk,:)=ak*x(kk,:)+bk*ut,end
    end
    k=k+n
  end
  if imp==0 then y=c*x+d*ut,else y=c*x,end
  if nargout==2 then x=v1*v2*x,end
endfunction
