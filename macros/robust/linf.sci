function [n]=linf(g,eps,tol)
//linf(g,[eps],[tol])  L_infinity norm
//    n=sup [sigmax(g(jw)] (sigmax largest singular value).
//       w
//-- g is a syslin system.
//-- eps is error tolerance on n.
//-- tol threshold for imaginary axis poles.
// See also: h_norm
//!
// Copyright INRIA
  if type(g,'short')=='r' then g=tf2ss(g);end
  if type(g,'short')=='m',if norm(g)==0,n=0,return,end,end,
  
  if type(g,'short')<>'linearsys' then 
    error("Error: expecting a linear system or transfer matrix");
    return;
  end
  if g(7)<>'c' then
    error('system must be continuous')
  else
    g(7)='c';
  end
  select nargin,
   case 1 then eps=1e-7,tol=1000*%eps,
   case 2 then tol=1000*%eps,
  end,
  [a,b,c,d]=abcd(g);[t,vvt]=size(a),
  p=ctr_gram(g),q=obs_gram(g);
  //Algorithm:
  //----------
  //1. min , max.
  //----------------------------------
  [slp,slm]=dtsi(g),
  if slp==0 then pp=0,qq=0,tp=1,
    pm=p,qm=q,tm=t,
  else
    if slm==0 then pm=0,qm=0,tm=1,
      pp=p,qq=q,tp=t,
    else
      [tp,vtp]=size(slp(2)),[tm,vtm]=size(slm(2)),
      pp=ctr_gram(slp),qq=obs_gram(slp),
      pm=ctr_gram(slm),qm=obs_gram(slm),
    end,
  end,
  hsvp=sqrt(spec(pp*qq)),hsvp=sort(real(hsvp)),
  hsvm=sqrt(spec(pm*qm)),hsvm=sort(real(hsvm)),
  gl=max([norm(d),hsvp(tp),hsvm(tm)]),
  gu=norm(d)+2*(sum(hsvp)+sum(hsvm)),
  //2. binary search
  //----------------------
  while gu-gl>2*eps*gl,
    x=(gl+gu)/2,
    r=d'*d-(x*x)*eye(size(d'*d)),s=d*d'-(x*x)*eye(size(d*d')),
    mx=[a-b/r*d'*c, -x*b/r*b'; ..
	x*c'/s*c,   -a'+c'*d/r*b'],
    mp=abs(real(spec(mx))),mp=min(mp),
    if mp>tol then gu=x, else gl=x, end,
  end
  n=(gu+gl)/2
endfunction
