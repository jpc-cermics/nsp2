function [q,fact]=lcm_m(p)
// the version with two arguments is hard coded.
// computes the lcm of an int vector 
// [pp,fact]=lcm(p) computes besides the vector fact of factors 
// such that  p.*fact=pp*ones(p)
//
// Copyright INRIA, S. Steer
  if size(p,'*')== 0 then q=p;fact=p;end 
  k=find(p==0);
  if ~isempty(k) then 
    q=p(k(1)), fact=0*ones(p),fact(k)=1,
    return;
  end
  q=p(1);
  for k=2:size(p,'*')
    q=q*p(k) ./ gcd(q,p(k));
  end
  fact=q ./ p
endfunction

function [x]=gcd_m(p)
// the version with two arguments is hard coded.
// computes the gcd of an int vector 
// and a unimodular matrix (with polynomial inverse) u, 
// with minimal degree such that [p1 p2]*u=[0 ... 0 pgcd]
// XXXX reste le calcul de u noter que dans euclide on n'a pas 
// directement u.
//
// Copyright INRIA
  mn=size(p,'*');
  if mn == 0 then x=p;uu=p;end
  one=1;
  x=p(1);
  for l=2:mn,
    // [x,u]=bezout(x,p(l)),
    [x,b,c]=euclide(x,p(l));
    if nargout==2 then
      // one=[one(:,1:l-2),one(:,l-1)*u(1,[2 1])];one(l,l-1:l)=u(2,[2 1]);
    end
  end,
endfunction

