function [y,R]=kpure(tf)
  //!
  // Copyright INRIA
  y=[],R=[]
  if type(tf,'short')=='linearsys' then tf=ss2tf(tf),end
  if type(tf,'short') <> 'r' then
    error("Error: expecting a transfert function or a linear system");
    return;
  end
  if ~or(tf.dom==['c','u']) then error('Error: expecting a continuous system');return;end
  if size(tf.num,'*') > 1 then error("Error: numerator should be 1x1");return;end

  r=routh_t(tf,poly(0,'k')),
  n=tf.num;d=tf.den;
  [s,t]=size(r);
  for i=1:s do
    coe=coeff(r(i,:)),
    if coe==0*ones(1,t) then error('Error: ---> infinite solution'),end
  end,

  z=0;u=0;eps=1E-7;

  for i=1:s do
    t=prod(size(r(i,:)));
    gd=r(i,1);
    for j=2:t do
      [gd,u]=bezout(gd,r(i,j)),
    end
    k=roots(gd)
    h=prod(size(k)),
    if h > 0 then
      for j=1:h do
        if isempty(y) || and(real(k(j)) <> y) then
          wr=roots(k(j)*n+d)
          [w,ki]=min(abs(real(wr)))
          if w < eps then
            y=[y,real(k(j))],
            R=[R,wr(ki)]
          end
        end
      end,
    end
  end
  [y,k]=sort(y)
  R=R(k)
endfunction
