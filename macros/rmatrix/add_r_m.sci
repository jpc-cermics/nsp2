// + 

function r=plus_r_r(r1,r2)
  if size(r1,'*')==1 || size(r2,'*')==1 || size(r1).equal[size(r2)] then 
    [n1,d1]=simp( r1.den.*r2.num + r2.den.*r1.num, r1.den.* r2.den);
    r=p2r(n1,d1);
  else
    error("Error: arguments should have compatible sizes\n");
    return;
  end
endfunction

function r1=plus_r_m(r,m)
  if size(m,'*')==1 || size(r).equal[size(m)] then 
    n=r.num;d = r.den;
    [n1,d1]=simp(n+m.*d,d);
    r1=r; r1.set_num[n1]; r1.set_den[d1];
  else
    error("Error: arguments should have compatible sizes\n");
    return;
  end
endfunction

function r1=plus_m_r(m,r)
  r1=plus_r_m(r,m);
endfunction

function r1=plus_r_p(r,p)
  if size(p,'*')==1 || size(r).equal[size(p)] then 
    n=r.num;d = r.den;
    [n1,d1]=simp(n+p*d,d);
    r1=r; r1.set_num[n1]; r1.set_den[d1];
  else
    error("Error: arguments should have compatible sizes\n");
    return;
  end
endfunction

function r1=plus_p_r(p,r)
  r1=plus_r_p(r,p);
endfunction

// * 
//------

function r1=mult_r_r(r1,r2)
//!
// Copyright INRIA
  n1=r1.num; d1=r1.den;
  n2=r2.num; d2=r2.den;
  [l1,m1]=size(n1);[l2,m2]=size(n2),
  if min([l1*m1,l2*m2])==1 then
    num=n1*n2
    den=d1*d2
  else
    if m1<>l2 then error("Error: incompatible dimensions");
    end
    pp=m2p([],var=r1.get_var[]);
    num=pp;den=pp;
    for i=1:l1, pp(i)=lcm(d1(i,:)),end
    for j=1:m2,
      y=lcm(d2(:,j)),
      for i=1:l1,
	yij=y*pp(i),
	x=m2p(0,var=r1.get_var[]);
	for k=1:m1,
	  x=x+n1(i,k)*n2(k,j)*pdiv(y,d2(k,j))*pdiv(pp(i),d1(i,k));
	end
	num(i,j)=x,den(i,j)=yij,
      end
    end
    [num,den]=simp(num,den),
  end
  r1=p2r(num,den);
endfunction

function r1=mult_r_m(r,m)
  r1 = r * m2r(m,var=r.get_var[],dim=".");
endfunction

function r1=mult_m_r(m,r)
  r1 =  m2r(m,var=r.get_var[],dim=".")*r;
endfunction

function r1=mult_r_p(r,p)
  r1= r * p2r(p);
endfunction

function r1=mult_p_r(p,r)
  r1= p2r(p) * r;
endfunction

// .* 

function r=dst_r_r(r1,r2)
  [n1,d1]=simp(r1.num .* r2.num , r1.den .* r2.den);
  r=p2r(n1,d1);
endfunction

function r1=dst_r_m(r,m)
  [n1,d1]=simp(r.num .* m ,r.den)
  r1=p2r(n1,d1);
endfunction

function r1=dst_m_r(m,r)
  [n1,d1]=simp( r.num .* m,r.den);
  r1=p2r(n1,d1);
endfunction

function r1=dst_r_p(r,p)
  [n1,d1]=simp(r.num .* p ,r.den)
  r1=p2r(n1,d1);
endfunction

function r1=dst_p_r(p,r)
  [n1,d1]=simp( p .* r.num ,r.den);
  r1=p2r(n1,d1);
endfunction

// ./   with r 

function r=dsl_r_r(r1,r2)
  [n1,d1]=simp(r1.num .* r2.den,r1.den .* r2.num);
  r=p2r(n1,d1);
endfunction

function r1=dsl_r_m(r,m)
  [n1,d1]=simp(r.num,r.den .* m);
  r1=p2r(n1,d1);
endfunction

function r1=dsl_m_r(m,r)
  [n1,d1]=simp( r.den .* m,r.num);
  r1=p2r(n1,d1);
endfunction

function r1=dsl_r_p(r,p)
  [n1,d1]=simp(r.num,r.den .* p);
  r1=p2r(n1,d1);
endfunction

function r1=dsl_p_r(p,r)
  [n1,d1]=simp( p .* r.den,r.num);
  r1=p2r(n1,d1);
endfunction

function r1=dsl_p_p(p1,p2)
  [n1,d1]=simp( p1, p2)
  r1=p2r(n1,d1);
endfunction

function r1=dsl_m_p(m,p2)
  [n1,d1]=simp( m2p(m,dim=".",var=p2.get_var[]), p2)
  r1=p2r(n1,d1);
endfunction

// - 

function r=minus_r_r(r1,r2)
  if size(r1,'*')==1 || size(r2,'*')==1 || size(r1).equal[size(r2)] then 
    [n1,d1]=simp( r1.den.*r2.num - r2.den.*r1.num, r1.den.* r2.den);
    r=p2r(n1,d1);
  else
    error("Error: arguments should have compatible sizes\n");
    return;
  end
endfunction

function r1=minus_r_m(r,m)
  if size(m,'*')==1 || size(r).equal[size(m)] then 
    n=r.num;d = r.den;
    [n1,d1]=simp(n - m.*d,d);
    r1=r; r1.set_num[n1]; r1.set_den[d1];
  else
    error("Error: arguments should have compatible sizes\n");
    return;
  end
endfunction

function r1=minus_r(r)
  r1=p2r(- r.num,r.den);
endfunction

function r1=minus_m_r(m,r)
  if size(m,'*')==1 || size(r).equal[size(m)] then 
    n=r.num;d = r.den;
    [n1,d1]=simp( m.*d -n,d);
    r1=r; r1.set_num[n1]; r1.set_den[d1];
  else
    error("Error: arguments should have compatible sizes\n");
    return;
  end
endfunction

function r1=minus_r_p(r,p)
  if size(p,'*')==1 || size(r).equal[size(p)] then 
    n=r.num;d = r.den;
    [n1,d1]=simp(n - p*d,d);
    r1=r; r1.set_num[n1]; r1.set_den[d1];
  else
    error("Error: arguments should have compatible sizes\n");
    return;
  end
endfunction

function r1=minus_p_r(p,r)
  if size(p,'*')==1 || size(r).equal[size(p)] then 
    n=r.num;d = r.den;
    [n1,d1]=simp( p*d -n,d);
    r1=r; r1.set_num[n1]; r1.set_den[d1];
  else
    error("Error: arguments should have compatible sizes\n");
    return;
  end
endfunction

// ^

// .^

// sum 
function y=sum_r(r,tag)
  if nargin <= 1 then tag = '*';end
  if isempty(r) then y=zeros(0,1);end
  if tag.equal[1] || tag.equal['r'] then 
    y=r(1,:); for i=2:size(r,'r'); y=y+r(i,:);end
  elseif tag.equal[1] || tag.equal['c'] then 
    y=r(:,1); for i=2:size(r,'c'); y=y+r(:,i);end
  elseif tag.equal['*'] then 
    y=r(1); for i=1:size(r,'*'); y = y+r(i);end
  else
    error("Error: unknown tag\n");
    return;
  end
endfunction

// concat 

function y=concatr_r_p(r,p)
  y=[r,p2r(p,var=r.get_var[])];
endfunction
  
function y=concatr_p_r(p,r)
  y=[p2r(p,var=r.get_var[]),r];
endfunction

function y=concatd_r_p(r,p)
  y=[r;p2r(p,var=r.get_var[])];
endfunction
  
function y=concatd_p_r(p,r)
  y=[p2r(p,var=r.get_var[]);r];
endfunction

// det 

function d=det_r(r)
// copyright scilab 
  [n,d]=lcmdiag(r);
  x=det(n) / det(d);
endfunction

//  operator / for rationals 

function s1=r_r_r(s1,s2)
// s1/s2    for rationals
//!
// Copyright INRIA
  // [s1,s2]=sysconv(s1,s2),
  [n,m]=size(s2.num)
  if n<>m then error("Error: matrix should be square");return;end
  if n*m==1 then
    s1= s1 * p2r(s2.den,s2.num);
  else
    p=s2.num;
    s2=s2.den;
    for k=1:n
      pp=lcm(s2(:,k))
      for l=1:n;p(l,k)=p(l,k)*pdiv(pp,s2(l,k)),end
      s1(:,k)=s1(:,k)*pp
    end
    s1=s1*invr(p)
  end
endfunction

