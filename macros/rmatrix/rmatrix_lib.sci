// Copyright  2010-2015 Jean-Philippe Chancelier Cermics/Enpc, François Delebeceque, Serge Steer (Inria)
//
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
// 
// A set of overloaded functions for rationals 
// rewritten for nsp from scilab macros or from scratch 

function rmatrix_lib()
  
endfunction

// + 
//-----------------------------------

function r=plus_r_r(r1,r2)
  if size(r1,'*')==1 || size(r2,'*')==1 || size(r1).equal[size(r2)] then 
    r = p2r(r1.den.*r2.num + r2.den.*r1.num, r1.den.* r2.den,simp=%t);
  else
    error("Error: arguments should have compatible sizes\n");
    return;
  end
endfunction

function r1=plus_r_m(r,m)
  if size(m,'*')==1 || size(r).equal[size(m)] then 
    r1 = p2r(r.num + m.*r.den, r.den,simp=%t);
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
    r1 = p2r(r.num + p.*r.den, r.den,simp=%t);
  else
    error("Error: arguments should have compatible sizes\n");
    return;
  end
endfunction

function r1=plus_p_r(p,r)
  r1=plus_r_p(r,p);
endfunction

// * 
//-----------------------------------

function r1=mult_r_r(r1,r2)
//
  n1=r1.num; d1=r1.den;
  n2=r2.num; d2=r2.den;
  [l1,m1]=size(n1);[l2,m2]=size(n2),
  if min([l1*m1,l2*m2])==1 then
    [num,den]=simp(n1*n2,d1*d2);
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
// XXX function crash if using 
// r1 = r *  m2r(m,var=r.get_var[],dim=".");
  var=r.get_var[]
  r1 = r *  m2r(m,var=var,dim=".");
endfunction

function r1=mult_m_r(m,r)
  var = r.get_var[];
  r1 =  m2r(m,var=var,dim=".")*r;
endfunction

function r1=mult_r_p(r,p)
  r1= r * p2r(p);
endfunction

function r1=mult_p_r(p,r)
  r1= p2r(p) * r;
endfunction

// .* 
//-----------------------------------

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
//-----------------------------------

function r=dsl_r_r(r1,r2)
  [n1,d1]=simp(r1.num .* r2.den,r1.den .* r2.num);
  r=p2r(n1,d1);
endfunction

function r1=dsl_r_m(r,m)
  r1=p2r(r.num,r.den .* m,simp=%t);
endfunction

function r1=dsl_m_r(m,r)
  r1=p2r( r.den .* m, r.num ,simp=%t);
endfunction

function r1=dsl_r_p(r,p)
  r1=p2r(r.num,r.den .* p,simp=%t);
endfunction

function r1=dsl_p_r(p,r)
  r1=p2r( p .* r.den,r.num,simp=%t);
endfunction

function r1=dsl_p_p(p1,p2)
  if size(p1,'*')==1 && size(p2,'*') > 0 then 
    p1 = p1*ones(size(p2));
  elseif size(p2,'*')==1 && size(p1,'*') > 0 then 
    p2 = p2*ones(size(p1));
  end
  if size(p1).equal[size(p2)] then 
    [n1,d1]=simp( p1, p2)
    r1=p2r(n1,d1);
  else
    error("Error: arguments should have compatible sizes\n");
    return;
  end
endfunction

function r1=dsl_m_p(m,p2)
  if size(m,'*')==1 && size(p2,'*') > 0 then 
    m = m*ones(size(p2));
  elseif size(p2,'*')==1 && size(m,'*') > 0 then 
    p2 = p2*ones(size(m));
  end
  if size(p2).equal[size(p2)] then 
    [n1,d1]=simp( m2p(m,dim=".",var=p2.get_var[]), p2)
    r1=p2r(n1,d1);
  else
    error("Error: arguments should have compatible sizes\n");
    return;
  end
endfunction

function r1=dsl_p_m(p1,m)
  if size(m,'*')==1 && size(p1,'*') > 0 then 
    m = m*ones(size(p1));
  elseif size(p1,'*')==1 && size(m,'*') > 0 then 
    p1 = p1*ones(size(m));
  end
  if size(p1).equal[size(m)] then 
    var = p1.get_var[];
    [n1,d1]=simp( p1, m2p(m,dim=".",var=var));
    r1=p2r(n1,d1);
  else
    error("Error: arguments should have compatible sizes\n");
    return;
  end
endfunction

// - 
//-----------------------------------

function r=minus_r_r(r1,r2)
  if size(r1,'*')==1 || size(r2,'*')==1 || size(r1).equal[size(r2)] then 
    [n1,d1]=simp( r1.num.*r2.den - r2.num.*r1.den, r1.den.* r2.den);
    r=p2r(n1,d1);
  else
    error("Error: arguments should have compatible sizes\n");
    return;
  end
endfunction

function r1=minus_r_m(r,m)
  if size(m,'*')==1 || size(r).equal[size(m)] then 
    r1=p2r( r.num - m.*r.den,r.den, simp=%t);
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
    r1=p2r( m.*r.den -r.num ,r.den, simp=%t);
  else
    error("Error: arguments should have compatible sizes\n");
    return;
  end
endfunction

function r1=minus_r_p(r,p)
  if size(p,'*')==1 || size(r).equal[size(p)] then 
    r1=p2r( r.num - p.*r.den,r.den, simp=%t);
  else
    error("Error: arguments should have compatible sizes\n");
    return;
  end
endfunction

function r1=minus_p_r(p,r)
  if size(p,'*')==1 || size(r).equal[size(p)] then 
    r1=p2r( p.*r.den -r.num ,r.den, simp=%t);
  else
    error("Error: arguments should have compatible sizes\n");
    return;
  end
endfunction

// ^
//-----------------------------------

// .^
//-----------------------------------

// sum 
//-----------------------------------
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
//-----------------------------------
function y=concatr_r_p(r,p)
  var = r.get_var[];
  y=[r,p2r(p,var=var)];
endfunction
  
function y=concatr_p_r(p,r)
  var = r.get_var[];
  y=[p2r(p,var=var),r];
endfunction

function y=concatd_r_p(r,p)
  var = r.get_var[];
  y=[r;p2r(p,var=var)];
endfunction
  
function y=concatd_p_r(p,r)
  var = r.get_var[];
  y=[p2r(p,var=var);r];
endfunction

// det 
//-----------------------------------

function d=det_r(r)
// copyright scilab 
  [n,d]=lcmdiag(r);
  d = det(n) / det(d);
  d.set_var[r.get_var[]];
endfunction


// clean 
//-----------------------------------

function r=clean_r(r,varargin)
  r=p2r(clean(r.num,varargin(:)),clean(r.den,varargin(:)),simp=%t);
endfunction

// div / 
//-----------------------------------

function r= div_r_r(r1,r2)
  if size(r2,'*')== 1 then 
    r = r1 ./ r2;
  elseif size(r2,'r')==size(r2,'c') then 
    r = inv(r2)*r1;
  else
    error("Error: r1/r2 is implemented for rationals when r2 is 1x1 or square");
  end
endfunction

function r= div_m_p(m,p)
  if size(p,'*')== 1 then 
    r = m ./ p;
  elseif size(p,'r')==size(p,'c') then 
    r = inv(p)*m;
  else
    error("Unimplemented");
  end
endfunction


function r= div_m_r(m,r)
  if size(r,'*')== 1 then 
    r = m ./ r;
  elseif size(r,'r')==size(r,'c') then 
    r = inv(r)*m;
  else
    error("Unimplemented");
  end
endfunction

function r= div_r_m(r,m)
  if size(m,'*')== 1 then 
    r = r ./ m;
  elseif size(m,'r')==size(m,'c') then 
    r = inv(m)*r;
  else
    error("Unimplemented");
  end
endfunction

function r= div_r_p(r,p)
  if size(p,'*')== 1 then 
    r = r ./ p;
  elseif size(p,'r')==size(p,'c') then 
    r = inv(p)*r;
  else
    error("Unimplemented");
  end
endfunction

function r= dsl_m_p(m,p)
  if and(size(m)==size(p)) then 
    r = p2r(m2p(m,var=p.get_var[],dim="."),p);
  elseif size(m,'*') == 1 
    r = p2r(m2p(m*ones(size(p)),var=p.get_var[], dim="."),p)
  elseif size(p,'*')== 1 then 
    r = p2r(m2p(m,var=p.get_var[], dim="."),p*ones(size(m)));
  else
    error("Unimplemented");
  end
endfunction

function res= div_p_r(p,r)
  if size(r,'*')== 1 then 
    res = p ./ r;
  elseif size(r,'r')==size(r,'c') then 
    res = inv(r)*p;
  else
    error("Unimplemented");
  end
endfunction

// .\ 
//-----------------------------------

function f=dbs_p_p(p1,p2)
//f= p1.\p2
  f= p2r(p2,p1,simp=%t);
endfunction

function r=dbs_p_r(p,r)
// r= p.\ r   polynomial./rational
  r= p2r(r.num, p.* r.den,simp=%t);
endfunction

function f=dbs_p_m(p1,m)
// f=p.\m
  var=p1.get_var[];
  f= p2r( m2p(m, var=var,dim="."), p1, simp=%t);
endfunction

function f=dbs_r_p(f1,f2)
//   r.\p
  f= p2r( f1.den.*f2,f1.num,simp=%t);
endfunction

function s=dbs_r_r(s1,s2)
  s = p2r( s1.den.*s2.num,s1.num.*s2.den,simp=%t);
endfunction

function res=dbs_r_m(r,m)
//  r.\m
  if size(m,'*')==0 then res=m2r([],var=r.get_var[]),return,end
  res= p2r(r.den.*m,r.num,simp=%t);
endfunction

function [z]=dbs_m_p(x,y)
// z = x .\ y 
  z = y ./ x 
endfunction

function res=dbs_m_r(m,r)
  if size(m,'*')==0 then res=m2r([],var=r.get_var[]),return,end
  res= r ./ m;
endfunction


// ^ 
//-----------------------------------

function res=hat_r_m(r,m)
  if size(m,'*') <> 1 then 
    error("Error: second arguments should be scalar");
    return;
  end
  if int(m) <> m then 
    error("Error: second arguments should be integer");
    return;
  end
  if m == 0 then 
    res=m2r(ones(size(r)),var=r.get_var[],dim='.');
    return 
  end
  if size(r,1)<>size(r,2) then 
    res = r .^ m;
    return
  end
  if m >= 0 then
    res= r;
    for i=2:m, res = res*r;end 
  else
    // should detect non invertible matrices XXXXX
    r = inv(r);
    res= r;
    for i=2:abs(m), res = res*r;end 
  end
endfunction

function res=dh_r_m(r,m)
  
  if size(m,'*') == 1 && size(r,'*') == 1 then 
    if m == 0 then 
      res=m2r(ones(size(r)),var=r.get_var[],dim='.');
      return 
    end
    if m >= 0 then 
      res= r;
      for i=2:m, res = res*r;end 
    else
      r=p2r(r.den,r.num);
      res=r;
      for i=2:abs(m), res = res*r;end 
    end
    return;
  end
  
  if size(m,'*') == 1 then 
    if int(m) <> m then 
      error("Error: second arguments should be integer");
      return;
    end
    res = r; for i=1:size(r,'*'); res(i) = r(i).^m;end
    return;
  end
  
  if ~size(m).equal[size(r)] then 
    error("Error: the two arguments should have the same size");
    return;
  end
  
  res = r;
  for i=1:size(r,'*');    res(i) = r(i).^ m(i); end 
endfunction
