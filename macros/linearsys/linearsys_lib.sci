// Copyright  2010-2015 
// Jean-Philippe Chancelier Cermics/Enpc, François Delebeceque, Serge Steer (Inria)
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
// A set of overloaded functions for linear systems
// rewritten for nsp from scilab macros or from scratch 

function linearsys_lib()
endfunction

// .\
//---------------

function s=bdiv_linearsys_linearsys(s1,s2)
  s=inv(s1)*s2;
endfunction

function [s]=bdiv_linearsys_p(s,p)
  s = inv(s1) * p
endfunction

function [s]=bdiv_linearsys_r(s1,s2)
  s= inv(tf2ss(s1))*s2
endfunction

function s=bdiv_linearsys_m(s1,s2)
  s=inv(s1)*s2
endfunction

function [sr]=bdiv_p_linearsys(p,s)
  sr=linear_system([],[],[],p,[],[]) \ s
endfunction

function s=bdiv_r_linearsys(s1,s2)
  s= tf2ss(s1) \s2
endfunction

function s=bdiv_s_linearsys(s1,s2)
  s=inv(s1)*s2
endfunction

// [ # ] 
//---------------

function [s]=concatdiag_linearsys_linearsys(s1,s2)
//  s=[s1;s2] in state-space
  [s1,s2]=sysconv(s1,s2)
  s=linear_system([s1.A # s2.A],[s1.B # s2.B],[s1.C # s2.C],... 
		  [s1.D # s2.D],[s1.X0;s2.X0],dom=s1.dom)
endfunction

function [s]=concatdiag_linearsys_r(s1,s2)
  [s1,s2]=sysconv(s1,s2);s=[s1# s2];
endfunction

function [s]=concatdiag_r_linearsys(s1,s2)
  s=[tf2ss(s1) # s2]
endfunction

function [s]=concatdiag_linearsys_p(s1,s2)
  s=[s1 # linear_system([],[],[],s2,[],dom=s1.dom,sample=s1.dt)];
endfunction

function [s]=concatdiag_linearsys_m(s1,s2)
  s=[s1 # linear_system([],[],[],s2,[],dom=s1.dom,sample=s1.dt)];
endfunction

function [s]=concatdiag_p_linearsys(s1,s2)
// s=[d1,s2]
  s=[ linear_system([],[],[],s1,[],dom=s2.dom,sample=s2.dt) # s2];
endfunction

function s=concatdiag_m_linearsys(s1,s2)
  s=[ linear_system([],[],[],s1,[],dom=s2.dom,sample=s2.dt) # s2];
endfunction

function [s]=concatd_linearsys_linearsys(s1,s2)
//  s=[s1;s2] in state-space
  [s1,s2]=sysconv(s1,s2)
  s=linear_system([s1.A # s2.A],[s1.B;s2.B],[s1.C # s2.C],... 
		  [s1.D;s2.D],[s1.X0;s2.X0],dom=s1.dom)
endfunction

// [;]
//---------------

function [s]=concatd_linearsys_p(s1,s2)
  [n1,m1]=size(s1.C);[p2,m2]=size(s2);
  s=linear_system(s1.A,s1.B,[s1.C;0*ones(p2,m1)],[s1.D;s2],s1.X0,dom=s1.dom)
endfunction

function [s]=concatd_linearsys_r(s1,s2)
  [s1,s2]=sysconv(s1,s2);s=[s1;s2];
endfunction

function [s]=concatd_linearsys_m(s1,s2)
// s=[s1;s2]
  [n1,m1]=size(s1.C);[p2,m2]=size(s2);
  s=linear_system(s1.A,s1.B,[s1.C;0*ones(p2,m1)],[s1.D;s2],s1.X0,dom=s1.dom)
endfunction

function [s]=concatd_p_linearsys(d1,s2)
// s=[d1,s2]
  [n2,m2]=size(s2.C);[p1,m1]=size(d1)
  s=linear_system(s2.A,s2.B,[0*ones(p1,m2);s2.C],[d1;s2.D],s2.X0,dom=s2.dom)
endfunction

function [s]=concatd_r_linearsys(s1,s2)
  s=[tf2ss(s1);s2]
endfunction

function s=concatd_m_linearsys(d1,s2)
// s=[d1,s2]
  [n2,m2]=size(s2.C);[p1,m1]=size(d1)
  s=linear_system(s2.A,s2.B,[0*ones(p1,m2);s2.C],[d1;s2.D],s2.X0,dom=s2.dom);
endfunction

// [,]
//---------------


function [s]=concatr_linearsys_linearsys(s1,s2)
  [s1,s2]=sysconv(s1,s2);
  s= linear_system([s1.A # s2.A], [s1.B # s2.B],...
		   [s1.C,s2.C],[s1.D,s2.D],[s1.X0;s2.X0],dom=s1.dom);
endfunction

function [s]=concatr_linearsys_p(s1,s2)
  [n1,m1]=size(s1.B);[p2,m2]=size(s2);
  s=linear_system(s1.A,[s1.B,0*ones(n1,m2)],s1.C,[s1.D,s2],s1.X0,dom=s1.dom);
endfunction

function [s]=concatr_linearsys_r(s1,s2)
  s= [s1, tf2ss(s2)]
endfunction

function [s]=concatr_linearsys_m(s1,s2)
  [n1,m1]=size(s1.B);[p2,m2]=size(s2);
  s=linear_system(s1.A,[s1.B 0*ones(n1,m2)],s1.C,[s1.D,s2],s1.X0,dom=s1.dom);
endfunction

function s=concatr_r_linearsys(s1,s2)
  s=[tf2ss(s1),s2]
endfunction

function s=concatr_m_linearsys(d1,s2)
  [n2,m2]=size(s2.B);[p1,m1]=size(d1)
  s=linear_system(s2.A,[0*ones(n2,m1),s2.B],s2.C,[d1,s2.D],s2.X0,dom=s2.dom);
endfunction

function [s]=concatr_p_linearsys(d1,s2)
  [n2,m2]=size(s2.B);[p1,m1]=size(d1)
  s=linear_system(s2.A,[0*ones(n2,m1),s2.B],s2.C,[d1,s2.D],s2.X0,dom=s2.dom);
endfunction

//   A/B 
//---------------

function [s]=div_linearsys_linearsys(s1,s2)
  [s1,s2]=sysconv(s1,s2)
  s=s1*inv(s2)
endfunction

function [sr]=div_linearsys_p(s,p)
// sr=div_linearsys_p(s,p) <=> sr=s/p
  sr=s/ linear_system([],[],[],p,[],[]);
endfunction

function s=div_linearsys_r(s1,s2)
  s=s1/ tf2ss(s2)
endfunction

function s1=div_linearsys_m(s1,d2)
  s1.B=s1.B/d2;
  s1.D=s1.D/d2;
endfunction

function [sr]=div_p_linearsys(p,s)
  sr=linear_system([],[],[],p,[],[]) / s
endfunction

function s=div_r_linearsys(s1,s2)
  s= tf2ss(s1)/s2
endfunction

function [sr]=div_m_linearsys(p,s)
  sr=linear_system([],[],[],p,[],[]) / s
endfunction

// A==B
//---------------

function [r]=eq_linearsys_linearsys(s1,s2)
  r = s1.equal[s2];
endfunction

function [r]=eq_linearsys_p(s1,s2)
  r=%f;
endfunction

function [r]=eq_linearsys_r(s1,s2)
  r=%f;
endfunction

function [r]=eq_linearsys_m(s1,s2)
  r= isempty(s1.B) && s1.D.equal[s2]
endfunction

function [r]=eq_p_linearsys(s1,s2)
  r=%f
endfunction

function r=eq_r_linearsys(s1,s2)
  r=%f
endfunction

function [r]=eq_s_linearsys(s1,s2)
  r= isempty(s2.B) && s1.equal[s2.D];
endfunction

// sl(i,j) , sl(:,j), sl(j,:) sl(i)
//---------------

function slo=extract_linearsys(sl,i,j)
  slo=linear_system(sl.A,sl.B(:,j),sl.C(i,:),sl.D(i,j),sl.X0,dom=sl.dom,sample=sl.dt);
endfunction

function slo=extractrows_linearsys(sl,i)
  slo=linear_system(sl.A,sl.B,sl.C(i,:),sl.D(i,:),sl.X0,dom=sl.dom,sample=sl.dt);
endfunction

function slo=extractcols_linearsys(sl,j)
  slo=linear_system(sl.A,sl.B(:,j),sl.C,sl.D(:,j),sl.X0,dom=sl.dom,sample=sl.dt);
endfunction

// - 
//---------------

function s2=minus_linearsys(s1)
// s=-s1
  s2 = s1; s2.C=-s1.C; s2.D=-s1.D;
endfunction

function [s]=minus_linearsys_linearsys(s1,s2)
  [s1 s2]=sysconv(s1,s2)
  [n1,vn1]=size(s1.A);
  [n2,vn2]=size(s2.A);
  A=[s1.A,0*ones(n1,n2);0*ones(n2,n1),s2.A];
  B=[s1.B;-s2.B];
  C=[s1.C,s2.C];
  D=[s1.D-s2.D];
  s=linear_system(A,B,C,D,[s1.X0;s2.X0],dom=s1.dom,sample=s1.dt);
endfunction

function s=minus_linearsys_p(s1,s2)
  s = s1; s.D= s1.D - s2;
endfunction

function s=minus_linearsys_r(s1,s2)
  [s1,s2]=sysconv(s1,s2)
  s=s1-s2
endfunction

function s=minus_linearsys_m(s1,s2)
  s=s1; s.D=s1.D- s2
endfunction

function s=minus_p_linearsys(s1,s2)
//s=minus_p_linearsys(s1,s2.D) <=> s=p-s1
// s1 : syslin list
// p  : polynomial matrix
  s=s2; s.D= s1 -s2.D
endfunction

function s=minus_r_linearsys(s1,s2)
  [s1,s2]=sysconv(s1,s2)
  s=s1-s2
endfunction

function s=minus_m_linearsys(s1,s2)
  s=linear_system(s2.A,-s2.B,s2.C,s1-s2.D,s2.X0,dom=s2.dom,sample=s2.dt),
endfunction

// *
//---------------

function [SS]=mult_linearsys_linearsys(S1,S2)
// computes S1*S2 in state-space form.
  [S1,S2]=sysconv(S1,S2)
  if max(S1.D.degree[])==0 && max(S2.D.degree[])==0 then
    D1=coeff(S1.D);D2=coeff(S2.D);
    B1C2= S1.B * S2.C
    SS=linear_system([S1.A,B1C2;0*B1C2',S2.A],[S1.B*D2;S2.B],...
		     [S1.C,D1*S2.C],D1*D2,[S1.X0;S2.X0],dom=S1.dom,sample=S1.dt);
    SS.D.set_var[S1.D.get_var[]];
    return
  end
  //improper systems
  J=[S1.A              ,S1.B*S2.C;
     0*ones(size(S1.B*S2.C))',S2.A];
  Ls=[S1.C, S1.D*S2.C]'
  Ms=[S1.B * S2.D;S2.B]

  if isempty(Ms) || isempty(Ls) then
    SS=linear_system([],[],[],S1.D*S2.D,[S1.X0;S2.X0],dom=S1.dom,sample=S1.dt)
    return;
  end
  //
  D12=S1.D*S2.D;
  if type(D12,'short')=='p' then
    s=poly(0,D12.get_var[]);
  end
  deg=max(Ms.degree[]);
  B=coeff(Ms,deg);
  Ps=0*B
  for i=1:deg
    Ps=s*Ps+B
    B=J*B+coeff(Ms,deg-i)
  end
  //
  deg=max(Ls.degree[]);  J=J'
  C=coeff(Ls,deg);
  pps=0*C
  for i=1:deg
    pps=s*pps+C
    C=J*C+coeff(Ls,deg-i)
  end
  //
  C=C';
  D=pps'*B+Ls'*Ps+S1.D * S2.D;
  Dg=max(D.degree[]);
  // if Dg==0 then D=coeff(D);end// jpc: keep Dg as polynomial to get its varname
  SS=linear_system(J',B,C,D,[S1.X0;S2.X0],dom=S1.dom,sample=S1.dt);
endfunction

function [s]=mult_linearsys_p(s1,p2)
  s=s1*linear_system([],[],[],p2,[],dom='u');
endfunction

function [s]=mult_linearsys_r(s1,s2)
  s=s1 *tf2ss(s2);
endfunction

function s=mult_linearsys_m(s1,D2)
// s=s1*gain
// SISO case FD
  [n2,m2]=size(D2);
  if prod(size(s1))==1 then
    if n2==1 then 
      // D= s1.D*D2; // [s1.A,s1.B * D2]; 
      s=linear_system( s1.A,s1.B*D2,s1.C, s1.D*D2 ,s1.X0,dom=s1.dom,sample=s1.dt);
      return;
    end
    if m2==1 then 
      s=linear_system(s1.A,s1.B, D2*s1.C, D2*s1.D,s1.X0,dom=s1.dom,sample=s1.dt);
      return;
    end   
    [Q,M]=fullrf(D2);[n2,mq]=size(Q);
    if mq==1 then 
      s=Q*linear_system(s1.A,s1.B*M,s1.C, s1.D*M,s1.X0,dom=s1.dom,sample=s1.dt);
      return;
    end  
    w=s1; 
    for k=2:mq, w=sysdiag(w,s1);end
    s=w*M;s=Q*s;
    return;
  end
  s=linear_system(s1.A,s1.B*D2,s1.C, s1.D*D2,s1.X0,dom=s1.dom,sample=s1.dt);
endfunction

function [s]=mult_p_linearsys(s2,s1)
//   s2*s1
  s=linear_system([],[],[],s2,[],dom='u')*s1
endfunction

function s=mult_r_linearsys(s1,s2)
  s= tf2ss(s1)*s2
endfunction

function s=mult_m_linearsys(d1,s2)
// s=d1*s2
  s=linear_system(s2.A,s2.B,d1*s2.C,d1*s2.D,s2.X0,dom=s2.dom,sample=s2.dt)
endfunction

// <>
//---------------

function [r]=ne_linearsys_linearsys(s1,s2)
  r = ~s1.equal[s2]
endfunction

function [r]=ne_linearsys_p(s1,s2)
  r=%f
endfunction

function [r]=ne_linearsys_r(s1,s2)
  r=%f
endfunction

function [r]=ne_linearsys_s(s1,s2)
  r= ~iesmpty(s1.B) || s2.equal[s1.D]
endfunction

function [r]=ne_p_linearsys(s1,s2)
  r=%f
endfunction

function r=ne_r_linearsys(s1,s2)
  r=%f
endfunction

function [r]=ne_s_linearsys(s1,s2)
  r= ~iesmpty(s2.B) || s1.equal[s2.D];
endfunction

// +
//---------------

function [s]=plus_linearsys_linearsys(s1,s2)
//  s=s1+s2 in state-space
  [s1 s2]=sysconv(s1,s2);
  s=linear_system([s1.A # s2.A],[s1.B;s2.B],[s1.C,s2.C],...
		  s1.D+s2.D,[s1.X0;s2.X0],dom=s1.dom,sample=s1.dt)
endfunction

function [s]=plus_linearsys_p(s1,s2)
  s=s1; s.D = s.D+s2
endfunction

function [s]=plus_linearsys_r(s1,s2)
  tf = tf2ss(s2)
  s= s1 + tf;
endfunction

function [s]=plus_linearsys_m(s1,s2)
  s=s1;
  s.D = s.D + s2
endfunction

function s=plus_p_linearsys(s1,s2)
  s = s2; s.D= s1 +s.D
endfunction

function s=plus_r_linearsys(s1,s2)
  s= tf2ss(s1) +s2;
endfunction

function s=plus_m_linearsys(d1,s2)
  s = s2; s.D=s.D+ d1
endfunction

// '
//---------------

function s=quote_linearsys(s)
  s=linear_system(s.A',s.C',s.B',s.D',s.X0,dom=s.dom,sample=s.dt);
endfunction

// /.
//---------------

function [s]=sld_linearsys_linearsys(s1,s2)
// s = s1 "fedback" with s2
  [s1,s2]=sysconv(s1,s2);
  ex= s2(5)*s1(5);
  e12= inv(eye(size(ex))+ex);
  e21=s1(5)*e12*s2(5); 
  e21= eye(size(e21))-e21;
  B1=s2.B*e21;
  e12=s1.B*e12
  a=[s1.A-e12*s2(5)*s1.C, -e12*s2.C ; B1*s1.C , s2.A- B1*s1(5)*s2.C]
  s=linear_system(a,[e12;B1*s1(5)],e21*[s1.C -s1(5)*s2.C],...
		  e21*s1(5),[s1.X0;s2.X0],dom=s1.dom);
endfunction

function [sr]=sld_linearsys_p(s,p)
// sr=(eye+s*p)\s
  sr=s /. linear_system([],[],[],p,[]);
endfunction

function s=sld_linearsys_r(s1,s2)
  s= s1 /. tf2ss(s2);
endfunction

function [s]=sld_linearsys_m(s1,s2)
// s=s1/.s2 
// 
  [A,B,C,D]=abcd(s1)
  e12= s2 * D;
  e12=1/(eye(size(e12))+ e12);
  e21=D*e12*s2 ;
  e21=eye(size(e21)) -e21;
  e12=B*e12
  a= A-e12*s2*C
  s=linear_system(A-e12*s2*C,e12,e21*C,e21*D,s1.X0,dom=s1.dom)
endfunction

function [sr]=sld_p_linearsys(p,s)
//  feedback sr=(eye+p*s)\p
// p : polynomial matrix
// s : state-space syslin list
  sr=linear_system([],[],[],p,[],[])/.s
endfunction

function s=sld_r_linearsys(s1,s2)
// s= s1/.s2
  s= tf2ss(s1)/.s2;
endfunction

function s=sld_m_linearsys(d1,s2)
// s=d1/.s2
  e12= s2.D*d1; 
  e12=1/(eye(size(e12))+e12);
  e21=d1*e12*s2.D;e21=eye(size(e21))- e21;
  B1=s2.B*e21
  s=linear_system(s2.A- B1*d1*s2.C, B1*d1,...
		  -e21*d1*s2.C,e21*d1,s2.X0,dom=s2.dom)
endfunction

function h=sld_p_p(h1,h2)
// h=(I+h1*h2)\h1 with h1 h2 polynomial matrices
  [m1,n1]=size(h1)
  [m2,n2]=size(h2)
  if abs(n1-m2)+abs(m1-n2)<>0 then error('inconsistent dimensions'),end
  h=inv(eye(m1,m1)+h1*h2)*h1
endfunction

function h=sld_p_r(h1,h2)
// (I+h1*h2)\h1. 
  [m1,n1]=size(h1)
  [m2,n2]=size(h2.num)
  if abs(n1-m2)+abs(m1-n2)<>0 then error('inconsistent dimensions'),end
  h=inv(eye(m1,m1)+h1*h2)*h1
endfunction

function h=sld_p_m(p,s)
  h=p/(1+p*s)
endfunction

function h=sld_r_p(h1,h2)
  [m1,n1]=size(h1.num);
  [m2,n2]=size(h2)
  if abs(n1-m2)+abs(m1-n2)<>0 then error('inconsistent dimensions'),end
  h=inv(eye(m1,m1)+h1*h2)*h1;
endfunction

function h=sld_r_r(h1,h2)
  [h1,h2]=sysconv(h1,h2),
  [m1,n1]=size(h1.num);
  [m2,n2]=size(h2.num);
  if abs(n1-m2)+abs(m1-n2)<>0 then error('inconsistent dimensions'),end
  if m1*n1==1 then
    h=p2r(h1.num*h2.den,h1.num*h2.num+h1.den*h2.den); 
    h.dom = h1.dom; 
    h.dt = h1.dt;
  else
    h=inv(eye(m1,m1)+h1*h2)*h1
  end
endfunction

function h=sld_r_m(h1,h2)
  [m1,n1]=size(h1.num)
  [m2,n2]=size(h2)
  if abs(n1-m2)+abs(m1-n2)<>0 then error('inconsistent dimensions'),end
  if m1*n1==1 then
    h=p2r(h1.num,h1.num*h2+h1.den);
  else
    h=inv(eye(m1,m1)+h1*h2)*h1
  end
endfunction

function h=sld_m_p(s,p)
  h=s/(1+s*p)
endfunction

function h=sld_m_r(h1,h2)
  [m1,n1]=size(h1)
  [m2,n2]=size(h2.num);
  if abs(n1-m2)+abs(m1-n2)<>0 then error('inconsistent dimensions'),end
  if m1*n1==1 then
    h=p2r(h1*h2.den,h1*h2.num+h2.den);
  else
    h=inv(eye(m1,m1)+h1*h2)*h1
  end
endfunction

function [h]=sld_m_m(h1,h2)
  [m1,n1]=size(h1)
  [m2,n2]=size(h2)
  if abs(n1-m2)+abs(m1-n2)<>0 then error('inconsistent dimensions'),end
  h=inv(eye(m1,m1)+h1*h2)*h1
endfunction
