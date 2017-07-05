function [r,q]=monodiv_p(a,alpha)
  // Copyright  2010-2017 Jean-Philippe Chancelier Cermics/Enpc
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
  // divide polynomial a by (x-alpha). faster than pdiv and
  // could be accelerated by the nsp band solver.
  //
  ca=a.coeffs{1};
  na=size(ca,'*');
  b(na-1)=ca(na);
  for i=na-2: -1: 1 do
    b(i)=alpha*b(i+1)+ca(i+1);
  end
  gamma=ca(1)+alpha*b(1)
  q=m2p(b,var = a.get_var[]);
  r=gamma;
endfunction

function [r,q]=pdiv_soft_p_p(a,b)
  // Copyright  2010-2017 Jean-Philippe Chancelier Cermics/Enpc
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
  // polynomial division a=q*b + r
  // a and b are two polynoms
  // should be extended to term to term

  q=[];
  cb=b.coeffs{1};
  nb=size(cb,'*');
  while %t do
    ca=a.coeffs{1};
    na=size(ca,'*');
    if (na<nb) then r=a;break;end
    c=ca($)/cb($)
    q(na-nb+1)=c;
    b1=c*b;
    b1.shift[na-nb];
    a=a-b1;
    if a.degree[]==b1.degree[] then
      // need to set to zero the leading element
      ca=a.coeffs{1};
      if size(ca,'*')==1 then r=a;break;end
      ca($)=0;
      a=m2p(ca,var = a.get_var[]);
    end
  end
  if isempty(q) then q=0;end
  q=m2p(q,var = a.get_var[]);
endfunction

function [g,Rp,sgn]=euclide_p_p(a,b,eps = 1.E-6,monic = %f)
  // epsilon euclid method
  // See Paola Boito Thesis Chap 3 or
  // original paper of Hribernig and Stetter.
  // Detection and validation of clusters of
  // Polynomial Zeros (J. Symbolic Computation (1997) 24 667-682.
  // the default eps is set to 1.e-8
  //
  da=a.degree[];
  db=b.degree[];
  if da>=db then
    f1=a;f2=b;ind=[1,2];
  else
    f1=b;f2=a;ind=[2,1];
  end
  vname=a.get_var[];
  M=m2p(eye(2,2),var = vname,dim = ".");
  detM=1;
  v=[f1;f2];
  // we start with [f1;f2]= M*v
  // and along the iterations we will have
  // [f1;f2]= Mn*vn
  while %t do
    // update the matrix M
    // note that M is unimodular
    // and detM is the value of det(M)
    [r,q]=pdiv_soft(v(1),v(2));
    M=[q*M(1,1)+M(1,2),M(1,1);q*M(2,1)+M(2,2),M(2,1)];
    detM=-detM;
    v=[v(2);r];
    // at this step we should have M*v == [f1;f2]
    // first way to stop
    [r1,q1]=pdiv_soft(f1,v(1));
    [r2,q2]=pdiv_soft(f2,v(1));
    if norm(r1,1)<eps&&norm(r2,1)<eps then
      break;
    end
    // [f1;f2]= M*v is an
    if norm(M(1,2)*v(2),1)<eps&&norm(M(2,2)*v(2),1)<eps then
      break;
    end
    if v(1).degree[]==0&&v(2).degree[]==0 then
      break;
    end
  end
  // when we break v(1) is the gcd and
  // [f1;f2]= M*v and v=[gcd;0]
  g=v(1);
  M=M(ind,:);
  // M*[g;0] -[a;b] should be small
  // compute R = M^(-1) and Rp =R'
  // Rp is such that
  // [a,b]*Rp = [g,0]
  Rp=[M(2,2),-M(2,1);-M(1,2),M(1,1)]*detM;
  sgn=detM;
  // we have
  // f = [ Rp(2,2), - Rp(1,2)]*detM;
  // a = f(1)*g
  // b = f(2)*g
endfunction

if %f then
  // a few basic tests
  // test
  p=m2p([1,4,6,4,1]);//(1+x)^4
  q=m2p([0,0,1,1]);// (1+x)*x^2
  [g,Rp,sgn]=euclide(p,q);
  if max(norm([p,q]*Rp-[g,0]))>100*%eps then pause ;end
  // recover the factors from Rp
  f=[Rp(2,2),-Rp(1,2)]*sgn;
  if norm(p-f(1)*g)>10*%eps then pause ;end
  if norm(q-f(2)*g)>10*%eps then pause ;end
  // compute the lcm
  pqlcm=f(1)*f(2)*g;
  x=m2p([0,1]);
  pqlcm.normalize[];
  if norm(pqlcm-(1+x)^4*x^2)>100*%eps then pause ;end

  // test
  x=poly(0);
  pp1=[x*(1+x)^4;x^3*(1+x)^3;x^3*(1+x)^2;x^3*(1+x)^2;x^4*(1+x)^2;x^4*(1+x);x^3*(1+x)];
  g=pp1(1);
  for i=1:size(pp1,'*') do g=euclide(g,pp1(i),monic = %t);end
  g.normalize[];
  if norm(g-x*(1+x))>100*%eps then pause ;end

  // test
  x=poly(0);
  qq=[3*(x^2-4);3*x*(x^2-4)];
  g=qq(1);
  for i=1:size(qq,'*') do g=euclide(g,qq(i),monic = %t);end
  g.normalize[];
  if norm(g-(x^2-4))>100*%eps then pause ;end
end


if %f then
  // test the influence of eps
  function gcd_euclide1_test()
    // test gcd computations
    // more tests
    x=poly(0);
    p1=(1+x);p2=(2+x);p3=(3+x);p4=(x);
    n=1000;T=ones(1,n)>0;N=zeros(1,n);
    for i=1:n do
      cp=grand(1,4,'uin',0,5);
      p=p1^cp(1)*p2^cp(2)*p3^cp(3)*p4^cp(4);
      cq=grand(1,4,'uin',0,5);
      q=p1^cq(1)*p2^cq(2)*p3^cq(3)*p4^cq(4);
      cpq=min(cq,cp);
      gcpq1=p1^cpq(1)*p2^cpq(2)*p3^cpq(3)*p4^cpq(4);
      // [g,p,q,res] = gcd_jpc(p,q);
      [g,b1]=euclide(p/norm(p,2),q/norm(q,2));
      g.normalize[];
      N(i)=norm(g-gcpq1);
      if N(i)>1.E-5 then
	T(i)=%f;
      end
    end
    I=find(N==0);N(I)=1.E-16;
    xclear();
    Nok=size(find(T),'*');
    plot2d(1:n,log(N)/log(10))
    xtitle(sprintf('Number of correct tests %d/%d',Nok,n));
  endfunction

  function gcd_euclide2_test()
    x=poly(0);
    r=-5:4;
    n=10000;
    veps=10 .^(-[0:14]);
    for k=1:size(veps,'*') do
      T=ones(1,n);
      for i=1:n do
	cp=grand(1,10,'uin',0,1);
	p=m2p(1);for j=1:10 do p=p*(r(j)+x)^cp(j);end
	cq=grand(1,10,'uin',0,1);
	q=m2p(1);for j=1:10 do q=q*(r(j)+x)^cq(j);end
	eps=veps(k);
	[gcpq,b1]=euclide(p/norm(p,1),q/norm(q,1),eps = eps);
	cpq=min(cq,cp);
	pq=m2p(1);for j=1:10 do pq=pq*(r(j)+x)^cpq(j);end
	gcpq1=pq;
	gcpq.normalize[];
	T(i)=norm(gcpq1-gcpq,1);
      end
      Ts=sort(T,'g','i');
      res(k)=Ts(8000);
    end
  endfunction
end
