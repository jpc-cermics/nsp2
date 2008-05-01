// -*- Mode: scilab -*- 
// test for grand 

prec = 1
function y=norm(x) ; y=max(abs(x));endfunction ;

//FIXME : cdft is not tested 
//FIXME : grand mn is not tested 
//FIXME : grand mul is not tested 
//FIXME : add circular tests for cdf*
//test for beta random deviate 
  
N=10000;A=1;B=3;
Rdev=grand(1,N,'bet',A,B);
RdevS=sort(Rdev);RdevS=RdevS($:-1:1)';
PS=(1:N)'/N;
//plot2d(RdevS,PS);
// theorical result 
[P]=cdfbet("PQ",RdevS,1-RdevS,A*ones_deprecated(RdevS),B*ones_deprecated(RdevS));
//plot2d(RdevS,P,style=2);
if norm(P-PS) > prec then pause,end

// test for f 

N=10000;A=1;B=3;
Rdev=grand(1,N,'f',A,B); 
RdevS=sort(Rdev);RdevS=RdevS($:-1:1)';
PS=(1:N)'/N;
//plot2d(RdevS,PS);
// theorical result 
[P]=cdff("PQ",RdevS,A*ones_deprecated(RdevS),B*ones_deprecated(RdevS));
//plot2d(RdevS,P,2,"000")
if norm(P-PS) > prec then pause,end

// test for mul
 

// test for gamma 

N=10000;A=1;B=3;
Rdev=grand(1,N,'gam',A,B); 
RdevS=sort(Rdev);RdevS=RdevS($:-1:1)';
PS=(1:N)'/N;
//plot2d(RdevS,PS);
// theorical result 
[P]=cdfgam("PQ",RdevS,A*ones_deprecated(RdevS),B*ones_deprecated(RdevS));
//plot2d(RdevS,P,2,"000")
if norm(P-PS) > prec then pause,end

// test for nor 

N=10000;A=1;B=2;
Rdev=grand(1,N,'nor',A,B); 
RdevS=sort(Rdev);RdevS=RdevS($:-1:1)';
PS=(1:N)'/N;
//plot2d(RdevS,PS);
// theorical result 
[P]=cdfnor("PQ",RdevS,A*ones_deprecated(RdevS),B*ones_deprecated(RdevS));
//plot2d(RdevS,P,2,"000")
if norm(P-PS) > prec then pause,end

// test for unf 

N=10000;A=1;B=2;
Rdev=grand(1,N,'unf',A,B); 
RdevS=sort(Rdev);RdevS=RdevS($:-1:1)';
PS=(1:N)'/N;
//plot2d(RdevS,PS);
// theorical result 
[P]=RdevS-A;
//plot2d(RdevS,P,2,"000")
if norm(P-PS) > prec then pause,end

// test for uin ( a finir ) 

N=10000;A=1;B=10;
Rdev=grand(1,N,'uin',A,B); 
RdevS=sort(Rdev);RdevS=RdevS($:-1:1)';
PS=(1:N)'/N;
//plot2d(RdevS,PS);
// theorical result 
[P]=RdevS-A;
//plot2d(RdevS,P,2,"000")
//need an other test XXX
//if norm(P-PS) > prec then pause,end

// test for lgi 

N=10000;
Rdev=grand(1,N,'lgi');
RdevS=sort(Rdev);RdevS=RdevS($:-1:1)';
PS=(1:N)'/N;
//plot2d(RdevS,PS);
// theorical result 
[P]=RdevS-A;
//plot2d(RdevS,P,2,"000")
//XXXX need an other test 
//if norm(P-PS) > prec then pause,end


// test fo perm 

N=1000;
Mat=grand(N,'prm',[1:10]');
if sum([1:10]')/10 - sum(Mat,'c')/N > 0 then pause;end 

// test for nbn 

N=10000;A=5;B=0.7;
Rdev=grand(1,N,'nbn',A,B); 
RdevS=sort(Rdev);RdevS=RdevS($:-1:1)';
PS=(1:N)'/N;
//plot2d(RdevS,PS);
// theorical result 
[P]=cdfnbn("PQ",RdevS,A*ones_deprecated(RdevS),B*ones_deprecated(RdevS),(1-B)*ones_deprecated(RdevS));
//plot2d(RdevS,P,2,"000")
//XXXX need an other test 
//if norm(P-PS) > prec then pause,end


// test for bin 

N=10000;A=5;B=0.7;
Rdev=grand(1,N,'bin',A,B); 
RdevS=sort(Rdev);RdevS=RdevS($:-1:1)';
PS=(1:N)'/N;
//plot2d(RdevS,PS);
// theorical result 
[P]=cdfbin("PQ",RdevS,A*ones_deprecated(RdevS),B*ones_deprecated(RdevS),(1-B)*ones_deprecated(RdevS));
//plot2d(RdevS,P,2,"000")
//XXX need to change test 
//if norm(P-PS) > prec then pause,end

// test for mn 

// test for 'def'

N=10000;
Rdev=grand(1,N,'def');
RdevS=sort(Rdev);RdevS=RdevS($:-1:1)';
PS=(1:N)'/N;
//plot2d(RdevS,PS);
// theorical result 
[P]=RdevS;
//plot2d(RdevS,P,2,"000")
if norm(P-PS) > prec then pause,end

// test for nch or chn 

N=10000;A=5;B=4;
Rdev=grand(1,N,'nch',A,B); 
RdevS=sort(Rdev);RdevS=RdevS($:-1:1)';
PS=(1:N)'/N;
//plot2d(RdevS,PS);
// theorical result 
[P]=cdfchn("PQ",RdevS,A*ones_deprecated(RdevS),B*ones_deprecated(RdevS));
//plot2d(RdevS,P,2,"000")
if norm(P-PS) > prec then pause,end

// test for nf or fnc

N=10000;A=5;B=4;C=10;
Rdev=grand(1,N,'nf',A,B,C); 
RdevS=sort(Rdev);RdevS=RdevS($:-1:1)';
PS=(1:N)'/N;
//plot2d(RdevS,PS);
// theorical result 
[P]=cdffnc("PQ",RdevS,A*ones_deprecated(RdevS),B*ones_deprecated(RdevS),C*ones_deprecated(RdevS));
//plot2d(RdevS,P,2,"000")
if norm(P-PS) > prec then pause,end

// test for chi 

N=10000;A=5;
Rdev=grand(1,N,'chi',A);
RdevS=sort(Rdev);RdevS=RdevS($:-1:1)';
PS=(1:N)'/N;
//plot2d(RdevS,PS);
// theorical result 
[P]=cdfchi("PQ",RdevS,A*ones_deprecated(RdevS));
//plot2d(RdevS,P,2,"000")
if norm(P-PS) > prec then pause,end

// test for poi 

N=10000;A=50;
Rdev=grand(1,N,'poi',A);
RdevS=sort(Rdev);RdevS=RdevS($:-1:1)';
PS=(1:N)'/N;
//plot2d(RdevS,PS);
// theorical result 
[P]=cdfpoi("PQ",RdevS,A*ones_deprecated(RdevS));
//plot2d(RdevS,P,2,"000")
// idem need an other test P is piecewize linear and PS 
// linear 
//if norm(P-PS) > prec then pause,end


// test for exp 

N=10000;A=2;
Rdev=grand(1,N,'exp',A);
RdevS=sort(Rdev);RdevS=RdevS($:-1:1)';
PS=(1:N)'/N;
//plot2d(RdevS,PS);
// theorical result 
P=1-exp(-RdevS/A);
//plot2d(RdevS,P,2,"000")
if norm(P-PS) > prec then pause,end

// test for geom 

N=10000;p=0.5;
Rdev=grand(1,N,'geom',p);
[ind,occ,x_info]=bsearch(Rdev,1:20,match='v');
if norm(occ/N - p*(1-p).^(0:19)) > prec then pause,end

// test for markov

N=10000;A=[0.1,0.3,0.6;0.2,0.3,0.5;0.7,0.2,0.1];
Rdev=grand(N,'markov',A,1);
for i=1:3
  [ind,occ,x_info]=bsearch(Rdev(1,:),1:3,match='v');
  if norm(occ/N - A(1,:)) > prec then pause,end
end 


// test de cdfnor 

v=[-5:0.1:5];
[P,Q]=cdfnor("PQ",v,0*ones_deprecated(v),1*ones_deprecated(v));
v1=cdfnor("X",0*ones_deprecated(v),1*ones_deprecated(v),P,Q);
if max(abs(v-v1)) > 1.e-14 then pause,end
M=cdfnor("Mean",1*ones_deprecated(v),P,Q,v);
if max(abs(M)) > 1.e-14 then pause,end
St=cdfnor("Std",P,Q,v,0*ones_deprecated(v));
// result can be false near P=0.5
if max(abs(St-1)) > 0.3 then pause,end

// test de cdfgam

v=[0:0.01:3];
[P,Q]=cdfgam("PQ",v,0.1*ones_deprecated(v),0.3*ones_deprecated(v));
v1=cdfgam("X",0.1*ones_deprecated(v),0.3*ones_deprecated(v),P,Q);
if max(abs(v-v1)) > 1.e-14 then pause,end
Shape=cdfgam("Shape",0.3*ones_deprecated(v),P,Q,v);
// Shape est faux si P==0;
if max(abs(Shape(2:$)-0.1)) > 1.e-8 then pause,end
Scale=cdfgam("Scale",P,Q,v,0.1*ones_deprecated(v));
if max(abs(Scale(2:$)-0.3)) > 1.e-8 then pause,end
