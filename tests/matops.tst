// Copyright Enpc 
// Test for real-Matrix

deff('[y]=test(x1,x2)','y=or(x1<>x2)');

deff('[y]=testN(x1,x2,z)','y=norm(x1-x2)>z');

nr=10
nc=20;
M=[nr,nc,0,1,1 ,nr];
N=[nc,nr,0,1,nc,1 ];

//----- test of size (+rand)

for i=1:6, m=M(i);n=N(i);
  a=rand(m,n,'uniform');
  if test(size(a),[m,n]) then pause,end
  if test(size(a+[]),[m,n]) then pause,end
  if test(size(a,'*'),m*n) then pause,end
  if test(size(a,'r'),m) then pause,end
  if test(size(a,'c'),n) then pause,end
end 

//----- test of eye

for i=1:6, m=M(i);n=N(i);
  a=rand(m,n,'uniform');
  b=0*a;for j=1:mini(m,n), b(j,j)=1;end
  if test(eye(m,n),b) then pause,end
  if test(eye(a),b) then pause,end
  if test(eye(a+[]),b) then pause,end
end 

//----- test of ones 

deff('[a]=Mones(m,n)',['a=rand(m,n,''uniform''),a(a>=0)=1']);

for i=1:6, m=M(i);n=N(i);a=rand(m,n,'uniform');
  if test(ones(m,n),Mones(m,n)) then pause,end
  if test(ones(a),Mones(m,n)) then pause,end
  if test(ones(a+[]),Mones(m,n)) then pause,end
end 

//------- cross test eye-ones-diag 

deff('[a]=Meye(m,n)',['if m<n ; a=[diag(ones(1,m)),0*ones(m,n-m)];';
		      'else a=[diag(ones(1,n));0*ones(m-n,n)];end']);

for i=1:6, m=M(i);n=N(i);
  a=rand(m,n,'uniform');
  if test(eye(m,n),Meye(m,n)) then pause,end
  if test(eye(a),Meye(m,n)) then pause,end
  if test(eye(a+[]),Meye(m,n)) then pause,end
end 

//------- test of diag (creation) 

deff('[a]=Mdiag(d)','m=size(d,''*'');a=0*ones(m,m);for j=1:m, a(j,j)=d(j);end;');

for i=1:6, m=M(i);
  d=1:m
  b=0*ones(m,m);for j=1:m, b(j,j)=d(j);end
  if test(diag(d),b) then pause,end
  if test(diag(d+[]),b) then pause,end
end 

deff('[a]=MFdiag(d,j)',['a=Mdiag(d);m=size(a,''r'');if a==[] then return;end;'
			'if j>=0; a=[0*ones(m+j,j),[a;0*ones(j,m)]]';
			'else a=[0*ones(-j,m-j);[a,0*ones(m,-j)]];end;']);

for i=1:6, m=M(i);
  for j=-2:2
  d=1:m;
    if test(diag(d,j),MFdiag(d,j)) then pause,end
    if test(diag(d+[],j),MFdiag(d,j)) then pause,end
  end 
end 


//-------- test of diag (extraction) 
// diag(a)
Mde=[nr,nc,0];
Nde=[nc,nr,0];
for i=1:3, m=Mde(i);n=Nde(i);
    d=rand(m,n,'uniform');
    b=0*ones(mini(m,n),1);for j=1:mini(m,n), b(j)=d(j,j);end
    if test(diag(d),b) then pause,end
    if test(diag(d+[]),b) then pause,end
end 
// diag(a,j) 

deff('[d]=DiagE(a,j)',['if j>0 ; d=diag(a(1:$-j,j+1:$));';
			'elseif j==0; d=diag(a);';
			'else d=diag(a(-j+1:$,1:$+j));end']);

for i=1:3, m=Mde(i);n=Nde(i);
    for j=-(m+1):m+1;
     d=rand(m,n,'uniform');
     b=0*ones(mini(m,n),1);for j=1:mini(m,n), b(j)=d(j,j);end
     if test(diag(d),b) then pause,end
     if test(diag(d+[]),b) then pause,end
    end 
end 

//----------test of  triu

deff('[T]=Triu(a,k)',['[m,n]=size(a);T=a;';
		'if k>=0;for i=1:m;T(i,1:mini(i+k-1,n))=0;end;';
		'else if -k+1<=m; for i=(-k+1):m; T(i,1:mini(i+k-1,n))=0;end;end;end']);



for i=1:6, m=M(i);n=N(i);
    for j=-(m+1):m+1;
     A=int(10*rand(m,n,'uniform'));
     if test(triu(A,j),Triu(A,j)) then pause,end
     if test(triu(A+[],j),Triu(A,j)) then pause,end
    end 
end 

//----------test of  tril 

deff('[T]=Tril(a,k)',['[m,n]=size(a);T=a;';
		'if k>=0;for i=1:m;for j=i+k+1:n,T(i,j)=0;end;end;';
		'else for i=1:m;for j=maxi(i+k+1,1):n,T(i,j)=0;end;end;end']);

for i=1:6, m=M(i);n=N(i);
    for j=-(m+1):m+1;
     A=int(10*rand(m,n,'uniform'));
     if test(tril(A,j),Tril(A,j)) then pause,end
     if test(tril(A+[],j),Tril(A,j)) then pause,end
    end 
end 

//--------- test of abs  

deff('[b]=Abs(a)',['if a==[] then b=[];else b=maxi(a,0)+maxi(-a,0);end;']);

for i=1:6, m=M(i);n=N(i);
     A=rand(m,n,'normal');
     if test(abs(A),Abs(A)) then pause,end
     if test(abs(A+[]),Abs(A)) then pause,end
end 

//--------- test of real 

for i=1:6, m=M(i);n=N(i);
     A=rand(m,n,'normal');
     if test(real(A),A) then pause,end
     if test(real(A+[]),A) then pause,end
end 

//--------- test of imag

for i=1:6, m=M(i);n=N(i);
     A=rand(m,n,'normal');
     if test(imag(A),0*A) then pause,end
     if test(imag(A+[]),0*A) then pause,end
end 

//--------- test of conj 

for i=1:6, m=M(i);n=N(i);
     A=rand(m,n,'normal');
     if test(conj(A),A) then pause,end
     if test(conj(A+[]),A) then pause,end
end 



//--------- test of int 

for i=1:6, m=M(i);n=N(i);
     A=int(10*rand(m,n,'uniform'));
     U=rand(m,n,'uniform');
     B=A+U;
     if test(int(B),A) then pause,end
     if test(int(B+[]),A) then pause,end
     B=-A-U;
     if test(int(B),-A) then pause,end
     if test(int(B+[]),-A) then pause,end
end 

//----------test of round

//limit 
m=10;
if test(round(0:m+0.5),0:m) then pause,end
if test(round(-(1:m)+0.5),-(1:m)+1) then pause,end

//generic 
for i=1:6, m=M(i);n=N(i);
     A=int(10*rand(m,n,'uniform'));
     U=rand(m,n,'uniform')/2;
     B=A+U;
     if test(round(B),A) then pause,end
     if test(round(B+[]),A) then pause,end
     if A<>[] then 
       B=A+U+0.5;
       if test(round(B),A+1) then pause,end
       if test(round(B+[]),A+1) then pause,end
     end
end 

//----------test of ceil 

deff('[y]=Mceil(x)','if x==[] then y=[];else y=int(x)+1;y(x<0)=y(x<0)-1;end');

for i=1:6, m=M(i);n=N(i);
     A=10*rand(m,n,'normal');
     if test(ceil(A),Mceil(A)) then pause,end
     if test(ceil(A+[]),Mceil(A)) then pause,end
end 

//----------test of floor

deff('[y]=Mfloor(x)','if x==[] then y=[];else y=int(x);y(x<0)=y(x<0)-1;end');

for i=1:6, m=M(i);n=N(i);
     A=10*rand(m,n,'normal');
     if test(floor(A),Mfloor(A)) then pause,end
     if test(floor(A+[]),Mfloor(A)) then pause,end
end 

//----------test of sign

deff('[y]=Msign(x)','z=x(x<>0);z=z./abs(z);y=x;y(x<>0)=z');

if sign(0)<>0 then pause,end

for i=1:6, m=M(i);n=N(i);
     A=rand(m,n,'normal');
     if test(sign(A),Msign(A)) then pause,end
     if test(sign(A+[]),Msign(A)) then pause,end
end 

//----------test of sum 

deff('[y]=Msum(x,j)',['[m,n]=size(x);if j==''c'', y= x*ones(n,1);';
		      'elseif j==''r'',y=ones(1,m)*x;';
		      'else y= ones(1,m)*x*ones(n,1);end']);

deff('[y]=MsumG(x,j)',['if and([x==[],j==''f'']) then y=0;else y=Msum(x,j);end']);

for i=1:6, m=M(i);n=N(i);
     A=10*rand(m,n,'normal');
     if test(sum(A),MsumG(A,'f')) then pause,end
     if test(sum(A+[]),MsumG(A,'f')) then pause,end
     if test(sum(A,'c'),MsumG(A,'c')) then pause,end
     if test(sum(A,2),MsumG(A,'c')) then pause,end
     if test(sum(A+[],'c'),MsumG(A,'c')) then pause,end     
     if test(sum(A,'r'),MsumG(A,'r')) then pause,end
     if test(sum(A,1),MsumG(A,'r')) then pause,end
     if test(sum(A+[],'r'),MsumG(A,'r')) then pause,end
end 


//----------test of prod

deff('[p]=Mprod(x,j)',['[m,n]=size(x);';
    'if j==''c'' then ;p=0*ones(m,1);for i=1:m,p(i)=det(diag(x(i,:)));end;';
    'elseif j==''r'' then p=0*ones(1,n);for i=1:n,p(i)=det(diag(x(:,i)));end;'
    'else p= Mprod(Mprod(x,''r''),''c'');end;']);

deff('[y]=MprodG(x,j)',['if and([x==[],j==''f'']) then y=1;else y=Mprod(x,j);end']);

nr=3;
nc=4;
Mp=[nr,nc,0,1,1 ,nr];
Np=[nc,nr,0,1,nc,1 ];

for i=1:6, m=Mp(i);n=Np(i);
     A=int(10*rand(m,n,'uniform'));
     if testN(prod(A),MprodG(A,'f'),0.1) then pause,end
     if testN(prod(A+[]),MprodG(A,'f'),0.1) then pause,end
     if testN(prod(A,'c'),MprodG(A,'c'),0.1) then pause,end
     if testN(prod(A,2),MprodG(A,'c'),0.1) then pause,end
     if testN(prod(A+[],'c'),MprodG(A,'c'),0.1) then pause,end     
     if testN(prod(A,'r'),MprodG(A,'r'),0.1) then pause,end
     if testN(prod(A,1),MprodG(A,'r'),0.1) then pause,end
     if testN(prod(A+[],'r'),MprodG(A,'r'),0.1) then pause,end
end 

//----------test of cumsum

deff('[y]=Mcumsum(x,j)',['[m,n]=size(x);'
  'if j==''c'', y=0*x; for i=1:n; y(:,i)= x(:,1:i)*ones(i,1);end;';
  'elseif j==''r'',y=Mcumsum(x'',''c'')'';';
  'else y=x(:);y=Mcumsum(y,''r'');y=matrix(y,m,n);end']);

deff('[y]=McumsumG(x,j)',['if x==[] then y=[];else y=Mcumsum(x,j);end']);

for i=1:6, m=M(i);n=N(i);
     A=10*rand(m,n,'normal');
     if test(cumsum(A),McumsumG(A,'f')) then pause,end
     if test(cumsum(A+[]),McumsumG(A,'f')) then pause,end
     if test(cumsum(A,'c'),McumsumG(A,'c')) then pause,end
     if test(cumsum(A,2),McumsumG(A,'c')) then pause,end
     if test(cumsum(A+[],'c'),McumsumG(A,'c')) then pause,end     
     if test(cumsum(A,'r'),McumsumG(A,'r')) then pause,end
     if test(cumsum(A,1),McumsumG(A,'r')) then pause,end
     if test(cumsum(A+[],'r'),McumsumG(A,'r')) then pause,end
end 

//----------test of cumprof AFINIR 

//cumprod
a=[1 2;-3 4;5,-6];
if or(cumprod(a)<>[1, -30;-3, -120;-15 720]) then pause,end
if or(cumprod(a+0)<> [1, -30;-3, -120;-15 720]) then pause,end
if or(cumprod(a,'r')<>[1 2;-3, 8;-15 ,-48]) then pause,end
if or(cumprod(a+0,'r')<>[1 2;-3, 8;-15, -48]) then pause,end
n='r';
if or(cumprod(a,n)<>[1 2;-3 8;-15, -48]) then pause,end
if or(cumprod(a+0,n)<>[1 2;-3 8;-15 ,-48]) then pause,end
if or(cumprod(a,'r')<>[1 2;-3 8;-15 ,-48]) then pause,end
if or(cumprod(a+0,'r')<>[1 2;-3 8;-15, -48]) then pause,end
n='r';
if or(cumprod(a,n)<>[1 2;-3 8;-15, -48]) then pause,end
if or(cumprod(a+0,n)<>[1 2;-3 8;-15, -48]) then pause,end

if or(cumprod(a,'c')<>[1 2;-3 ,-12;5, -30]) then pause,end
if or(cumprod(a+0,'c')<>[1 2;-3, -12;5, -30]) then pause,end
n='c';
if or(cumprod(a,n)<>[1 2;-3, -12;5, -30]) then pause,end
if or(cumprod(a+0,n)<>[1 2;-3 ,-12;5, -30]) then pause,end
if or(cumprod(a,'c')<>[1 2;-3, -12;5, -30]) then pause,end
if or(cumprod(a+0,'c')<>[1 2;-3, -12;5, -30]) then pause,end
n='c';
if or(cumprod(a,n)<>[1 2;-3, -12;5 ,-30]) then pause,end
if or(cumprod(a+0,n)<>[1 2;-3, -12;5 ,-30]) then pause,end

a=[1 2;-3 4;5,-6]+0*%i;
if cumprod(a)<>[1, -30;-3, -120;-15 720]+0*%i then pause,end
if cumprod(a+0)<>[1, -30;-3, -120;-15 720]+0*%i then pause,end
if or(cumprod(a,'r')<>[1 2;-3 8;-15, -48]+0*%i) then pause,end
if or(cumprod(a+0,'r')<>[1 2;-3 8;-15, -48]+0*%i) then pause,end
n='r';
if or(cumprod(a,n)<>[1 2;-3 8;-15, -48]+0*%i) then pause,end
if or(cumprod(a+0,n)<>[1 2;-3 8;-15, -48]+0*%i) then pause,end
if or(cumprod(a,'r')<>[1 2;-3 8;-15, -48]+0*%i) then pause,end
if or(cumprod(a+0,'r')<>[1 2;-3 8;-15, -48]+0*%i) then pause,end
n='r';
if or(cumprod(a,n)<>[1 2;-3 8;-15, -48]+0*%i) then pause,end
if or(cumprod(a+0,n)<>[1 2;-3 8;-15 ,-48]+0*%i) then pause,end

if or(cumprod(a,'c')<>[1 2;-3, -12;5, -30]+0*%i) then pause,end
if or(cumprod(a+0,'c')<>[1 2;-3, -12;5, -30]+0*%i) then pause,end
n='c';
if or(cumprod(a,n)<>[1 2;-3, -12;5, -30]+0*%i) then pause,end
if or(cumprod(a+0,n)<>[1 2;-3, -12;5, -30]+0*%i) then pause,end
if or(cumprod(a,'c')<>[1 2;-3, -12;5 ,-30]+0*%i) then pause,end
if or(cumprod(a+0,'c')<>[1 2;-3, -12;5, -30]+0*%i) then pause,end
n='c';
if or(cumprod(a,n)<>[1 2;-3, -12;5, -30]+0*%i) then pause,end
if or(cumprod(a+0,n)<>[1 2;-3, -12;5 ,-30]+0*%i) then pause,end

a=[];
if cumprod(a)<>[] then pause,end
if cumprod([])<>[] then pause,end
if cumprod(a,'r')<>[] then pause,end
if cumprod([],'r')<>[] then pause,end
n='r';
if cumprod(a,n)<>[] then pause,end
if cumprod([],n)<>[] then pause,end
if cumprod(a,'r')<>[] then pause,end
if cumprod([],'r')<>[] then pause,end
n='r';
if cumprod(a,n)<>[] then pause,end
if cumprod([],n)<>[] then pause,end

if cumprod(a,'c')<>[] then pause,end
if cumprod([],'c')<>[] then pause,end
n='c';
if cumprod(a,n)<>[] then pause,end
if cumprod([],n)<>[] then pause,end
if cumprod(a,'c')<>[] then pause,end
if cumprod([],'c')<>[] then pause,end
n='c';
if cumprod(a,n)<>[] then pause,end
if cumprod([],n)<>[] then pause,end

a=sparse([1 2;-3 4;5,-6]);
//if cumprod(a)<> [1 -30;-3 -120;-15 720] then pause,end
//if cumprod(a+0*a)<> [1 -30;-3 -120;-15 720] then pause,end
a=sparse([1 2;-3 4;5,-6]+0*%i);
//if cumprod(a)<>[1 -30;-3 -120;-15 720]+0*%i then pause,end
//if cumprod(a+0*a)<>[1 -30;-3 -120;-15 720]+0*%i then pause,end

//----------test of maxi 

exit 

deff('[y,k]=Mmaxi(x)',['k=1,y=x(1),';
	'for i=1:size(x,''*''),if x(i)>y,y=x(i),k=i;end;end']);

deff('[y,k]=MmaxiG(x,j)',['[m,n]=size(x);';
	'if j==''c'', y=0*ones(m,1),k=y;for i=1:m,[yi,ki]=Mmaxi(x(i,:));'
	'y(i)=yi;k(i)=ki;end;';
	'else  y=0*ones(1,n),k=y;for i=1:n,[yi,ki]=Mmaxi(x(:,i));'
	'y(i)=yi;k(i)=ki;end;end']);

for i=1:6, m=M(i);n=N(i);
     A=10*rand(m,n,'normal');
     if test(maxi(A),Mmaxi(A)) then pause,end
     if test(maxi(A+[]),Mmaxi(A)) then pause,end
     [Am,Km]=maxi(A);[Am1,Km1]=Mmaxi(A);
     if test(Am,Am1) then pause,end
	// A FINIR      if test(Km,Km1) then pause,end
	// 
     if test(maxi(A,'r'),MmaxiG(A,'r')) then pause,end
     if test(maxi(A+[],'r'),MmaxiG(A,'r')) then pause,end
     [Am,Km]=maxi(A,'r');[Am1,Km1]=MmaxiG(A,'r');
     if test(Am,Am1) then pause,end
     if test(Km,Km1) then pause,end
	//
     if test(maxi(A,'c'),MmaxiG(A,'c')) then pause,end
     if test(maxi(A+[],'c'),MmaxiG(A,'c')) then pause,end
     [Am,Km]=maxi(A,'c');[Am1,Km1]=MmaxiG(A,'c');
     if test(Am,Am1) then pause,end
     if test(Km,Km1) then pause,end
end 



//mini
a=[1 2;-3 4;5,-6];
if or(mini(a)<>-6) then pause,end
if or(mini(a+0)<>-6) then pause,end
if or(mini(a,'r')<>[-3,-6]) then pause,end
if or(mini(a+0,'r')<>[-3,-6]) then pause,end
n='r';
if or(mini(a,n)<>[-3,-6]) then pause,end
if or(mini(a+0,n)<>[-3,-6]) then pause,end

if or(mini(a,'c')<>[1;-3;-6]) then pause,end
if or(mini(a+0,'c')<>[1;-3;-6]) then pause,end
n='c';
if or(mini(a,n)<>[1;-3;-6]) then pause,end
if or(mini(a+0,n)<>[1;-3;-6]) then pause,end

//a=[1 2;-3 4;5,-6]+0*%i;
//if mini(a)<>[1 -30;-3 -120;-15 720]+0*%i then pause,end
//if mini(a+0)<>[1 -30;-3 -120;-15 720]+0*%i then pause,end
//if or(mini(a,'r')<>[-3,-6]+0*%i) then pause,end
//if or(mini(a+0,'r')<>[-3,-6]+0*%i) then pause,end
//n='r';
//if or(mini(a,n)<>[-3,-6]+0*%i) then pause,end
//if or(mini(a+0,n)<>[-3,-6]+0*%i) then pause,end

//if or(mini(a,'c')<>[1;-3;-6]+0*%i) then pause,end
//if or(mini(a+0,'c')<>[1;-3;-6]+0*%i) then pause,end
//n='c';
//if or(mini(a,n)<>[1;-3;-6]+0*%i) then pause,end
//if or(mini(a+0,n)<>[1;-3;-6]+0*%i) then pause,end

a=[];
if mini(a)<>[] then pause,end
if mini([])<>[] then pause,end

if mini(a,'r')<>[] then pause,end
if mini([],'r')<>[] then pause,end
n='r';
if mini(a,n)<>[] then pause,end
if mini([],n)<>[] then pause,end

if mini(a,'c')<>[] then pause,end
if mini([],'c')<>[] then pause,end
n='c';
if mini(a,n)<>[] then pause,end
if mini([],n)<>[] then pause,end


a=sparse([1 2;-3 4;5,-6]);
//if mini(a)<>-6 then pause,end
//if mini(a+0*a)<>-6 then pause,end
a=sparse([1 2;-3 4;5,-6]+0*%i);
//if mini(a)<>[1 -30;-3 -120;-15 720]+0*%i then pause,end
//if mini(a+0*a)<>[1 -30;-3 -120;-15 720]+0*%i then pause,end

//sort
a=[5 1 3 2 4]
if or(sort(a)<>[5 4 3 2 1]) then pause,end
if or(sort(a+0)<>[5 4 3 2 1]) then pause,end
[s,k]=sort(a);
if or(k<>[1 5 3 4 2]) then pause,end
if or(s<>[5 4 3 2 1]) then pause,end
[s,k]=sort(a+0);
if or(k<>[1 5 3 4 2]) then pause,end
if or(s<>[5 4 3 2 1]) then pause,end

a=string([5 1 3 2 4])
//if or(sort(a)<>string(1:5)) then pause,end
//if or(sort(string([5 1 3 2 4]))<>string(1:5)) then pause,end
[s,k]=sort(a);
//if or(k<>[2 4 3 5 1]) then pause,end
//if or(s<>string(1:5)) then pause,end
[s,k]=sort(string([5 1 3 2 4]));
//if or(k<>[2 4 3 5 1]) then pause,end
//if or(s<>string(1:5)) then pause,end

a=[]
if sort(a)<>[] then pause,end
[s,k]=sort(a);if s<>[]|k<>[] then pause,end

if sort([])<>[] then pause,end
[s,k]=sort([]);if s<>[]|k<>[] then pause,end

//kron
a=[1 2];b=[3;4];
if or(kron(a,b)<>[3 6;4 8]) then pause,end
if or(kron(a+0,b)<>[3 6;4 8]) then pause,end
if or(kron(a,b+0)<>[3 6;4 8]) then pause,end
if or(kron(a+0,b+0)<>[3 6;4 8]) then pause,end
if kron([],b)<>[] then pause,end
if kron([],b+0)<>[] then pause,end
a=[];
if kron(a,b)<>[] then pause,end
if kron(a,b+0)<>[] then pause,end
a=[1 2];b=[]
if kron(a,b)<>[] then pause,end
if kron(a+0,b)<>[] then pause,end
if kron(a,[])<>[] then pause,end
if kron(a+0,[])<>[] then pause,end
a=[];b=[];
if kron(a,b)<>[] then pause,end
if kron(a,[])<>[] then pause,end
if kron([],b)<>[] then pause,end
if kron([],[])<>[] then pause,end

//matrix
a=[1 2 3 4 5 6];
n=1;m=6;
if or(matrix(a,1,6)<>a) then pause,end
if or(matrix(a,n,6)<>a) then pause,end
if or(matrix(a,1,m)<>a) then pause,end
if or(matrix(a,n,m)<>a) then pause,end
if or(matrix(a+0,1,6)<>a) then pause,end
if or(matrix(a+0,n,6)<>a) then pause,end
if or(matrix(a+0,1,m)<>a) then pause,end
if or(matrix(a+0,n,m)<>a) then pause,end

n=3;m=2; b=[1 4;2 5;3 6];
if or(matrix(a,3,2)<>b) then pause,end
if or(matrix(a,n,2)<>b) then pause,end
if or(matrix(a,3,m)<>b) then pause,end
if or(matrix(a,n,m)<>b) then pause,end
if or(matrix(a+0,3,2)<>b) then pause,end
if or(matrix(a+0,n,2)<>b) then pause,end
if or(matrix(a+0,3,m)<>b) then pause,end
if or(matrix(a+0,n,m)<>b) then pause,end

a=[1+%i 2 3 4 5 6];
n=1;m=6;
if or(matrix(a,1,6)<>a) then pause,end
if or(matrix(a,n,6)<>a) then pause,end
if or(matrix(a,1,m)<>a) then pause,end
if or(matrix(a,n,m)<>a) then pause,end
if or(matrix(a+0,1,6)<>a) then pause,end
if or(matrix(a+0,n,6)<>a) then pause,end
if or(matrix(a+0,1,m)<>a) then pause,end
if or(matrix(a+0,n,m)<>a) then pause,end

n=3;m=2; b=[1+%i 4;2 5;3 6];
if or(matrix(a,3,2)<>b) then pause,end
if or(matrix(a,n,2)<>b) then pause,end
if or(matrix(a,3,m)<>b) then pause,end
if or(matrix(a,n,m)<>b) then pause,end
if or(matrix(a+0,3,2)<>b) then pause,end
if or(matrix(a+0,n,2)<>b) then pause,end
if or(matrix(a+0,3,m)<>b) then pause,end
if or(matrix(a+0,n,m)<>b) then pause,end

a=string([1 2 3 4 5 6]);n=1;m=6;
//AFAIRE if or(matrix(a,1,6)<>a) then pause,end
//AFAIRE if or(matrix(a,n,6)<>a) then pause,end
//AFAIRE if or(matrix(a,1,m)<>a) then pause,end
//AFAIRE if or(matrix(a,n,m)<>a) then pause,end
//AFAIRE if or(matrix(a+a,1,6)<>a+a) then pause,end
//AFAIRE if or(matrix(a+a,n,6)<>a+a) then pause,end
//AFAIRE if or(matrix(a+a,1,m)<>a+a) then pause,end
//AFAIRE if or(matrix(a+a,n,m)<>a+a) then pause,end

n=3;m=2; b=string([1 4;2 5;3 6]);
//AFAIRE if or(matrix(a,3,2)<>b) then pause,end
//AFAIRE if or(matrix(a,n,2)<>b) then pause,end
//AFAIRE if or(matrix(a,3,m)<>b) then pause,end
//AFAIRE if or(matrix(a,n,m)<>b) then pause,end
//AFAIRE if or(matrix(a+a,3,2)<>b+b) then pause,end
//AFAIRE if or(matrix(a+a,n,2)<>b+b) then pause,end
//AFAIRE if or(matrix(a+a,3,m)<>b+b) then pause,end
//AFAIRE if or(matrix(a+a,n,m)<>b+b) then pause,end

//AFAIRE a=[1 2 3 4 5 6]+%s;
//AFAIRE n=1;m=6;
//AFAIRE if or(matrix(a,1,6)<>a) then pause,end
//AFAIRE if or(matrix(a,n,6)<>a) then pause,end
//AFAIRE if or(matrix(a,1,m)<>a) then pause,end
//AFAIRE if or(matrix(a,n,m)<>a) then pause,end
//AFAIRE if or(matrix(a+0,1,6)<>a) then pause,end
//AFAIRE if or(matrix(a+0,n,6)<>a) then pause,end
//AFAIRE if or(matrix(a+0,1,m)<>a) then pause,end
//AFAIRE if or(matrix(a+0,n,m)<>a) then pause,end

//AFAIRE n=3;m=2; b=[1 4;2 5;3 6]+%s;
//AFAIRE if or(matrix(a,3,2)<>b) then pause,end
//AFAIRE if or(matrix(a,n,2)<>b) then pause,end
//AFAIRE if or(matrix(a,3,m)<>b) then pause,end
//AFAIRE if or(matrix(a,n,m)<>b) then pause,end
//AFAIRE if or(matrix(a+0,3,2)<>b) then pause,end
//AFAIRE if or(matrix(a+0,n,2)<>b) then pause,end
//AFAIRE if or(matrix(a+0,3,m)<>b) then pause,end
//AFAIRE if or(matrix(a+0,n,m)<>b) then pause,end
//AFAIRE 
//AFAIRE a=[1+%i 2 3 4 5 6]+%s;
//AFAIRE n=1;m=6;
//AFAIRE if or(matrix(a,1,6)<>a) then pause,end
//AFAIRE if or(matrix(a,n,6)<>a) then pause,end
//AFAIRE if or(matrix(a,1,m)<>a) then pause,end
//AFAIRE if or(matrix(a,n,m)<>a) then pause,end
//AFAIRE if or(matrix(a+0,1,6)<>a) then pause,end
//AFAIRE if or(matrix(a+0,n,6)<>a) then pause,end
//AFAIRE if or(matrix(a+0,1,m)<>a) then pause,end
//AFAIRE if or(matrix(a+0,n,m)<>a) then pause,end
//AFAIRE 
//AFAIRE n=3;m=2; b=[1+%i 4;2 5;3 6]+%s;
//AFAIRE if or(matrix(a,3,2)<>b) then pause,end
//AFAIRE if or(matrix(a,n,2)<>b) then pause,end
//AFAIRE if or(matrix(a,3,m)<>b) then pause,end
//AFAIRE if or(matrix(a,n,m)<>b) then pause,end
//AFAIRE if or(matrix(a+0,3,2)<>b) then pause,end
//AFAIRE if or(matrix(a+0,n,2)<>b) then pause,end
//AFAIRE if or(matrix(a+0,3,m)<>b) then pause,end
//AFAIRE if or(matrix(a+0,n,m)<>b) then pause,end

//AFINIR 
exit


//clean
a=[1 1.d-12 1.d-5 2d8];
b=[1 0 0 2d8];
if or(clean(a)<>b) then pause,end
if or(clean(a+0)<>b) then pause,end
epsa=1.d-10;
if or(clean(a,epsa)<>b) then pause,end
if or(clean(a+0,epsa)<>b) then pause,end
if or(clean(a,epsa+0)<>b) then pause,end
if or(clean(a+0,epsa+0)<>b) then pause,end
epsr=1.d-5;b=[0 0 0 2d8];
if or(clean(a,epsa,epsr)<>b) then pause,end
if or(clean(a+0,epsa,epsr)<>b) then pause,end
if or(clean(a,epsa+0,epsr)<>b) then pause,end
if or(clean(a+0,epsa,epsr+0)<>b) then pause,end
if or(clean(a,epsa+0,epsr)<>b) then pause,end
if or(clean(a+0,epsa,epsr+0)<>b) then pause,end
if or(clean(a,epsa+0,epsr)<>b) then pause,end
if or(clean(a+0,epsa+0,epsr+0)<>b) then pause,end

a=[1+%i 1.d-12 1.d-5 2d8];
b=[1+%i 0 0 2d8];
if or(clean(a)<>b) then pause,end
if or(clean(a+0)<>b) then pause,end
epsa=1.d-10;
if or(clean(a,epsa)<>b) then pause,end
if or(clean(a+0,epsa)<>b) then pause,end
if or(clean(a,epsa+0)<>b) then pause,end
if or(clean(a+0,epsa+0)<>b) then pause,end
epsr=1.d-5;b=[0+0*%i 0 0 2d8];
if or(clean(a,epsa,epsr)<>b) then pause,end
if or(clean(a+0,epsa,epsr)<>b) then pause,end
if or(clean(a,epsa+0,epsr)<>b) then pause,end
if or(clean(a+0,epsa,epsr+0)<>b) then pause,end
if or(clean(a,epsa+0,epsr)<>b) then pause,end
if or(clean(a+0,epsa,epsr+0)<>b) then pause,end
if or(clean(a,epsa+0,epsr)<>b) then pause,end
if or(clean(a+0,epsa+0,epsr+0)<>b) then pause,end




//log and exp
a=[1.2 2.51;-3.4 4.52;5.8,-6.2];
if norm(exp(log(a))-a)>10*%eps then pause,end
if norm(exp(log(a+0))-a)>10*%eps then pause,end
b=log(a);if norm(exp(b)-a)>10*%eps then pause,end
a=[1+%i 2.51;-3.4 4.52;5.8,-6.2];
if norm(exp(log(a))-a)>10*%eps then pause,end
if norm(exp(log(a+0))-a)>10*%eps then pause,end
b=log(a);if norm(exp(b)-a)>10*%eps then pause,end

if exp([])<>[] then pause,end
a=[];if exp(a)<>[] then pause,end
if log([])<>[] then pause,end
a=[];if log(a)<>[] then pause,end

//sin and cos
a=[1.2 2.51;-3.4 4.52;5.8,-6.2];
if norm(sin(a).^2+cos(a).^2-1)>10*%eps then pause,end
if norm(sin(a+0).^2+cos(a+0).^2-1)>10*%eps then pause,end
a=[1+%i 2.51;-3.4 4.52;5.8,-6.2];
if norm(sin(a).^2+cos(a).^2-1)>10*%eps then pause,end
if norm(sin(a+0).^2+cos(a+0).^2-1)>10*%eps then pause,end

if cos([])<>[] then pause,end
a=[];if cos(a)<>[] then pause,end
if sin([])<>[] then pause,end
a=[];if sin(a)<>[] then pause,end


//tan et atan
a=[1.2 2.51;-3.4 4.52;5.8,-6.2];
if norm(tan(atan(a))-a)>100*%eps then pause,end
if norm(tan(atan(a+0))-a)>100*%eps then pause,end
b=log(a);if norm(exp(b)-a)>100*%eps then pause,end
a=[1+%i 2.51;-3.4 4.52;5.8,-6.2];
if norm(tan(atan(a))-a)>100*%eps then pause,end
if norm(tan(atan(a+0))-a)>100*%eps then pause,end

if atan([])<>[] then pause,end
a=[];if atan(a)<>[] then pause,end

//AFAIRE if atan([],[])<>[] then pause,end
//AFAIRE a=[];if atan(a,[])<>[] then pause,end
//AFAIRE a=[];if atan(a,a)<>[] then pause,end
//AFAIRE a=[];if atan([],a)<>[] then pause,end

//expm
//AFAIRE a=[0 2;0 0];
//AFAIRE if norm(expm(a)-[1 2;0 1])>10*%eps then pause,end
//AFAIRE a=[0 2*%i;0 0];
//AFAIRE if norm(expm(a)-[1 2*%i;0 1])>10*%eps then pause,end
//AFAIRE if expm([])<>[] then pause,end
//AFAIRE a=[];if expm(a)<>[] then pause,end

//sqrt
a=[1.2 2.51;-3.4 4.52;5.8,-6.2];
if norm(sqrt(a).^2-a)>100*%eps then pause,end
if norm(sqrt(a+0).^2-a)>100*%eps then pause,end
a=[1+%i 2.51;-3.4 4.52;5.8,-6.2];
if norm(sqrt(a).^2-a)>100*%eps then pause,end
if norm(sqrt(a+0).^2-a)>100*%eps then pause,end

if sqrt([])<>[] then pause,end
a=[];if sqrt(a)<>[] then pause,end
