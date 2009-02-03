// -*- Mode: scilab -*- 
function r=Err(x),r=norm(x,1),endfunction;
//define tools
rand('normal')
exec('algebra-funs.sce');

//==========================================================================
//==============================    schur     ============================== 
//==========================================================================
// XXXXXXXXXXXX to be done 

function t=sel(R),t=real(R)<0 ,endfunction;
//Empty matrix
A=[];
if schur(A)<>[] then pause,end
if schur(A,complex=%f)<>[] then pause,end
if schur(A,complex=%t)<>[] then pause,end

if schur(A,sort='c')<>[] then pause,end
if schur(A,sort='d')<>[] then pause,end
if schur(A,sort=sel)<>[] then pause,end

[U,S]=schur(A);
if U<>[]|S<>[] then pause,end
[U,S]=schur(A,complex=%f);
if U<>[]|S<>[] then pause,end
[U,S]=schur(A,complex=%t);
if U<>[]|S<>[] then pause,end

[U,N]=schur(A,sort='c');
if U<>[]|N<>0 then pause,end
[U,N]=schur(A,sort='d');
if U<>[]|N<>0 then pause,end
[U,N]=schur(A,sort=sel);
if U<>[]|N<>0 then pause,end

[U,N,S]=schur(A,sort='c');
if U<>[]|N<>0|S<>[] then pause,end
[U,N,S]=schur(A,sort='d');
if U<>[]|N<>0|S<>[] then pause,end
[U,N,S]=schur(A,sort=sel);
if U<>[]|N<>0|S<>[] then pause,end

//Rectangular matrix
if execstr('schur(rand(2,3))',errcatch=%t)==%t then pause,end
if execstr('[U,S]=schur(rand(2,3))',errcatch=%t)==%t then pause,end

if execstr('schur(rand(2,3)+%i*eye(2,3))',errcatch=%t)==%t then pause,end
if execstr('[U,S]=schur(rand(2,3)+%i*eye(2,3))',errcatch=%t)==%t then pause,end

//Small dimension
A=testmat1(3,5);Ac=testmat1(3+%i,5);
//Real
[U,S]=schur(A);
if Err(triu(S,-1)-S)>%eps then pause,end
if Err(U*S*U'-A)>200*%eps then pause,end
if Err(schur(A)-S) >%eps then pause,end

[U,S]=schur(A,complex=%f);
if Err(triu(S,-1)-S)>%eps then pause,end
if Err(U*S*U'-A)>200*%eps then pause,end
if Err(schur(A)-S) >%eps then pause,end

[U,S]=schur(A,complex=%t);
if Err(triu(S)-S)>%eps then pause,end
if Err(U*S*U'-A)>200*%eps then pause,end
if Err(schur(A,complex=%t)-S) >%eps then pause,end

[U,n]=schur(A,sort='c');S=U'*A*U;
if n<>2 then pause,end
if or(real(spec(S(1:n,1:n)))>=0) then pause,end
if or(real(spec(S(n+1:$,n+1:$)))<0) then pause,end

[U,n,S]=schur(A,sort='c');
if n<>2 then pause,end
if or(real(spec(S(1:n,1:n)))>=0) then pause,end
if or(real(spec(S(n+1:$,n+1:$)))<0) then pause,end
if Err(U*S*U'-A)>200*%eps then pause,end

[U,n]=schur(A,sort='d');S=U'*A*U;
if n<>0 then pause,end
if or(abs(spec(S(n+1:$,n+1:$)))<1) then pause,end

[U,n,S]=schur(A,sort='d');
if n<>0 then pause,end
if or(abs(spec(S(n+1:$,n+1:$)))<1) then pause,end
if Err(U*S*U'-A)>200*%eps then pause,end

[U,n,S]=schur(A,sort=sel);
if n<>2 then pause,end
if or(real(spec(S(1:n,1:n)))>=0) then pause,end
if or(real(spec(S(n+1:$,n+1:$)))<0) then pause,end
if Err(U*S*U'-A)>200*%eps then pause,end

//Complex

[U,S]=schur(Ac);
if Err(triu(S,-1)-S)>%eps then pause,end
if Err(U*S*U'-Ac)>200*%eps then pause,end
if Err(schur(Ac)-S) >%eps then pause,end

[U,S]=schur(Ac,complex=%t);
if Err(triu(S,-1)-S)>%eps then pause,end
if Err(U*S*U'-Ac)>200*%eps then pause,end
if Err(schur(Ac)-S) >%eps then pause,end

[U,n]=schur(Ac,sort='c');S=U'*Ac*U;
if n<>3 then pause,end
if or(real(spec(S(1:n,1:n)))>=0) then pause,end
if or(real(spec(S(n+1:$,n+1:$)))<0) then pause,end

[U,n,S]=schur(Ac,sort='c');
if n<>3 then pause,end
if or(real(spec(S(1:n,1:n)))>=0) then pause,end
if or(real(spec(S(n+1:$,n+1:$)))<0) then pause,end
if Err(triu(S)-S)>%eps then pause,end
if Err(U*S*U'-Ac)>200*%eps then pause,end

[U,n]=schur(Ac,sort='d');S=U'*Ac*U;
if n<>0 then pause,end
if or(abs(spec(S(n+1:$,n+1:$)))<1) then pause,end

[U,n,S]=schur(Ac,sort='d');
if n<>0 then pause,end
if or(abs(spec(S(n+1:$,n+1:$)))<1) then pause,end
if Err(triu(S)-S)>%eps then pause,end
if Err(U*S*U'-Ac)>200*%eps then pause,end

[U,n]=schur(Ac,sort=sel);S=U'*Ac*U;
if n<>3 then pause,end
if or(real(spec(S(1:n,1:n)))>=0) then pause,end
if or(real(spec(S(n+1:$,n+1:$)))<0) then pause,end

[U,n,S]=schur(Ac,sort=sel);
if n<>3 then pause,end
if or(real(spec(S(1:n,1:n)))>=0) then pause,end
if or(real(spec(S(n+1:$,n+1:$)))<0) then pause,end
if Err(triu(S)-S)>%eps then pause,end
if Err(U*S*U'-Ac)>200*%eps then pause,end


//Large dimension
A=testmat1(3,50);Ac=testmat1(3+%i,50);
//Real
[U,S]=schur(A);
if Err(triu(S,-1)-S)>%eps then pause,end
if Err(U*S*U'-A)>2000*%eps then pause,end
if Err(schur(A)-S) >%eps then pause,end

[U,S]=schur(A,complex=%f);
if Err(triu(S,-1)-S)>%eps then pause,end
if Err(U*S*U'-A)>2000*%eps then pause,end
if Err(schur(A)-S) >%eps then pause,end

[U,S]=schur(A,complex=%t);
if Err(triu(S)-S)>%eps then pause,end
if Err(U*S*U'-A)>2000*%eps then pause,end
//BUGXXXXX if Err(schur(A,complex=%t)-S) >%eps then pause,end


[U,n]=schur(A,sort='c');S=U'*A*U;
if n<>25 then pause,end
if or(real(spec(S(1:n,1:n)))>=0) then pause,end
if or(real(spec(S(n+1:$,n+1:$)))<0) then pause,end

[U,n,S]=schur(A,sort='c');
if n<>25 then pause,end
if or(real(spec(S(1:n,1:n)))>=0) then pause,end
if or(real(spec(S(n+1:$,n+1:$)))<0) then pause,end
if Err(triu(S,-1)-S)>%eps then pause,end
if Err(U*S*U'-A)>2000*%eps then pause,end

[U,n]=schur(A,sort='d');S=U'*A*U;
if n<>0 then pause,end
if or(abs(spec(S(n+1:$,n+1:$)))<1) then pause,end

[U,n]=schur(A,sort=sel);S=U'*A*U;
if n<>25 then pause,end
if or(real(spec(S(1:n,1:n)))>=0) then pause,end
if or(real(spec(S(n+1:$,n+1:$)))<0) then pause,end

//Complex 
[U,S]=schur(Ac);
if Err(triu(S)-S)>%eps then pause,end
if Err(U*S*U'-Ac)>2000*%eps then pause,end
// succesive calls of schur do not give the same ordering !
// if Err(schur(Ac)-S) >%eps then pause,end

[U,S]=schur(Ac,complex=%t);
if Err(triu(S)-S)>%eps then pause,end
if Err(U*S*U'-Ac)>2000*%eps then pause,end
//if Err(schur(Ac)-S) >%eps then pause,end

[U,n]=schur(Ac,sort='c');S=U'*Ac*U;
if n<>25 then pause,end
if or(real(spec(S(1:n,1:n)))>=0) then pause,end
if or(real(spec(S(n+1:$,n+1:$)))<0) then pause,end

[U,n,S]=schur(Ac,sort='c');
if n<>25 then pause,end
if or(real(spec(S(1:n,1:n)))>=0) then pause,end
if or(real(spec(S(n+1:$,n+1:$)))<0) then pause,end
if Err(triu(S)-S)>%eps then pause,end
if Err(U*S*U'-Ac)>2000*%eps then pause,end

[U,n]=schur(Ac,sort='d');S=U'*Ac*U;
if n<>0 then pause,end
if or(abs(spec(S(n+1:$,n+1:$)))<1) then pause,end

[U,n,S]=schur(Ac,sort='d');
if n<>0 then pause,end
if or(abs(spec(S(n+1:$,n+1:$)))<1) then pause,end
if Err(triu(S)-S)>%eps then pause,end
if Err(U*S*U'-Ac)>2000*%eps then pause,end

[U,n]=schur(Ac,sort=sel);S=U'*Ac*U;
if n<>25 then pause,end
if or(real(spec(S(1:n,1:n)))>=0) then pause,end
if or(real(spec(S(n+1:$,n+1:$)))<0) then pause,end

[U,n,S]=schur(Ac,sort=sel);
if n<>25 then pause,end
if or(real(spec(S(1:n,1:n)))>=0) then pause,end
if or(real(spec(S(n+1:$,n+1:$)))<0) then pause,end
if Err(triu(S)-S)>%eps then pause,end
if Err(U*S*U'-Ac)>2000*%eps then pause,end
