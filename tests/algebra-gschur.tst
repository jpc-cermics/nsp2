// -*- Mode: scilab -*- 
function r=Err(x),r=norm(x,1),endfunction
rand('normal')


//==========================================================================
//==============================    gschur    ============================== 
//==========================================================================
//Empty matrix

// XXXX to be done 
if %f then 
[As,Es]=schur([],[]);
if As<>[]|Es<>[] then pause,end

[As,dim]=schur([],[],'c');
if As<>[]|dim<>0 then pause,end
[As,dim]=schur([],[],'d');
if As<>[]|dim<>0 then pause,end
[As,dim]=schur([],[],sel);
if As<>[]|dim<>0 then pause,end

[As,Es,Q,Z]=schur([],[]);
if As<>[]|Es<>[]|Q<>[]|Z<>[] then pause,end

[As,Es,dim]=schur([],[],'c');
if As<>[]|Es<>[]|dim<>0 then pause,end
[As,Es,dim]=schur([],[],'d');
if As<>[]|Es<>[]|dim<>0 then pause,end
[As,Es,dim]=schur([],[],sel);
if As<>[]|Es<>[]|dim<>0 then pause,end

[Z,dim]=schur([],[],'c');
if Z<>[]|dim<>0 then pause,end
[Z,dim]=schur([],[],'d');
if Z<>[]|dim<>0 then pause,end
[Z,dim]=schur([],[],sel);
if Z<>[]|dim<>0 then pause,end


//Rectangular matrix
if execstr('[As,Es]=schur(rand(2,3),rand(2,3))',errcatch=%t)==%t then  pause,end
if execstr('[As,Es,Q,Z]=schur(rand(2,3),rand(2,3))',errcatch=%t)==%t then  pause,end
if execstr('[As,Es,dim]=schur(rand(2,3),rand(2,3),''c'')',errcatch=%t)==%t then  pause,end
if execstr('[Z,dim]=schur(rand(2,3),rand(2,3),sel)',errcatch=%t)==%t then  pause,end

//Small dimension
//----Real------------
A=testmat1(1,5);E=testmat1(-2,5) ;
[As,Es,Q,Z]=schur(A,E);
if Err(Q*Q'-eye(Q)) >200*%eps then pause,end
if Err(Z*Z'-eye(Z)) >200*%eps then pause,end
if Err(As-Q'*A*Z) >200*%eps then pause,end
if Err(Es-Q'*E*Z) >200*%eps then pause,end

[As1,Es1]=schur(A,E);
if Err(As1-As)>10*%eps then pause,end
if Err(Es1-Es)>10*%eps then pause,end

// Ordered 'c'
dim=schur(A,E,'c');
if dim<>5 then pause,end
[Z,dim]=schur(A,E,'c');
if Err(Z*Z'-eye(Z)) >200*%eps then pause,end

[Q,Z1,dim]=schur(A,E,'c');
if Err(Z1-Z)>10*%eps then pause,end
if Err(Q*Q'-eye(Q)) >200*%eps then pause,end
if dim<>5 then pause,end

[As,Es,Z,dim]=schur(A,E,'d');
if dim<>5 then pause,end
if Err(Q*Q'-eye(Q)) >200*%eps then pause,end
if Err(Z*Z'-eye(Z)) >200*%eps then pause,end
if Err(As-Q'*A*Z) >200*%eps then pause,end
if Err(Es-Q'*E*Z) >200*%eps then pause,end
// Ordered 'd'
dim=schur(A,E,'d');
if dim<>5 then pause,end
[Z,dim]=schur(A,E,'d');
if Err(Z*Z'-eye(Z)) >200*%eps then pause,end

[Q,Z1,dim]=schur(A,E,'d');
if Err(Z1-Z)>10*%eps then pause,end
if Err(Q*Q'-eye(Q)) >200*%eps then pause,end
if dim<>5 then pause,end

[As,Es,Z,dim]=schur(A,E,'d');
if dim<>5 then pause,end
if Err(Q*Q'-eye(Q)) >200*%eps then pause,end
if Err(Z*Z'-eye(Z)) >200*%eps then pause,end
if Err(As-Q'*A*Z) >200*%eps then pause,end
if Err(Es-Q'*E*Z) >200*%eps then pause,end

//ordered sel
function t=sel(Alpha,Beta),t=real(Alpha)>-0.2*real(Beta) ,endfunction

dim=schur(A,E,sel);
if dim<>2 then pause,end
[Z,dim]=schur(A,E,sel);
if Err(Z*Z'-eye(Z)) >200*%eps then pause,end

[Q,Z1,dim]=schur(A,E,sel);
if Err(Z1-Z)>10*%eps then pause,end
if Err(Q*Q'-eye(Q)) >200*%eps then pause,end
if dim<>2 then pause,end

[As,Es,Z,dim]=schur(A,E,sel);
if dim<>2 then pause,end
if Err(Q*Q'-eye(Q)) >200*%eps then pause,end
if Err(Z*Z'-eye(Z)) >200*%eps then pause,end
if Err(As-Q'*A*Z) >200*%eps then pause,end
if Err(Es-Q'*E*Z) >200*%eps then pause,end
//----Complex------------
A=testmat1(1+%i,5);E=testmat1(-2-3*%i,5) ;
[As,Es,Q,Z]=schur(A,E);
if Err(Q*Q'-eye(Q)) >200*%eps then pause,end
if Err(Z*Z'-eye(Z)) >200*%eps then pause,end
if Err(As-Q'*A*Z) >200*%eps then pause,end
if Err(Es-Q'*E*Z) >200*%eps then pause,end

[As1,Es1]=schur(A,E);
if Err(As1-As)>10*%eps then pause,end
if Err(Es1-Es)>10*%eps then pause,end

// Ordered 'c'
dim=schur(A,E,'c');
if dim<>5 then pause,end
[Z,dim]=schur(A,E,'c');
if Err(Z*Z'-eye(Z)) >200*%eps then pause,end

[Q,Z1,dim]=schur(A,E,'c');
if Err(Z1-Z)>10*%eps then pause,end
if Err(Q*Q'-eye(Q)) >200*%eps then pause,end
if dim<>5 then pause,end

[As,Es,Z,dim]=schur(A,E,'d');
if dim<>5 then pause,end
if Err(Q*Q'-eye(Q)) >200*%eps then pause,end
if Err(Z*Z'-eye(Z)) >200*%eps then pause,end
if Err(As-Q'*A*Z) >200*%eps then pause,end
if Err(Es-Q'*E*Z) >200*%eps then pause,end
// Ordered 'd'
dim=schur(A,E,'d');
if dim<>5 then pause,end
[Z,dim]=schur(A,E,'d');
if Err(Z*Z'-eye(Z)) >200*%eps then pause,end

[Q,Z1,dim]=schur(A,E,'d');
if Err(Z1-Z)>10*%eps then pause,end
if Err(Q*Q'-eye(Q)) >200*%eps then pause,end
if dim<>5 then pause,end

[As,Es,Z,dim]=schur(A,E,'d');
if dim<>5 then pause,end
if Err(Q*Q'-eye(Q)) >200*%eps then pause,end
if Err(Z*Z'-eye(Z)) >200*%eps then pause,end
if Err(As-Q'*A*Z) >200*%eps then pause,end
if Err(Es-Q'*E*Z) >200*%eps then pause,end

//ordered sel
function t=sel(Alpha,Beta),t=imag(Alpha)>0 ,endfunction

dim=schur(A,E,sel);
if dim<>3 then pause,end
[Z,dim]=schur(A,E,sel);
if Err(Z*Z'-eye(Z)) >200*%eps then pause,end

[Q,Z1,dim]=schur(A,E,sel);
if Err(Z1-Z)>10*%eps then pause,end
if Err(Q*Q'-eye(Q)) >200*%eps then pause,end
if dim<>3 then pause,end

[As,Es,Z,dim]=schur(A,E,sel);
if dim<>3 then pause,end
if Err(Q*Q'-eye(Q)) >200*%eps then pause,end
if Err(Z*Z'-eye(Z)) >200*%eps then pause,end
if Err(As-Q'*A*Z) >200*%eps then pause,end
if Err(Es-Q'*E*Z) >200*%eps then pause,end

//Large dimension

//----Real------------
A=testmat1(1,50);E=testmat1(-2,50) ;
[As,Es,Q,Z]=schur(A,E);
if Err(Q*Q'-eye(Q)) >200*%eps then pause,end
if Err(Z*Z'-eye(Z)) >200*%eps then pause,end
if Err(As-Q'*A*Z) >200*%eps then pause,end
if Err(Es-Q'*E*Z) >200*%eps then pause,end

[As1,Es1]=schur(A,E);
if Err(As1-As)>10*%eps then pause,end
if Err(Es1-Es)>10*%eps then pause,end

// Ordered 'c'
dim=schur(A,E,'c');
if dim<>50 then pause,end
[Z,dim]=schur(A,E,'c');
if Err(Z*Z'-eye(Z)) >200*%eps then pause,end

[Q,Z1,dim]=schur(A,E,'c');
if Err(Z1-Z)>10*%eps then pause,end
if Err(Q*Q'-eye(Q)) >200*%eps then pause,end
if dim<>50 then pause,end

[As,Es,Z,dim]=schur(A,E,'d');
if dim<>50 then pause,end
if Err(Q*Q'-eye(Q)) >200*%eps then pause,end
if Err(Z*Z'-eye(Z)) >200*%eps then pause,end
if Err(As-Q'*A*Z) >200*%eps then pause,end
if Err(Es-Q'*E*Z) >200*%eps then pause,end
// Ordered 'd'
dim=schur(A,E,'d');
if dim<>50 then pause,end
[Z,dim]=schur(A,E,'d');
if Err(Z*Z'-eye(Z)) >200*%eps then pause,end

[Q,Z1,dim]=schur(A,E,'d');
if Err(Z1-Z)>10*%eps then pause,end
if Err(Q*Q'-eye(Q)) >200*%eps then pause,end
if dim<>50 then pause,end

[As,Es,Z,dim]=schur(A,E,'d');
if dim<>50 then pause,end
if Err(Q*Q'-eye(Q)) >200*%eps then pause,end
if Err(Z*Z'-eye(Z)) >200*%eps then pause,end
if Err(As-Q'*A*Z) >200*%eps then pause,end
if Err(Es-Q'*E*Z) >200*%eps then pause,end

//ordered sel
function t=sel(Alpha,Beta),t=real(Alpha)>-0.2*real(Beta) ,endfunction

dim=schur(A,E,sel);
if dim<>12 then pause,end
[Z,dim]=schur(A,E,sel);
if Err(Z*Z'-eye(Z)) >200*%eps then pause,end

[Q,Z1,dim]=schur(A,E,sel);
if Err(Z1-Z)>10*%eps then pause,end
if Err(Q*Q'-eye(Q)) >200*%eps then pause,end
if dim<>12 then pause,end

[As,Es,Z,dim]=schur(A,E,sel);
if dim<>12 then pause,end
if Err(Q*Q'-eye(Q)) >200*%eps then pause,end
if Err(Z*Z'-eye(Z)) >200*%eps then pause,end
if Err(As-Q'*A*Z) >200*%eps then pause,end
if Err(Es-Q'*E*Z) >200*%eps then pause,end
//----Complex------------
A=testmat1(1+%i,50);E=testmat1(-2-3*%i,50) ;
[As,Es,Q,Z]=schur(A,E);
if Err(Q*Q'-eye(Q)) >200*%eps then pause,end
if Err(Z*Z'-eye(Z)) >200*%eps then pause,end
if Err(As-Q'*A*Z) >1000*%eps then pause,end
if Err(Es-Q'*E*Z) >1000*%eps then pause,end

[As1,Es1]=schur(A,E);
if Err(As1-As)>10*%eps then pause,end
if Err(Es1-Es)>10*%eps then pause,end

// Ordered 'c'
dim=schur(A,E,'c');
if dim<>50 then pause,end
[Z,dim]=schur(A,E,'c');
if Err(Z*Z'-eye(Z)) >200*%eps then pause,end

[Q,Z1,dim]=schur(A,E,'c');
if Err(Z1-Z)>10*%eps then pause,end
if Err(Q*Q'-eye(Q)) >200*%eps then pause,end
if dim<>50 then pause,end

[As,Es,Z,dim]=schur(A,E,'d');
if dim<>50 then pause,end
if Err(Q*Q'-eye(Q)) >200*%eps then pause,end
if Err(Z*Z'-eye(Z)) >200*%eps then pause,end
if Err(As-Q'*A*Z) >1000*%eps then pause,end
if Err(Es-Q'*E*Z) >1000*%eps then pause,end
// Ordered 'd'
dim=schur(A,E,'d');
if dim<>50 then pause,end
[Z,dim]=schur(A,E,'d');
if Err(Z*Z'-eye(Z)) >200*%eps then pause,end

[Q,Z1,dim]=schur(A,E,'d');
if Err(Z1-Z)>10*%eps then pause,end
if Err(Q*Q'-eye(Q)) >200*%eps then pause,end
if dim<>50 then pause,end

[As,Es,Z,dim]=schur(A,E,'d');
if dim<>50 then pause,end
if Err(Q*Q'-eye(Q)) >200*%eps then pause,end
if Err(Z*Z'-eye(Z)) >200*%eps then pause,end
if Err(As-Q'*A*Z) >1000*%eps then pause,end
if Err(Es-Q'*E*Z) >1000*%eps then pause,end

//ordered sel
function t=sel(Alpha,Beta),t=imag(Alpha)>0 ,endfunction

dim=schur(A,E,sel);
if dim<>32 then pause,end
[Z,dim]=schur(A,E,sel);
if Err(Z*Z'-eye(Z)) >200*%eps then pause,end

[Q,Z1,dim]=schur(A,E,sel);
if Err(Z1-Z)>10*%eps then pause,end
if Err(Q*Q'-eye(Q)) >200*%eps then pause,end
if dim<>32 then pause,end

[As,Es,Z,dim]=schur(A,E,sel);
if dim<>32 then pause,end
if Err(Q*Q'-eye(Q)) >200*%eps then pause,end
if Err(Z*Z'-eye(Z)) >200*%eps then pause,end
if Err(As-Q'*A*Z) >1000*%eps then pause,end
if Err(Es-Q'*E*Z) >1000*%eps then pause,end

end 
