// -*- Mode: scilab -*- 
function r=Err(x),r=norm(x,1),endfunction

//==========================================================================
//==============================      lu      ============================== 
//==========================================================================
//Empty matrix
A=[];
[L,U]=lu(A);
if L*U<>A then pause,end
[L,U,E]=lu(A);
if L*U <> A(E,:) then pause,end
//Non full rank
A=randn(5,2);A=A*A';Ac=randn(5,2)+%i*randn(5,2);Ac=Ac*Ac';
[L,U,E]=lu(A);
if Err(L*U-A(E,:)) >200*%eps then pause,end
[L,U,E]=lu(Ac);
if Err(L*U-Ac(E,:)) >200*%eps then pause,end

//Small dimension
//---------------
//Square
A=randn(5,5);Ac=A+%i*randn(A);
//Real case 

[L,U]=lu(A);
if Err(L*U-A) >200*%eps then pause,end
[L,U,E]=lu(A);
if Err(L*U-A(E,:)) >200*%eps then pause,end
//Complex case
[L,U]=lu(Ac);
if Err(L*U-Ac) >200*%eps then pause,end
[L,U,E]=lu(Ac);
if Err(L*U-Ac(E,:)) >200*%eps then pause,end
//Fat
A=randn(3,5);Ac=A+%i*randn(A);
//Real case
[L,U]=lu(A);
if Err(L*U-A) >200*%eps then pause,end
[L,U,E]=lu(A);
if Err(L*U-A(E,:)) >200*%eps then pause,end
//Complex case
[L,U]=lu(Ac);
if Err(L*U-Ac) >200*%eps then pause,end
[L,U,E]=lu(Ac);
if Err(L*U-Ac(E,:)) >200*%eps then pause,end
//Tall
A=randn(5,3);Ac=A+%i*randn(A);
//Real case
[L,U]=lu(A);
if Err(L*U-A) >200*%eps then pause,end
[L,U,E]=lu(A);
if Err(L*U-A(E,:)) >200*%eps then pause,end
//Complex case
[L,U]=lu(Ac);
if Err(L*U-Ac) >200*%eps then pause,end
[L,U,E]=lu(Ac);
if Err(L*U-Ac(E,:)) >200*%eps then pause,end


//large dimension
//---------------
//Square
A=randn(50,50);Ac=A+%i*randn(A);
//Real case
[L,U]=lu(A);
if Err(L*U-A) >1000*%eps then pause,end
[L,U,E]=lu(A);
if Err(L*U-A(E,:)) >1000*%eps then pause,end
//Complex case
[L,U]=lu(Ac);
if Err(L*U-Ac) >1000*%eps then pause,end
[L,U,E]=lu(Ac);
if Err(L*U-Ac(E,:)) >1000*%eps then pause,end
//Fat
A=randn(30,50);Ac=A+%i*randn(A);
//Real case
[L,U]=lu(A);
if Err(L*U-A) >1000*%eps then pause,end
[L,U,E]=lu(A);
if Err(L*U-A(E,:)) >1000*%eps then pause,end
//Complex case
[L,U]=lu(Ac);
if Err(L*U-Ac) >1000*%eps then pause,end
[L,U,E]=lu(Ac);
if Err(L*U-Ac(E,:)) >1000*%eps then pause,end
//Tall
A=randn(50,30);Ac=A+%i*randn(A);
//Real case
[L,U]=lu(A);
if Err(L*U-A) >1000*%eps then pause,end
[L,U,E]=lu(A);
if Err(L*U-A(E,:)) >1000*%eps then pause,end
//Complex case
[L,U]=lu(Ac);
if Err(L*U-Ac) >1000*%eps then pause,end
[L,U,E]=lu(Ac);
if Err(L*U-Ac(E,:)) >1000*%eps then pause,end
