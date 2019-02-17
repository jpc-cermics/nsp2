// -*- Mode: nsp -*- 
function r=Err(x),r=norm(x,1),endfunction

//==========================================================================
//==============================    rcond     ============================== 
//==========================================================================
//Empty matrix
A=[];
if rcond(A)<>[] then pause,end

//Rectangular matrix
if execstr('rcond(randn(2,3))',errcatch=%t)==%t then pause,end
if execstr('rcond(randn(2,3)+%i*eye(2,3))',errcatch=%t)==%t then pause,end
//Small dimension
//---------------
//Real Case
if Err(rcond(eye_new(5,5))-1)>10*%eps then pause,end
//Complex Case
if  Err(rcond(eye_new(5,5)*(1+%i))-1)>10*%eps then pause,end


//Large dimension
//---------------
//Real Case
if Err(rcond(eye_new(50,50))-1)>10*%eps then pause,end
//Complex Case
if  Err(rcond(eye_new(50,50)*(1+%i))-1)>10*%eps then pause,end

