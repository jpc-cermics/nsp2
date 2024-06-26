// -*- Mode: nsp -*- 
function r=Err(x),r=norm(x,1),endfunction


//==========================================================================
//==============================    gspec     ============================== 
//==========================================================================
// XXXXXXX to be done 
if %f then 
//Empty matrix
S=spec([],[]);
if S<>[] then pause,end
[al,be]=spec([],[]);
if al<>[]|be<>[] then pause,end
[al,be,Z]=spec([],[]);
if al<>[]|be<>[]|Z<>[] then pause,end
[al,be,Z,Q]=spec([],[]);
if al<>[]|be<>[]|Z<>[]|Q<>[] then pause,end

//Matrix with Inf or Nan (test de la detection d'erreur
if execstr('spec([%inf 1;2 3],[1 2;3 4])',errcatch=%t)==%t then pause,end
if execstr('spec([1 2;3 4],[1 %nan;2 3])',errcatch=%t)==%t then pause,end

//Small dimension
//---------------
//Real Case
A=testmat1(3,5);E=testmat1(-2,5);
S=spec(A,E);
[Sa,Se]=spec(A,E);
if Err(S-Sa./Se)>10*%eps then pause,end
[Sa,Se,Z]=spec(A,E);
if Err(S-Sa./Se)>10*%eps then pause,end
if Err(A*Z-E*Z*diag(S))>200*%eps then pause,end
[Sa,Se,Q,Z]=spec(A,E);
if Err(S-Sa./Se)>10*%eps then pause,end
if Err(A*Z-E*Z*diag(S))>200*%eps then pause,end
if Err(Q'*A-diag(S)*Q'*E)>200*%eps then pause,end

//Complex Case

A=testmat1(3-%i,5);E=testmat1(-2+0.1*%i,5);
S=spec(A,E);
[Sa,Se]=spec(A,E);
if Err(S-Sa./Se)>10*%eps then pause,end
[Sa,Se,Z]=spec(A,E);
if Err(S-Sa./Se)>10*%eps then pause,end
if Err(A*Z-E*Z*diag(S))>200*%eps then pause,end
[Sa,Se,Q,Z]=spec(A,E);
if Err(S-Sa./Se)>10*%eps then pause,end
if Err(A*Z-E*Z*diag(S))>200*%eps then pause,end
if Err(Q'*A-diag(S)*Q'*E)>200*%eps then pause,end

//Large dimension
//---------------

//Real Case
A=testmat1(3,30);E=testmat1(-2,30);
S=spec(A,E);
[Sa,Se]=spec(A,E);
if Err(S-Sa./Se)>10*%eps then pause,end
[Sa,Se,Z]=spec(A,E);
if Err(S-Sa./Se)>10*%eps then pause,end
if Err(A*Z-E*Z*diag(S))>1000*%eps then pause,end
[Sa,Se,Q,Z]=spec(A,E);
if Err(S-Sa./Se)>10*%eps then pause,end
if Err(A*Z-E*Z*diag(S))>1000*%eps then pause,end
if Err(Q'*A-diag(S)*Q'*E)>1000*%eps then pause,end

//Complex Case

A=testmat1(3-%i,30);E=testmat1(-2+0.1*%i,30);
S=spec(A,E);
[Sa,Se]=spec(A,E);
if Err(S-Sa./Se)>20*%eps then pause,end
[Sa,Se,Z]=spec(A,E);
if Err(S-Sa./Se)>20*%eps then pause,end
if Err(A*Z-E*Z*diag(S))>1000*%eps then pause,end
[Sa,Se,Q,Z]=spec(A,E);
if Err(S-Sa./Se)>20*%eps then pause,end
if Err(A*Z-E*Z*diag(S))>1000*%eps then pause,end
if Err(Q'*A-diag(S)*Q'*E)>1000*%eps then pause,end

end 


