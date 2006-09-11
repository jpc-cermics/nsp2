// -*- Mode: scilab -*- 
// tests for setdiff


//
// with real matrices
//
A = [1, 4, %pi, 4, 1, -1, 5.5];
B = [%pi, 4];
res = [-1, 1, 5.5];

C = setdiff(A,B);
if ~and(C == res) then, pause; end

[C,k] = setdiff(A,B);
if ~and(C == res) then, pause; end
if size(k,1) ~=1 then, pause; end
if ~and(C == A(k)) then, pause; end

// transpose B (nothing must change : only A impose the output form)
B.redim[-1,1];

C = setdiff(A,B);
if ~and(C == res) then, pause; end

[C,k] = setdiff(A,B);
if ~and(C == res) then, pause; end
if size(k,1) ~=1 then, pause; end
if ~and(C == A(k)) then, pause; end

// transpose A (outputs must be columns)
A.redim[-1,1];
res.redim[-1,1];

C = setdiff(A,B);
if ~and(C == res) then, pause; end

[C,k] = setdiff(A,B);
if ~and(C == res) then, pause; end
if size(k,2) ~=1 then, pause; end
if ~and(C == A(k)) then, pause; end

// one again transpose B (nothing must change)
B.redim[-1,1];

C = setdiff(A,B);
if ~and(C == res) then, pause; end

[C,k] = setdiff(A,B);
if ~and(C == res) then, pause; end
if size(k,2) ~=1 then, pause; end
if ~and(C == A(k)) then, pause; end


// take B = []
//
A = [1, 4, %pi, 4, 1, -1, 5.5];
B = [];
res = [-1, 1, %pi, 4, 5.5];

C = setdiff(A,B);
if ~and(C == res) then, pause; end

[C,k] = setdiff(A,B);
if ~and(C == res) then, pause; end
if size(k,1) ~=1 then, pause; end
if ~and(C == A(k)) then, pause; end

A.redim[-1,1];
res.redim[-1,1];

C = setdiff(A,B);
if ~and(C == res) then, pause; end

[C,k] = setdiff(A,B);
if ~and(C == res) then, pause; end
if size(k,2) ~=1 then, pause; end
if ~and(C == A(k)) then, pause; end


// take A = []
//
A = []
B = [1,2,3];
C = setdiff(A,B);
if ~and(C == []) then, pause; end

[C,k] = setdiff(A,B);
if C ~= [] then, pause; end
if k ~= [] then, pause; end


// A a matrix => considered as a column => outputs must be columns
A = [1,2,3;1,2,3;1,2,3];
B = 3;
res = [1;2];

C = setdiff(A,B);
if ~and(C == res) then, pause; end

[C,k] = setdiff(A,B);
if ~and(C == res) then, pause; end
if size(k,2) ~=1 then, pause; end
if ~and(C == A(k)) then, pause; end


//
// with string matrices
//
A = ["toto", "foo", "toto", "foo", "bar", "toto"];
B = ["foo"];
res = ["bar", "toto"];


C = setdiff(A,B);
if ~and(C == res) then, pause; end

[C,k] = setdiff(A,B);
if ~and(C == res) then, pause; end
if size(k,1) ~=1 then, pause; end
if ~and(C == A(k)) then, pause; end

// put B in column form (nothing must change : only A impose the output form)
B.redim[-1,1];

C = setdiff(A,B);
if ~and(C == res) then, pause; end

[C,k] = setdiff(A,B);
if ~and(C == res) then, pause; end
if size(k,1) ~=1 then, pause; end
if ~and(C == A(k)) then, pause; end

// put A in column form (outputs must be columns)
A.redim[-1,1];
res.redim[-1,1];

C = setdiff(A,B);
if ~and(C == res) then, pause; end

[C,k] = setdiff(A,B);
if ~and(C == res) then, pause; end
if size(k,2) ~=1 then, pause; end
if ~and(C == A(k)) then, pause; end

// put B in row form (nothing must change)
B.redim[-1,1];

C = setdiff(A,B);
if ~and(C == res) then, pause; end

[C,k] = setdiff(A,B);
if ~and(C == res) then, pause; end
if size(k,2) ~=1 then, pause; end
if ~and(C == A(k)) then, pause; end


// put A as matrix (outputs must be columns)
A.redim[3,2];

C = setdiff(A,B);
if ~and(C == res) then, pause; end

[C,k] = setdiff(A,B);
if ~and(C == res) then, pause; end
if size(k,2) ~=1 then, pause; end
if ~and(C == A(k)) then, pause; end

