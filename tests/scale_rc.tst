// -*- Mode: nsp -*- 
// test scale_rows and scale_cols methods (full and sparse matrices)

// for full matrices 
// case A real - x real
A = rand(6,8);
B = A;
x = rand(6,1);
A.scale_rows[x];
C = B.*(x*ones(1,8));
if ~A.equal[C] then, pause, end

A = B;
A.scale_rows[x,op='/'];
C = B./(x*ones(1,8));
if ~A.equal[C] then, pause, end

x = rand(1,8);
A = B;
A.scale_cols[x,op='*'];
C = B.*(ones(6,1)*x);
if ~A.equal[C] then, pause, end

A = B;
A.scale_cols[x,op='/'];
C = B./(ones(6,1)*x);
if ~A.equal[C] then, pause, end

// case A complex - x real
A = rand(6,8) + %i*rand(6,8);
B = A;
x = rand(6,1);
A.scale_rows[x];
C = B.*(x*ones(1,8));
if ~A.equal[C] then, pause, end

A = B;
A.scale_rows[x,op='/'];
C = B./(x*ones(1,8));
if ~A.equal[C] then, pause, end

x = rand(1,8);
A = B;
A.scale_cols[x,op='*'];
C = B.*(ones(6,1)*x);
if ~A.equal[C] then, pause, end

A = B;
A.scale_cols[x,op='/'];
C = B./(ones(6,1)*x);
if ~A.equal[C] then, pause, end

// case A complex - x complex
A = rand(6,8) + %i*rand(6,8);
B = A;
x = rand(6,1) + %i*rand(6,1);
A.scale_rows[x];
C = B.*(x*ones(1,8));
if ~A.equal[C] then, pause, end

A = B;
A.scale_rows[x,op='/'];
C = B./(x*ones(1,8));
if ~A.equal[C] then, pause, end

x = rand(1,8) + %i*rand(1,8);
A = B;
A.scale_cols[x,op='*'];
C = B.*(ones(6,1)*x);
if ~A.equal[C] then, pause, end

A = B;
A.scale_cols[x,op='/'];
C = B./(ones(6,1)*x);
if ~A.equal[C] then, pause, end


// case A real - x complex
A = rand(6,8);
B = A;
x = rand(6,1) + %i*rand(6,1);
A.scale_rows[x];
C = B.*(x*ones(1,8));
if ~A.equal[C] then, pause, end

A = B;
A.scale_rows[x,op='/'];
C = B./(x*ones(1,8));
if ~A.equal[C] then, pause, end

x = rand(1,8) + %i*rand(1,8);
A = B;
A.scale_cols[x,op='*'];
C = B.*(ones(6,1)*x);
if ~A.equal[C] then, pause, end

A = B;
A.scale_cols[x,op='/'];
C = B./(ones(6,1)*x);
if ~A.equal[C] then, pause, end


// for sparse matrices 
// case A real - x real
A = sprand(6,8,0.5);
B = A;
x = rand(6,1);
A.scale_rows[x];
C = B.*(x*ones(1,8));
if ~A.equal[C] then, pause, end

A = B;
A.scale_rows[x,op='/'];
C = B./(x*ones(1,8));
if ~A.equal[C] then, pause, end

x = rand(1,8);
A = B;
A.scale_cols[x,op='*'];
C = B.*(ones(6,1)*x);
if ~A.equal[C] then, pause, end

A = B;
A.scale_cols[x,op='/'];
C = B./(ones(6,1)*x);
if ~A.equal[C] then, pause, end

// case A complex - x real
A = sprand(6,8,0.5) + %i*sprand(6,8,0.5);
B = A;
x = rand(6,1);
A.scale_rows[x];
C = B.*(x*ones(1,8));
if ~A.equal[C] then, pause, end

A = B;
A.scale_rows[x,op='/'];
C = B./(x*ones(1,8));
if ~A.equal[C] then, pause, end

x = rand(1,8);
A = B;
A.scale_cols[x,op='*'];
C = B.*(ones(6,1)*x);
if ~A.equal[C] then, pause, end

A = B;
A.scale_cols[x,op='/'];
C = B./(ones(6,1)*x);
if ~A.equal[C] then, pause, end

// case A complex - x complex
A = sprand(6,8,0.5) + %i*sprand(6,8,0.5);
B = A;
x = rand(6,1) + %i*rand(6,1);
A.scale_rows[x];
C = B.*(x*ones(1,8));
if ~A.equal[C] then, pause, end

A = B;
A.scale_rows[x,op='/'];
C = B./(x*ones(1,8));
if ~A.equal[C] then, pause, end

x = rand(1,8) + %i*rand(1,8);
A = B;
A.scale_cols[x,op='*'];
C = B.*(ones(6,1)*x);
if ~A.equal[C] then, pause, end

A = B;
A.scale_cols[x,op='/'];
C = B./(ones(6,1)*x);
if ~A.equal[C] then, pause, end


// case A real - x complex
A = sprand(6,8,0.5);
B = A;
x = rand(6,1) + %i*rand(6,1);
A.scale_rows[x];
C = B.*(x*ones(1,8));
if ~A.equal[C] then, pause, end

A = B;
A.scale_rows[x,op='/'];
C = B./(x*ones(1,8));
if ~A.equal[C] then, pause, end

x = rand(1,8) + %i*rand(1,8);
A = B;
A.scale_cols[x,op='*'];
C = B.*(ones(6,1)*x);
if ~A.equal[C] then, pause, end

A = B;
A.scale_cols[x,op='/'];
C = B./(ones(6,1)*x);
if ~A.equal[C] then, pause, end


