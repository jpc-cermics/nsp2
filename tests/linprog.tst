// -*- Mode: nsp -*-
// Copyright (C) 2012-2015 Bruno Pinçon ESIAL/IECN

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
// Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

// tests for linprog and readlp (to be completed) 

if ~%glpk then 
   printf("This version of nsp was not compiled with glpk support\n");
   return;
end 

/////////////////////////////////////////////////////////////////////////////////
// test 1-1:   max c'*x
//             Ax <= b
//              x >= 0
A = [ 2 4 8  6;
     10 8 6 10;
      1 1 2  2];
b = [100; 160;  20];
c= [50; 40; 70; 80];
[xopt,fopt,flag,extra] = linprog(c,A,b,[],[],sense="max",verb=0);

// exact sol, function value and basis information
XM = [12;0;0;4]; FM = 920; final_basis = hash(aux = [1;3;3], str = [1;2;2;1]);

if norm(xopt-XM)/norm(XM) >= 4*%eps then, pause, end
if abs(fopt-FM)/abs(FM) >= 4*%eps then, pause, end
if ~extra.basis_info.equal[final_basis] then, pause, end
if abs(dot(extra.redcosts,xopt)/norm(xopt)) >= 4*%eps then, pause, end
if abs((dot(extra.lambda,b)-fopt)/fopt) >= 4*%eps then, pause, end

// test 1-2: same problem but we provide a given feasible initial basis
basis = hash(aux = [1,1,1], str = [2,2,2,2]);
sopts = hash(basis_info = basis);
[xopt,fopt,flag,extra] = linprog(c,A,b,[],[],sense="max",verb=0,solver_options=sopts);

if norm(xopt-XM)/norm(XM) >= 4*%eps then, pause, end
if abs(fopt-FM)/abs(FM) >= 4*%eps then, pause, end
if ~extra.basis_info.equal[final_basis] then, pause, end
if abs(dot(extra.redcosts,xopt)/norm(xopt)) >= 4*%eps then, pause, end
if abs((dot(extra.lambda,b)-fopt)/fopt) >= 4*%eps then, pause, end

// test 1-3 : same problem but use dual solver
sopts = hash(meth=2);
[xopt,fopt,flag,extra] = linprog(c,A,b,[],[],sense="max",verb=0,solver_options=sopts);
if norm(xopt-XM)/norm(XM) >= 4*%eps then, pause, end
if abs(fopt-FM)/abs(FM) >= 4*%eps then, pause, end
if ~extra.basis_info.equal[final_basis] then, pause, end
if abs(dot(extra.redcosts,xopt)/norm(xopt)) >= 4*%eps then, pause, end
if abs((dot(extra.lambda,b)-fopt)/fopt) >= 4*%eps then, pause, end

// test 1-4 : same problem but use interior point solver
[xopt,fopt,flag,extra] = linprog(c,A,b,[],[],sense="max",verb=0,solver="ipt");
if norm(xopt-XM)/norm(XM) >= 5e-7 then, pause, end
if abs(fopt-FM)/abs(FM) >= 1e-8 then, pause, end
if abs(dot(extra.redcosts,xopt)/norm(xopt)) >= 5e-7 then, pause, end
if abs((dot(extra.lambda,b)-fopt)/fopt) >= 1e-8 then, pause, end

/////////////////////////////////////////////////////////////////////////////
// test 2-1  (unbounded solution)
//   max c'x
//   Ax <= b
//    x >= 0
A= [-1 -1; -1 2; -2 1];
b=[-3; -5; 5];
c=[1; 3];
[xopt,fopt,flag] = linprog(c,A,b,[],[],sense="max",verb=0);
if ~ (fopt.equal[%inf] && flag.equal[1] && isempty(xopt)) then, pause, end

// test 2-2 same problem but using dual simplex
sopts = hash(meth=2);
[xopt,fopt,flag,extra] = linprog(c,A,b,[],[],sense="max",verb=0,solver_options=sopts);
if ~ (isnan(fopt) && flag.equal[3] && isempty(xopt)) then, pause, end

// test 2-3 use simplex directly on the dual LP. An unfeasible LP should
// give flag=2 and gopt is set to +inf (we use the convention min empty_set = +inf).
//  min b'y
//  A'y >= c  <=> - A' y <= -c
//    y >= 0
[yopt,gopt,flag] = linprog(b,-A',-c,[],[],sense="min",verb=0);
if ~ (gopt.equal[%inf] && flag.equal[2] && isempty(xopt)) then, pause, end


/////////////////////////////////////////////////////////////////////////////
// test 3-1 both primal and dual unfeasible
//   min c'x
//     Ae x = be
//       x >= 0
Ae = [1,-1;1,-1]; be = [1;-1];
c = [-1;-1];
[xopt,fopt,flag] = linprog(c,[],[],Ae,be,sense="min",verb=0);
if ~ (fopt.equal[%inf] && flag.equal[2] && isempty(xopt)) then, pause, end

// test 3-2 use dual simplex
sopts = hash(meth=2);
[xopt,fopt,flag] = linprog(c,[],[],Ae,be,sense="min",verb=0,solver_options=sopts);
if ~ (isnan(fopt) && flag.equal[3] && isempty(xopt)) then, pause, end

// test 3-3 use simplex directly on dual LP
//   max be'*y
//       Ae'y <= c
[yopt,gopt,flag] = linprog(be,Ae',c,[],[],lb=[-%inf,-%inf],sense="max",verb=0);
if ~ (gopt.equal[-%inf] && flag.equal[2] && isempty(yopt)) then, pause, end


//////////////////////////////////////////////////////////////////////////////
// tests on easy mips (from netlib)

// bal8x12.mps is a mip with x >= 0 (no need to provide lb=0)
[c,A,b,Ae,be,sense,lb,ub,binprog,intprog,var_type] = readlp("bal8x12.mps",verb=0);
Fe = 471.55; 
[xopt,fopt,flag] = linprog(c,A,b,Ae,be,ub=ub,var_type=var_type,verb=0);
if abs((fopt-Fe)/Fe) >= 4*%eps then, pause, end

// gr4x6.mps is a mip with x >= 0 (no need to provide lb=0)
[c,A,b,Ae,be,sense,lb,ub,binprog,intprog,var_type] = readlp("gr4x6.mps",verb=0);
Fe = 202.35; 
[xopt,fopt,flag] = linprog(c,A,b,Ae,be,ub=ub,var_type=var_type,verb=0);
if abs((fopt-Fe)/Fe) >= 4*%eps then, pause, end

// bk4x3.mps is a mip with x >= 0 (no need to provide lb=0)
[c,A,b,Ae,be,sense,lb,ub,binprog,intprog,var_type] = readlp("bk4x3.mps",verb=0);
Fe = 350.0; 
[xopt,fopt,flag] = linprog(c,A,b,Ae,be,ub=ub,var_type=var_type,verb=0);
if abs((fopt-Fe)/Fe) >= 4*%eps then, pause, end


