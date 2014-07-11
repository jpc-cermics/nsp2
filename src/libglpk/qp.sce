// basic test for quadratic programming 
//     Maximize
//       obj: x1 + 2 x2 + 3 x3
//              - 0.5 ( 33x1*x1 + 22*x2*x2 + 11*x3*x3
//                   -  12*x1*x2 - 23*x2*x3 )
//      Subject To
//       c1: - x1 + x2 + x3 <= 20
//       c2: x1 - 3 x2 + x3 <= 30
//      Bounds
//       0 <= x1 <= 40
//      End
// this is a test case a cplex 

c=[1,2,3];
A=sparse([-1,1,1;1,-3,1]);
b=[20;30];

lb=[0,-%inf,-%inf];
ub=[40,%inf,%inf];

Q= -[ 33,-06,00;
      -06,22,-23/2;
      00,-23/2,11];

[xopt1,fopt1,flag1,extra1] = linprog_cplex(c,A,b,sparse([]),[],ub=ub,lb=lb,sense="max",Q=Q,loglevel=0);

if (fopt1- 2.015617) > 1.e-5 then pause;end
if xopt1(1) - 0.139115 > 1.e-5 then pause;end
if xopt1(2) - 0.598465 > 1.e-5 then pause;end
if xopt1(3) - 0.898396 > 1.e-5 then pause;end
if extra1(1) - 18.642254 > 1.e-5 then pause;end
if extra1(2) - 30.757886 > 1.e-5 then pause;end

if exists('quapro') then 
  [xopt2,lambda2,f2,info2] = quapro(-Q,-c,full(A),b,lb,ub,0);
end


[xopt1,fopt1,flag1,extra1] = linprog_clp(c,A,b,sparse([]),[],ub=ub,lb=lb,sense="max",Q=Q);
[xopt1,fopt1,flag1,extra1] = linprog_coinmp(c,A,b,sparse([]),[],ub=ub,lb=lb,sense="max");

