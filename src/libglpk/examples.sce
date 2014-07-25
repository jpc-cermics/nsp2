// example 1 
//-----------

c= [50; 40; 70; 80]; 
A = [ 2 4 8  6; 
      10 8 6 10; 
      1 1 2  2]; 
b = [100; 160;  20]; 


[xopt,fopt,flag,extra] = linprog(c,A,b,[],[],sense="max");
// Take care that default value for lb is 0 and ub = %inf 

// check optimality conditions 
L=extra.lambda;
T= and(-c + A'*L >= 0) && (-c+A'*L)'*xopt == 0 && L'*(A*xopt-b) == 0 
if ~T then pause;end 
// check optimal cost and dual cost 
if L'*b <> c'*xopt then pause;end
// check optimal cost
if fopt <> c'*xopt then pause;end 
if fopt <> 920 then pause;end 


[xopt1,fopt1,flag1,extra1] = linprog_clp(c,A,b,[],[],sense="max");
if norm(xopt-xopt1) >= 1.e-8 then pause;end 
if norm(extra1- extra.lambda) >= 1.e-8 then pause;end 

// example 2 
//---------- 
c = -[6 5];                
A = [1,4; 6,4; 2,-5];      
b = [16;28;6];    
lb = [0;0];                
ub = [10;10];

[xopt,fopt,flag,extra] = linprog(c,A,b,[],[],ub=ub,lb=lb,sense="min");
[xopt1,fopt1,flag1,extra1] = linprog_clp(c,A,b,[],[],ub=ub,lb=lb,sense="min");
if norm(xopt-xopt1) >= 1.e-8 then pause;end 
if norm(extra1- extra.lambda) >= 1.e-8 then pause;end 

// example 3
//------------
c = -[1 2 3];
A = [-1 , 1 , 1; 
     1 , -3 , 1];
Ae=[1  1  1];
b = [20;30];
be=40;
lb = [0;0;0];
ub = [40;%inf;%inf];

[xopt,fopt,flag,extra] = linprog(c,A,b,Ae,be,ub=ub,lb=lb,sense="min");
[xopt1,fopt1,flag1,extra1] = linprog_clp(c,A,b,Ae,be,ub=ub,lb=lb,sense="min");

if norm(xopt-xopt1) >= 1.e-8 then pause;end 
if norm(extra1- extra.lambda) >= 1.e-8 then pause;end 
