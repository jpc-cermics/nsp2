// Example 1 

c = -[6 5];
A = [1,4; 6,4; 2,-5]; b = [16;28;6];     // Ax <= b 
lb = [0;0]; ub = [10;10];
[xopt,fopt,flag,extra] = linprog(c,A,b,[],[])
[xopt1,fopt1,flag1,extra1] = clp_linprog(c,A,b,[],[])

// Example 2 

c = -[1 2 3];
A=[-1  1  1;1 -3  1 ];
b=[20;30];
Ae=[1  1  1];
be=[40];
lb = [0;0;0];
ub = [40;%inf;%inf]; 

[xopt,fopt,flag,extra] = linprog(c,A,b,Ae,be,ub=ub,lb=lb)
[xopt1,fopt1,flag1,extra1] = clp_linprog(c,A,b,Ae,be,ub=ub,lb=lb)



