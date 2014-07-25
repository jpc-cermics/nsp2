// We code here in nsp the problems used for coinmp test
//-----------------------------------------------------

objectConst = 0.0;
n=8;
m=5;
c = ones(1,8);
lb = 0*ones(1,n);
ub = 1000000*ones(1,n);
// char rowType[5] = [ 'L', 'L', 'L', 'L', 'L' ];
b = [14., 80., 50., 50., 50.];
beg=[0,2,4,6,8,10,11,12,14];
count=[2,2,2,2,2,1,1,2];
ind=[0,4,0,1,1,2,0,3,0,4,2,3,0,4];
val=[3., 5.6, 1., 2., 1.1, 1., -2., 2.8, -1., 1., 1., -1.2, -1., 1.9];
A=spfrommtlb(beg,ind,val,[m,n]);

optimalValue = 1428729.2857143;

[xopt,fopt,flag,extra] = linprog(c,A,b,[],[],ub=ub,lb=lb,sense="max");
if abs(fopt - optimalValue) > 1.e-7 then pause;end

[xopt1,fopt1,flag1,extra1] = linprog_clp(c,A,b,sparse([]),[],ub=ub,lb=lb,sense="max");
if abs(fopt1 - optimalValue) > 1.e-7 then pause;end

[xopt1,fopt1,flag1,extra1] = linprog_coinmp(c,A,b,sparse([]),[],ub=ub,lb=lb,sense="max");
if abs(fopt1 - optimalValue) > 1.e-7 then pause;end

[xopt1,fopt1,flag1,extra1] = linprog_cplex(c,A,b,sparse([]),[],ub=ub,lb=lb,sense="max");
if abs(fopt1 - optimalValue) > 1.e-7 then pause;end

pause zzz;

// "Bakery"
// --------

n=2;
m=3;
cte=  - 4000.0 / 30.0;
c =[ 0.05 , 0.08];
lb =[ 0, 0 ];
ub =[ 1000000, 1000000 ];
b =[1400 , 8000 , 5000 ];

beg =  [ 0 , 2, 4 ];
ind =  [ 0, 1, 0, 2];
val =  [ 0.1, 1, 0.2, 1];
A=spfrommtlb(beg,ind,val,[m,n]);

optimalValue = 506.66666667 -cte ;

[xopt,fopt,flag,extra] = linprog(c,A,b,[],[],ub=ub,lb=lb,sense="max");
if abs(fopt - optimalValue) > 1.e-7 then pause;end

[xopt1,fopt1,flag1,extra1] = linprog_clp(c,A,b,sparse([]),[],ub=ub,lb=lb,sense="max");
if abs(fopt1 - optimalValue) > 1.e-7 then pause;end

[xopt1,fopt1,flag1,extra1] = linprog_coinmp(c,A,b,sparse([]),[],ub=ub,lb=lb,sense="max");
if abs(fopt1 - optimalValue) > 1.e-7 then pause;end

[xopt1,fopt1,flag1,extra1] = linprog_cplex(c,A,b,sparse([]),[],ub=ub,lb=lb,sense="max");
if abs(fopt1 - optimalValue) > 1.e-7 then pause;end

// Afiro
//-------
n = 32;
m = 27;
sense = "min";
c =[0, -0.4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -0.32, 0, 0, 0, -0.6, ... 
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -0.48, 0, 0, 10];
lb = zeros(1,n);
ub = %inf*ones(1,n);

ct = ['E', 'E', 'L', 'L', 'E', 'E', 'L', 'L', 'L', 'L', 'E', 'E', 'L', ...
      'L', 'E', 'E', 'L', 'L', 'L', 'L', 'L', 'L', 'L', 'L', 'L', 'L', 'L'];

b = [0, 0, 80, 0, 0, 0, 80, 0, 0, 0, 0, 0, 500, 0, 0, 44, 500, 0, ...
		0, 0, 0, 0, 0, 0, 0, 310, 300];
beg = [0, 4, 6, 8, 10, 14, 18, 22, 26, 28, 30, 32, 34, 36, 38, 40, ...
       44, 46, 48, 50, 52, 56, 60, 64, 68, 70, 72, 74, 76, 78, 80, 82, ...
       83];
ind=[0, 1, 2, 23, 0, 3, 0, 21, 1, 25, 4, 5, 6, 24, 4, 5, 7, 24, 4, 5, ...
     8, 24, 4, 5, 9, 24, 6, 20, 7, 20, 8, 20, 9, 20, 3, 4, 4, 22, 5, 26, 10, 11, ...
     12, 21, 10, 13, 10, 23, 10, 20, 11, 25, 14, 15, 16, 22, 14, 15, 17, 22, 14, ...
     15, 18, 22, 14, 15, 19, 22, 16, 20, 17, 20, 18, 20, 19, 20, 13, 15, 15, 24, ...
     14, 26, 15];

val =[-1, -1.06, 1, 0.301, 1, -1, 1, -1, 1, 1, -1, -1.06, 1, 0.301, ...
      -1, -1.06, 1, 0.313, -1, -0.96, 1, 0.313, -1, -0.86, 1, 0.326, -1, 2.364, -1, ...
      2.386, -1, 2.408, -1, 2.429, 1.4, 1, 1, -1, 1, 1, -1, -0.43, 1, 0.109, 1, -1, ...
      1, -1, 1, -1, 1, 1, -0.43, 1, 1, 0.109, -0.43, 1, 1, 0.108, -0.39, 1, 1, ...
      0.108, -0.37, 1, 1, 0.107, -1, 2.191, -1, 2.219, -1, 2.249, -1, 2.279, 1.4, ...
      -1, 1, -1, 1, 1, 1];

optimalValue = -464.753142857;

A=spfrommtlb(beg,ind,val,[m,n]);
Eq=find(ct == 'E');
Ae=A(Eq,:);be=b(Eq);
Lq=find(ct == 'L');
Al=A(Lq,:);bl=b(Lq);

[xopt,fopt,flag,extra] = linprog(c,A,b,[],[],ub=ub,lb=lb,sense=sense);
if abs(fopt - optimalValue) > 1.e-7 then pause;end

[xopt1,fopt1,flag1,extra1] = linprog_clp(c,A,b,sparse([]),[],ub=ub,lb=lb,sense=sense);
if abs(fopt1 - optimalValue) > 1.e-7 then pause;end

[xopt1,fopt1,flag1,extra1] = linprog_coinmp(c,A,b,sparse([]),[],ub=ub,lb=lb,sense=sense);
if abs(fopt1 - optimalValue) > 1.e-7 then pause;end

[xopt1,fopt1,flag1,extra1] = linprog_cplex(c,A,b,sparse([]),[],ub=ub,lb=lb,sense=sense);
if abs(fopt1 - optimalValue) > 1.e-7 then pause;end

// P0033
//--------

n = 33;
m = 15;
sense = "min";
c = [171, 171, 171, 171, 163, 162, 163, 69, 69, 183, 183, 183, ...
     183, 49, 183, 258, 517, 250, 500, 250, 500, 159, 318, 159, 318, 159, 318, 159, ...
     318, 114, 228, 159, 318];
lb= zeros(1,n);
ub= ones(1,n);

b=[1, 1, 1, 1, -5, 2700, -2600, -100, -900, -1656, -335, -1026, -5, -500, -270];

beg=[0, 3, 6, 10, 14, 19, 24, 26, 31, 36, 38, 41, 45, 49, 53, 54, ...
     55, 56, 58, 60, 61, 62, 66, 70, 73, 76, 80, 84, 87, 90, 93, 96, 97, 98];
ind=[0, 8, 9, 0, 12, 13, 0, 5, 6, 9, 0, 5, 6, 7, 1, 5, 6, 10, 11, 1, ...
		5, 6, 8, 9, 1, 14, 2, 5, 6, 10, 11, 2, 5, 6, 8, 9, 3, 4, 3, 10, 11, 3, 5, 6, ...
		11, 3, 5, 6, 9, 5, 6, 8, 9, 3, 4, 4, 12, 13, 12, 13, 13, 13, 5, 6, 10, 11, 5, ...
		6, 10, 11, 5, 6, 11, 5, 6, 11, 5, 6, 8, 9, 5, 6, 8, 9, 5, 6, 9, 5, 6, 9, 5, 6, ...
		7, 5, 6, 7, 14, 14];
val=[1, -300, -300, 1, -300, -300, 1, 300, -300, -300, 1, 300, ...
     -300, -300, 1, 285, -285, -285, -285, 1, 285, -285, -285, -285, 1, -285, 1, ...
     265, -265, -265, -265, 1, 265, -265, -265, -265, 1, -230, 1, -230, -230, 1, ...
     230, -230, -230, 1, 230, -230, -230, 190, -190, -190, -190, 1, -200, -400, ...
     -200, -200, -400, -400, -200, -400, 200, -200, -200, -200, 400, -400, -400, ...
     -400, 200, -200, -200, 400, -400, -400, 200, -200, -200, -200, 400, -400, ...
     -400, -400, 200, -200, -200, 400, -400, -400, 200, -200, -200, 400, -400, ...
     -400, -200, -400];

ctyp = smat_create(1,n,"B");
A=spfrommtlb(beg,ind,val,[m,n]);

Ae=sparse([]);
be =[];
optimalValue = 3089.0;
ctyp = smat_create(1,n,"B"); 

[xopt,fopt,flag] = linprog(c,A,b,Ae,be,binprog=%t,sense=sense);
if abs(fopt - optimalValue) > 1.e-7 then pause;end

// binary variables we use coinmp 
// [xopt1,fopt1,flag1,extra1] = linprog_clp(c,A,b,Ae,be,ub=ub,lb=lb,var_type=ctyp, sense=sense)

// XXX we should not have to give ul and bl 
[xopt1,fopt1,flag1,extra1] = linprog_coinmp(c,A,b,Ae,be,ub=ub,lb=lb,var_type=ctyp, sense=sense);
if abs(fopt1 - optimalValue) > 1.e-7 then pause;end

[xopt1,fopt1,flag1,extra1] = linprog_cplex(c,A,b,Ae,be,var_type=ctyp, sense=sense);
if abs(fopt1 - optimalValue) > 1.e-7 then pause;end

// Exmip1
//--------------------------

n = 8;
m= 5;
// objectname = "z";
sense = "min";
objconst = 0.0;
c=[1, 0, 0, 0, 2, 0, 0, -1];
lb=[2.5, 0, 0, 0, 0.5, 0, 0, 0];
ub=[%inf, 4.1, %inf, %inf, 4, %inf, %inf, 4.3];
// char rtyp[5]= ['G', 'L', 'E', 'G', 'L'];
b=[2.5, 2.1, 4, 1.8, 15];
//drng[5]=[0, 0, 0, -3.2, 12];

beg=[0, 2, 4, 6, 8, 10, 11, 12, 14];
ind=[0, 4, 0, 1, 1, 2, 0, 3, 0, 4, 2, 3, 0, 4];
val=[3, 5.6, 1, 2, 1.1, 1, -2, 2.8, -1, 1, 1, -1.2, -1, 1.9];

ctyp = [ 'C', 'C', 'B', 'B', 'C', 'C', 'C', 'C'];
A=spfrommtlb(beg,ind,val,[m,n]);
Ae=A(3,:);be=b(3);
A=[-A(1,:);A(2,:);-A(4,:);A(5,:)];
b=[-b(1);b(2);-b(4);b(5)];
optimalValue = 3.23684210526;

// no 'B' in linprog 
// we use I with extra bounds 

ctyp1 = [ 'C', 'C', 'I', 'I', 'C', 'C', 'C', 'C'];
ub1=ub;ub1(3:4)=1;
[xopt,fopt,flag] = linprog(c,A,b,Ae,be,var_type=ctyp1,ub=ub1,lb=lb,sense=sense);
if abs(fopt - optimalValue) > 1.e-7 then pause;end

H=hash(LogLevel=0); 

[xopt1,fopt1,flag1,extra1] = linprog_coinmp(c,A,b,Ae,be,ub=ub,lb=lb, ...
					    var_type=ctyp, sense=sense, ...
					    options =H);
if abs(fopt1 - optimalValue) > 1.e-7 then pause;end

[xopt2,fopt2,flag2,extra2] = linprog_cplex(c,A,b,Ae,be,ub=ub,lb=lb, ...
					   var_type=ctyp, sense=sense, ...
					   options =H);
if abs(fopt1 - optimalValue) > 1.e-7 then pause;end

pause xxxx;

// GamsSos1a
// Sos variables XXXX
//----------------------------------

n = 3;
m = 1;
sense = "max"
objconst = 0.0;
c=[0.9, 1.0, 1.1];
lb=[0, 0, 0];
ub=[0.8, 0.6, 0.6];

//drlo[1]=[-1e+37];
//drup[1]=[1.0];

beg=[0, 1, 2, 3];
ind=[0, 0, 0];
val=[1, 1, 1];

// int sosCount = 1;
// int sosNZCount = 3;
// int sosType[1] = [1];
// int sosBegin[1+1] = [0, 3];
// int sosIndex[3] = [0, 1, 2];
	
optimalValue = 0.72;

// GamsSos2a
// Sos variables XXXX
// ----------------------------------

n = 7;
m= 5;
// objectname = "z";
sense = "min";
objconst = 0.0;
c=[0, 0, 0, 0, 0, 1, 1];
lb=[0.0, 0, 0, -1e+37, -1e+37, 0, 0];
ub=[1e+37, 1e+37, 1e+37, 1e+37, 1e+37, 1e+37, 1e+37];

rtyp= ['E', 'E', 'E', 'G', 'G'];
rhs=[1, 0, 0, -1.3, 1.3];

beg = [0, 3, 6, 9, 10, 13, 14, 15];
ind = [0, 1, 2, 0, 1, 2, 0, 1, 2, 1, 2, 3, 4, 3, 4];
val = [1, 1, 1, 1, 2, 2, 1, 3, 3, -1, -1, -1, 1, 1, 1];

// int sosCount = 1;
// int sosNZCount = 3;
// int sosType[1] = [2];
// int sosBegin[1+1] = [0, 3];
// int sosIndex[3] = [0, 1, 2];

optimalValue = 0.0;

// char* probname = "SemiCont";
// --------------------------------

n = 4;
m = 3;
// objectname = "z";
sense = "min";
objconst = 0.0;

c=[0.0, 1.0, 1.0, 0.0];
lb=[2.8, 0.0, 0.0, 0.0];
ub=[10.0, 1e+37, 1e+37, 1e+37];

rtyp= ['L', 'G', 'E'];
b=[8.9, 8.9, 10.0];

beg=[0, 1, 2, 3, 6];
ind=[2, 0, 1, 0, 1, 2];
val=[1, -1, 1, 1, 1, 1];

semiCount = 1;
semiIndex = [1];

optimalValue = 1.1;

A=spfrommtlb(beg,ind,val,[m,n]);
Ae=A(3,:);be=b(3);
Al=[A(1,:);-A(2,:)];bl=[b(1);-b(2)];

//[xopt,fopt,flag,extra] = linprog(c,Al,bl,Ae,be,ub=ub,lb=lb,sense=sense);
[xopt1,fopt1,flag1,extra1] = linprog_coinmp(c,Al,bl,Ae,be,ub=ub,lb=lb,sense=sense,semi_cont=1);

// On force la premiere variable a 0 
lb(1)=0;ub(1)=0;
[xopt,fopt,flag,extra] = linprog(c,Al,bl,Ae,be,ub=ub,lb=lb,sense=sense)//,semi_cont=1);

if abs(fopt - optimalValue) > 1.e-7 then pause;end
if abs(fopt1 - optimalValue) > 1.e-7 then pause;end

