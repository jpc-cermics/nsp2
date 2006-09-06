// -*- Mode: scilab -*- 

//------------------------------------------- 

m=10;n=20;
A=int(20*rand(m,n));
B=int(20*rand(m,n));
Ai=int(20*rand(m,n));
Bi=int(20*rand(m,n));


C=[A;B];
D=[A',B']';
if or(C<>D)  then pause;end 

C=[A;B+%i*Bi];
D=[A',(B+%i*Bi)']';
if or(C<>D)  then pause;end 

C=[A+%i*Ai;B];
D=[(A+%i*Ai)',B']';
if or(C<>D)  then pause;end 

C=[A+%i*Ai;B+%i*Bi];
D=[(A+%i*Ai)',(B+%i*Bi)']';
if or(C<>D)  then pause;end 


//------------------------------------------- 

m=10;n=20;
A=(int(20*rand(m,n))>=10);
B=(int(20*rand(m,n))>=10);

C=[A;B];
D=[A',B']';
if or(C<>D)  then pause;end 

//------------------------------------------- 

N=500;
timer();
Code=145;
for i=1:N
  Code = [Code ; 
	  134;
	  134;
	  134;
	  134;
	  134;
	  134;
	  134;
	  134;
	  134];
end
t1=timer();

if size(Code)<>[9*N+1,1] then pause,end
if ~and(find(Code==134)==(2:(9*N+1))) then pause,end

timer();
Code=145;
for i=1:N
  Code1 = [134;
	   134;
	   134;
	   134;
	   134;
	   134;
	   134;
	   134;
	   134];
  Code=[Code ;Code1];
end
t2=timer();
timer();

if size(Code)<>[9*N+1,1] then pause,end
if ~and(find(Code==134)==(2:(9*N+1))) then pause,end

timer();
Code=145;
for i=1:N
  Code1 = [134;
	   134;
	   134;
	   134;
	   134;
	   134;
	   134;
	   134;
	   134];
  Code($+1:$+9,:)=Code1;
end

t3=timer();


if size(Code)<>[9*N+1,1] then pause,end
if ~and(find(Code==134)==(2:(9*N+1))) then pause,end


	


N=500;
timer();
Code=145;
for i=1:N
  Code = [Code,134,134,134,134,...
	  134,134, 134, 134, 134];
end
t1=timer();

if size(Code)<>[1,9*N+1] then pause,end
if ~and(find(Code==134)==(2:(9*N+1))) then pause,end

timer();
Code=145;
for i=1:N
  Code1 = [134,134,134,134,...
	   134,134, 134, 134, 134];
  Code=[Code, Code1];
end
t2=timer();
timer();

if size(Code)<>[1,9*N+1] then pause,end
if ~and(find(Code==134)==(2:(9*N+1))) then pause,end

Code=145;
for i=1:N
  Code1 = [134,134,134,134,...
	   134,134, 134, 134, 134];
  Code(:,$+1:$+9)=Code1;
end

t3=timer();
	
T=[t1,t2,t3];

if size(Code)<>[1,9*N+1] then pause,end
if ~and(find(Code==134)==(2:(9*N+1))) then pause,end


