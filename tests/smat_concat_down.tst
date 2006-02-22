// -*- Mode: scilab -*- 
N=500;
timer();
Code="";
for i=1:N
  Code = [Code ; 
	  'pipo';
	  'pipo';
	  'pipo';
	  'pipo';
	  'pipo';
	  'pipo';
	  'pipo';
	  'pipo';
	  'pipo'];
end
t1=timer();

if size(Code)<>[9*N+1,1] then pause,end
if ~and(find(Code=='pipo')==(2:(9*N+1))) then pause,end

timer();
Code="";
for i=1:N
  Code1 = ['pipo';
	   'pipo';
	   'pipo';
	   'pipo';
	   'pipo';
	   'pipo';
	   'pipo';
	   'pipo';
	   'pipo'];
  Code=[Code ;Code1];
end
t2=timer();
timer();

if size(Code)<>[9*N+1,1] then pause,end
if ~and(find(Code=='pipo')==(2:(9*N+1))) then pause,end

timer();
Code="";
for i=1:N
  Code1 = ['pipo';
	   'pipo';
	   'pipo';
	   'pipo';
	   'pipo';
	   'pipo';
	   'pipo';
	   'pipo';
	   'pipo'];
  Code($+1:$+9,:)=Code1;
end

t3=timer();


if size(Code)<>[9*N+1,1] then pause,end
if ~and(find(Code=='pipo')==(2:(9*N+1))) then pause,end

timer();
Code="";
for i=1:N
  Code1 = ['pipo';
	   'pipo';
	   'pipo';
	   'pipo';
	   'pipo';
	   'pipo';
	   'pipo';
	   'pipo';
	   'pipo'];
  Code.concatd[Code1];
end
t4=timer();

if size(Code)<>[9*N+1,1] then pause,end
if ~and(find(Code=='pipo')==(2:(9*N+1))) then pause,end

timer();
Code="";
for i=1:N
  Code.concatd[['pipo';
		'pipo';
		'pipo';
		'pipo';
		'pipo';
		'pipo';
		'pipo';
		'pipo';
		'pipo']];
end
t5=timer();
T=[t1,t2,t3,t4,t5];

if size(Code)<>[9*N+1,1] then pause,end
if ~and(find(Code=='pipo')==(2:(9*N+1))) then pause,end
	
