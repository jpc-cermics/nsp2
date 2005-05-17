N=10000;

B=(1:1000);
timer();
for i=1:N;A=B;del1(A,10:10:1000);end;
for i=1:N;A=B;del1(A,1:100);end;
for i=1:N;A=B;del1(A,100:500);end;
t1=timer()

B=(1:1000);
timer();
for i=1:N;A=B;del3(A,10:10:1000);end;
for i=1:N;A=B;del3(A,1:100);end;
for i=1:N;A=B;del3(A,100:500);end;
t3=timer()

B=(1:1000);
timer();
for i=1:N;A=B;del4(A,10:10:1000);end;
for i=1:N;A=B;del4(A,1:100);end;
for i=1:N;A=B;del4(A,100:500);end;
t4=timer()

[t1,t3,t4]
