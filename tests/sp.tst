// test of sparse redim 

a=testmatrix('magic',10);
for i=10:20:30;
 a(a>10)=0;
 asp=m2sp(a);
 a1=spredim(asp,2,50);
 a2=spredim(a1,20,5);
 a3=spredim(a2,10,10);
 maxi(abs(a-sp2m(a3)));
end

