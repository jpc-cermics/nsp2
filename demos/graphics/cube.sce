S=[1,0,0
   1,1,0
   0,1,0
   0,0,0
   1,1,1
   0,1,1
   1,0,1
   0,0,1]

function F=facettes(i)
  F=[S(4,i),S(3,i),S(2,i),S(1,i)
     S(2,i),S(3,i),S(6,i),S(5,i)
     S(3,i),S(6,i),S(8,i),S(4,i)
     S(4,i),S(8,i),S(7,i),S(1,i)
     S(1,i),S(2,i),S(5,i),S(7,i)
     S(5,i),S(6,i),S(8,i),S(7,i)];
endfunction

Fx=facettes(1);
Fy=facettes(2);
Fz=facettes(3);

plot3d(Fx',Fy',Fz',colors=[1:6]);

