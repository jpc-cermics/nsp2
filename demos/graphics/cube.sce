S=[1,-1,-1
   1,1,-1
   -1,1,-1
   -1,-1,-1
   1,1,1
   -1,1,1
   1,-1,1
   -1,-1,1];

function F=faces(i)
  F=[S(4,i),S(3,i),S(2,i),S(1,i)
     S(2,i),S(3,i),S(6,i),S(5,i)
     S(4,i),S(8,i),S(6,i),S(3,i)
     S(1,i),S(7,i),S(8,i),S(4,i)
     S(1,i),S(2,i),S(5,i),S(7,i)
     S(5,i),S(6,i),S(8,i),S(7,i)];
  // reverse the orientation
  F=F(:,4:-1:1);
endfunction

Fx=faces(1);
Fy=faces(2);
Fz=faces(3);

Fx(2:$-1,:) = Fx(2:$-1,:)*1.2;
Fy(2:$-1,:) = Fy(2:$-1,:)*1.2;
Fz(2:$-1,:) = Fz(2:$-1,:)*1.2;
Fz(1,:) = Fz(1,:)-0.4;
Fz(6,:) = Fz(6,:)+0.4;

// 4 is hidden3D color 

colors=[1,2,3,5,6,9];
plot3d(Fx',Fy',Fz',colors=colors);

// shade 
//plot3d(Fx',Fy',Fz',colors=ones(4,1)*colors);


