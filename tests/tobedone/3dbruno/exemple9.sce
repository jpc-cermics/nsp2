xset("default")
      
S1 = tlist(["string3d","coord","str"],[0;-1;-1],"x,m,m",6,3);
S2 = tlist(["string3d","coord","str"],[0;1;-1],"x,M,m",6,3);
S3 = tlist(["string3d","coord","str"],[0;-1;1],"x,m,M",6,3);
S4 = tlist(["string3d","coord","str"],[0;1;1],"x,M,M",6,3);
S5 = tlist(["string3d","coord","str"],[-1;-1;0],"m,m,z",6,3);
S6 = tlist(["string3d","coord","str"],[-1;1;0],"m,M,z",6,3);
S7 = tlist(["string3d","coord","str"],[1;-1;0],"M,m,z",6,3);
S8 = tlist(["string3d","coord","str"],[1;1;0],"M,M,z",6,3);
S9 = tlist(["string3d","coord","str"],[-1;0;-1],"m,y,m",6,3);
S10= tlist(["string3d","coord","str"],[1;0;-1],"M,y,m",6,3);
S11= tlist(["string3d","coord","str"],[-1;0;1],"m,y,M",6,3);
S12= tlist(["string3d","coord","str"],[1;0;1],"M,y,M",6,3);

T1 = tlist(["string3d","coord","str"],[-1;-1;-1],"0",6,3);
T2 = tlist(["string3d","coord","str"],[1;1;1],"1",6,3);
T3 = tlist(["string3d","coord","str"],[-1;-1;1],"2",6,3);
T4 = tlist(["string3d","coord","str"],[1;1;-1],"3",6,3);
T5 = tlist(["string3d","coord","str"],[1;-1;1],"4",6,3);
T6 = tlist(["string3d","coord","str"],[-1;1;-1],"5",6,3);
T7 = tlist(["string3d","coord","str"],[1;-1;-1],"6",6,3);
T8 = tlist(["string3d","coord","str"],[-1;1;1],"7",6,3);

Lx = tlist(["polyline" "coord" "color" "mark"], [-1 , 1;-1 ,-1;-1 ,-1], 2);
Ly = tlist(["polyline" "coord" "color" "mark"], [-1 ,-1;-1 , 1;-1 ,-1], 2);
Lz = tlist(["polyline" "coord" "color" "mark"], [-1 ,-1;-1 ,-1;-1  ,1], 2);

C = cube();
C.coord = C.coord/2;



listObj=list(C,S1,S2,S3,S4,S5,S6,S7,S8,S9,S9,S10,S11,...
	     S12,T1,T2,T3,T4,T5,T6,T7,T8,Lx,Ly,Lz)
// a coder en dur 
[Gmin, Gmax] = emboite(listObj)
ebox=matrix([Gmin';Gmax'],1,6);
draw3d_objs(listObj,ebox=ebox);

// my_plot3d(list(C,S1,S2,S3,S4,S5,S6,S7,S8,S9,S9,S10,S11,S12,T1,T2,T3,T4,T5,T6,T7,T8,Lx,Ly,Lz),...
	

