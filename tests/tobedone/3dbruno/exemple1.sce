// exemple 1 : un cube ouvert avec des lignes et des points
xset("default")
[C, L, LL, P] = obj1();
S = tlist(["string3d","coord","str"],[1.2;0.5;0.5],"A string !");

listObj = list(C, LL, P, L, S);
[Gmin, Gmax] = emboite(listObj)
ebox=matrix([Gmin';Gmax'],1,6);

draw3d_objs(listObj,ebox=ebox,flag=[0,1,0],with_box=%f,with_mesh=%t);

  
