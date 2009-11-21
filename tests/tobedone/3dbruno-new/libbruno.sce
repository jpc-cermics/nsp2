
function [zcol, zmin, zmax] = color_scaling(z, n1, n2)
   // a function to associate (linearly) a color number
   // to real values... 
   zmin = min(z); zmax = max(z);
   if zmin==zmax then zcol=n1*ones(size(z));return ;end 
   zcol = bsearch(z, linspace(zmin,zmax,n2-n1+1)) + (n1-1)
endfunction

function [C, L, LL, Q] = obj1()
// cube, lines and points 
// the cube as a polyhedron 
  P = [0 1 1 0 0 1 1 0;...
       0 0 1 1 0 0 1 1;...
       0 0 0 0 1 1 1 1];
  
  face = [1 5 1 5 4;...
	  4 6 5 6 8;...
	  3 7 8 7 7;...
	  2 8 4 8 3];
  color = [2 3 4 5 6];
  C = polyhedron_create(Mcoord=P', Mface = face,Mcolor=color, Mback_color=3);
  
  // polyline around the cube 
  P = [-0.1 , 1.1  ,1.1 ,-0.1 ,-0.1;
       -0.1 ,-0.1  ,1.1 , 1.1 ,-0.1;
       0.5  ,0.5  ,0.5  ,0.5  ,0.5];
  color = 1;
  L = polyline3d_create(Mcoord=P',Mcolor=1);
  
  // polyline inside the cube 
  
  P = [ 0.1  0.9  0.9  0.1  0.1;...
	0.1  0.1  0.9  0.9  0.1;...
	0.5  0.5  0.5  0.5  0.5];
  LL = polyline3d_create(Mcoord=P',Mcolor=1); 
  
  // points 
  P = [-0.1  0.5  1.1  0.5  0.5;...
       0.5 ,-0.1  0.5  1.1  0.5;...
       0.5  0.5  0.5  0.5  0.5];
  color = 1;
  mark = 9;
  Q = points3d_create(Mcoord= P',color= color,mark_size=10, mark_type= mark);
endfunction

function [C] = obj2()
//  un carre
  P = [0.1 0.9 0.9 0.1;...
       0.1 0.1 0.9 0.9;...
       0.5 0.5 0.5 0.5];
  face = [4 ; 3 ; 2 ; 1];
  C = polyhedron_create(Mcoord=P', Mface = face,Mcolor=[2], Mback_color=[3]);
endfunction

function [P] = zsurf_to_polyhedron(x, y, z, color_low, color_high, back_color=0,mesh=%t)
//
// Build a polyhedron from a standard plot3d triplet (x,y,z)
// k-th face is = [x_i x_i+1]x[y_j y_j+1] (z(i,j), z(i+1,j), z(i+1,j+1), z(i,j+1))
//   X(nx*(i-1)+j) = X(i,j) = x_i
  if %t then 
    [nx, ny] = size(z)
    [X, Y]=ndgrid(x, y);
    coord = [X(:) , Y(:), z(:)];
    face = zeros_new(4,(nx-1)*(ny-1));
    for j=1:ny-1
      ix = nx*(j-1)+1:nx*j-1
      face(:,(nx-1)*(j-1)+1:(nx-1)*j) = [ix ; ix+1 ; ix+nx+1 ; ix+nx]
    end
  else
    // new way with primitives 
    coord=surf_to_coords(x,y,z);
    face=surf_to_faces(x,y);
  end
  zm = 0.25*(coord(face(1,:),3)+coord(face(2,:),3)+coord(face(3,:),3)+coord(face(4,:),3));
  color = color_scaling(zm, color_low, color_high);
  if back_color == 0 then
    back_color = color
  end
  P=polyhedron_create(Mcoord=coord,Mface=face,Mcolor=color,Mback_color=back_color,mesh=mesh);
endfunction

function [P] = zsurf_to_spolyhedron(x, y, z, color_low, color_high, back_color= -1, mesh=%f )
// Build a spolyhedron from a standard plot3d triplet (x,y,z)
//   face k = [x_i x_i+1]x[y_j y_j+1] (z(i,j), z(i+1,j), z(i+1,j+1), z(i,j+1))
//   
//   X(nx*(i-1)+j) = X(i,j) = x_i
//
  if %f then 
    // old way with nsp code 
    [nx, ny] = size(z)
    [X, Y]=ndgrid(x, y);
    coord = [X(:) , Y(:), z(:)]
    face = zeros_new(4,(nx-1)*(ny-1))
    for j=1:ny-1
      ix = nx*(j-1)+1:nx*j-1
      face(:,(nx-1)*(j-1)+1:(nx-1)*j) = [ix ; ix+1 ; ix+nx+1 ; ix+nx]
    end
    val = z(:);
  else
    // new way with primitives 
    coord=surf_to_coords(x,y,z);
    face=surf_to_faces(x,y);
    val=coord(:,3);
  end
  P=spolyhedron_create(Mcoord=coord,Mface=face,Mval = val,...
		       vmin = min(val), vmax=max(val),...
		       coloutmin= color_low, coloutmax= color_high,...
		       colmin = color_low, colmax = color_high,...
		       back_color=back_color, mesh=mesh, shade =%t );
endfunction

function [P] = psurf_to_spolyhedron(u, v, func_surf, color_low, color_high, back_color=-1,mesh=%f)
// une fonction pour transformer une surface parametrique
// en objet polyedre
  face=surf_to_faces(u,v);
  [U,V]=ndgrid(u,v);
  U.redim[1,-1];
  V.redim[1,-1];
  [x, y, z] = func_surf(U,V);
  coord = [x ; y ; z]';
  val = z(:);
  P=spolyhedron_create(Mcoord=coord,Mface=face,Mval = val,...
		       vmin = min(val), vmax=max(val),...
		       coloutmin= color_low, coloutmax= color_high,...
		       colmin = color_low, colmax = color_high,...
		       back_color=back_color, mesh=mesh, shade=%t);

endfunction

function [P] = psurf_to_polyhedron(u, v, func_surf, color_low, color_high, back_color=0,mesh=%f)
// une fonction pour transformer une surface parametrique
// en objet polyedre
  face=surf_to_faces(u,v);
  [U,V]=ndgrid(u,v);
  U.redim[1,-1];
  V.redim[1,-1];
  [x, y, z] = func_surf(U,V);
  coord = [x ; y ; z]';
  zm = coord(:,3);
  zm = 0.25*(zm(face(1,:))+zm(face(2,:))+zm(face(3,:))+zm(face(4,:)));
  color = color_scaling(zm, color_low, color_high)
  if back_color == 0 then
    back_color = color
  end
  P = polyhedron_create(Mcoord=coord, Mface = face,Mcolor=color, Mback_color=back_color,mesh=mesh);
endfunction

// surface paramétrique 
// les faces sont décrites par (x,y,z) trois matrices de 
// meme taille: les sommets sont obtenus avec
// (i,j),(i+1,j),(i+1,j+1),(i,j+1) 

function [x,y,z] = sphere(theta, phi)
  R = 0.6;
  x = R*sin(phi).*cos(theta)
  y = R*sin(phi).*sin(theta)
  z = R*cos(phi)
endfunction

function [x,y,z] = tore(theta, phi)
  // paramétrisation classique d'un tore de rayons R et r et d'axe Oz
  R = 1; r = 0.2
  x = (R + r*cos(phi)).*cos(theta)
  y = (R + r*cos(phi)).*sin(theta)
  z = r*sin(phi)
endfunction

function [x,y,z] = helice_torique(theta, phi)
  // paramétrisation d'une helice torique
  R = 1; r = 0.3
  x = (R + r*cos(phi)).*cos(theta)
  y = (R + r*cos(phi)).*sin(theta)
  z = r*sin(phi) + 0.5*theta
endfunction

function [x,y,z] = moebius(theta, rho)
  // paramétrisation d'une bande de Moëbius
  R = 1;
  x = (R + rho.*sin(theta/2)).*cos(theta)
  y = (R + rho.*sin(theta/2)).*sin(theta)
  z = rho.*cos(theta/2)
endfunction

function [x,y,z] = tore_bossele(theta, phi)
  // paramétrisation d'un tore dont le petit rayon r est variable avec theta
  R = 1; r = 0.2*(1+ 0.4*sin(8*theta))
  x = cos(theta).*(R + r.*cos(phi))
  y = sin(theta).*(R + r.*cos(phi))
  z = r.*sin(phi)
endfunction

function [D] = dodecahedron()
   t = (sqrt(5)-1)/2
   v = (sqrt(5)+1)/2   
   V = [ 0  0  1  1 ,-1 ,-1  v  t ,-t ,-v ,-t  t  v ,-v  1 ,-1 ,-1  1  0  0;...
        -t  t ,-1  1  1 ,-1  0  v  v  0 ,-v ,-v  0  0  1  1 ,-1 ,-1 ,-t  t;...
	 v  v  1  1  1  1  t  0  0  t  0  0 ,-t ,-t ,-1 ,-1 ,-1 ,-1 ,-v ,-v]

   F = [ 1  1  2  1  3  4 10  6 12 13 14  8;...
	 3  2  4  6 12  7  5 10 11 18 16 15;...
	 7  5  8 11 18 13  9 14 17 19 20 20;...
	 4 10  9 12 13 15 16 17 19 20 19 16;...
	 2  6  5  3  7  8 14 11 18 15 17  9];
   D = polyhedron_create(Mcoord=V', Mface = F,Mcolor=[2:13], Mback_color=[14:25]);
endfunction

function [O] = octahedron()
   //
   // l'octaedre
   //
   V = [1 ,-1  0  0  0  0 ; ...
        0  0  1 ,-1  0  0 ; ...
        0  0  0  0  1 ,-1 ];

   F = [4  4  1  1  5  3  4  2 ; ...
	1  6  3  6  3  6  5  6 ; ...
	5  1  5  3  2  2  2  4 ];
   O = polyhedron_create(Mcoord=V', Mface =F,Mcolor=[2:9], Mback_color=[3]);
endfunction

function [T] = tetrahedron()
   P = [ 1 ,-1 ,-1  1;...
	-1  1 ,-1  1;...
	-1 ,-1  1  1]
   F = [1  1  2  1;...
	3  4  3  2;...
	2  3  4  4];
   T = polyhedron_create(Mcoord=P', Mface =F,Mcolor=[2:5], Mback_color=[3]);
endfunction

function [C] = cube()
   P = [ 1  1 ,-1 ,-1  1  1 ,-1 ,-1;...
	-1  1  1 ,-1 ,-1  1  1 ,-1;...
         1  1  1  1 ,-1 ,-1 ,-1 ,-1];

   F = [1 5 1 2 3 1;... 
	2 8 4 6 7 5;...
	3 7 8 7 8 6;...
	4 6 5 3 4 2];
   C = polyhedron_create(Mcoord=P', Mface =F,Mcolor=[2:7], Mback_color=[3]);
endfunction

function [I] = icosahedron()
   t = (sqrt(5)-1)/2
   v = 1-t;
   
   V = [ v ,-v  0  0  t  t ,-t ,-t  0  v ,-v  0;...
	 0  0  t ,-t ,-v  v  v ,-v ,-t  0  0  t;...
	 t  t  v  v  0  0  0  0 ,-v ,-t ,-t ,-v]
   
   F = [1  3  1  6  2  8  1  3 12  2  4  9 10 11  6 12 11  5 11 10;...
	2  2  4  3  3  4  5  6  7  7  8  5  6  8 10 11  9  9 10 11;...
	4  1  5  1  7  2  6 12  3  8  9  4  5  7 12  7  8 10  9 12]
   I = polyhedron_create(Mcoord=V', Mface =F,Mcolor=[2:21], Mback_color=[1]);
endfunction

function [D] = dino_steph_nsp(mesh=%f)
   load('dinosaure.nsp')
   coord = [x, y,z];
   D=spolyhedron_create(Mcoord=coord,Mface=nodes,Mval = x,...
			vmin = min(x), vmax=max(x),...
			coloutmin= 1 , coloutmax= 1,...
			colmin = 1 , colmax = 128,...
  			back_color= 1, mesh=mesh, shade=%t);
endfunction

function [S] = sphere_bis(C,r,level)   
   //  computes a polyhedronl approximation of a sphere from an icosaedre
   I = icosahedron(); V=I.Mcoord'; F = I.Mface
   for l=1:level
      nv =  size(V,2)
      [E,F2E] = edge_table(F)
      V =  [V , 0.5*(V(:,E(1,:))+V(:,E(2,:)))]
      clear E
      F2E = F2E + nv
      F = [F(1,:)   F(2,:)   F(3,:)   F2E(1,:) ;...
	   F2E(1,:) F2E(2,:) F2E(3,:) F2E(2,:) ;...
	   F2E(3,:) F2E(1,:) F2E(2,:) F2E(3,:) ]
      clear F2E
   end
   // normalisation
   normV = sqrt(sum(V.^2,"r"))
   V = r*V ./ (ones_new(3,1)*normV)
   for i=1:3, V(i,:) = V(i,:) + C(i); end
   S = polyhedron_create(Mcoord=V', Mface =F,Mcolor=[2], Mback_color=[3]);
endfunction

function [S] = sphere_ter(C,r,level, color_low, color_high, back_color, mesh=%f)   
   //  computes a spolyhedron approximation of a sphere from an icosaedre
   I = icosahedron(); V=I.Mcoord'; F = I.Mface
   for l=1:level
      nv =  size(V,2)
      [E,F2E] = edge_table(F)
      V =  [V , 0.5*(V(:,E(1,:))+V(:,E(2,:)))]
      clear E
      F2E = F2E + nv
      F = [F(1,:)   F(2,:)   F(3,:)   F2E(1,:) ;...
	   F2E(1,:) F2E(2,:) F2E(3,:) F2E(2,:) ;...
	   F2E(3,:) F2E(1,:) F2E(2,:) F2E(3,:) ]
      clear F2E
   end
   // normalisation
   normV = sqrt(sum(V.^2,"r"))
   V = r*V ./ (ones_new(3,1)*normV)
   for i=1:3, V(i,:) = V(i,:) + C(i); end
   val = V(3,:)
   valminmax = [min(val), max(val)]
   colminmax = [color_low color_high]
   colout = [color_low, color_high];
   S = spolyhedron_create(Mcoord=V',Mface=F,Mval = val,...
			  vmin = min(val), vmax=max(val),...
			  coloutmin= color_low, coloutmax= color_high,...
			  colmin = color_low, colmax = color_high,...
			  back_color=back_color, mesh=mesh, shade=%t);
endfunction

function [E,F2E] = edge_table(F)
   nf = size(F,2)
   Tab = zeros_new(4,3*nf)
   v = ones_new(1,nf)
   Tab = [F(1,:) F(2,:) F(3,:);...
	  F(2,:) F(3,:) F(1,:);...
	  1:nf   1:nf   1:nf  ;...
	  v      2*v    3*v   ]
   k = find(Tab(1,:) > Tab(2,:))
   Tab(1:2,k) = [Tab(2,k);Tab(1,k)]
   T = Tab(1:2,:)' // ' pour lex_sort
   if exists('%nsp') then 
     [T,k] = gsort(T,'lr','i'); T = T';
   else
     [T,k] = lex_sort(T); T = T';
   end
   E = T(:,1:2:$)
   ne = 3*nf/2
   num_a = matrix([1:ne;1:ne],1,3*nf)
   Tab = [num_a; Tab(3:4,k)];
   [t, k] = sort(-Tab(2,:))
   F2E = Tab(1,k)
   num = matrix(Tab(3,k),3,nf)
   num = num + ones_new(3,1)*(0:3:3*(nf-1))
   F2E(num)  = F2E
   F2E = matrix(F2E,3,nf)
endfunction

function [Csc,Fsc]= fac3d_to_coord(xx,yy,zz,colors)
// faire un polyhedron a partir d'une description xx,yy,zz 
// x=1:3;
// y=5:7;
// z= x'*y ;
// [xx,yy,zz]=genfac3d(x,y,z);
// cette fonction existe en dur sous le nom de facets_to_faces 
//------------------------------------------------------------
  
  [m,n]=size(xx);
  // les coordonnées 
  C = [xx(:),yy(:),zz(:)];
  // seul probleme C contient des redondances 
  [Cs,ks]= sort(C,'ldr','i');
  kp(ks)=1:m*n;
  Fs = matrix(kp,m,n); // les faces au sens polyhedron 
  // on enleve les redondances 
  // mise a jour de Fs 
  Fc= ones(m*n,1);
  k=1;
  for i=2:m*n
    if  and(Cs(i,:) == Cs(i-1,:)) then 
      Fc(i)=k
    else 
      k=k+1;
      Fc(i)=k;
    end
  end
  Fsc = Fc(Fs);
  Fsc.redim[m,n];
  // on enleve les doublons dans Cs 
  Id=find(Fc(2:$)-Fc(1:$-1)==0)+1;
  Csc= Cs;
  Csc(Id,:)=[];
  // vérification de la compression.
  if %f then 
    for i=1:size(Fs,'c') 
      if norm( Csc(Fsc(:,i),:) - Csc(Fsc(:,i),:)) <> 0 then pause,end
    end
  end
endfunction
  
  
  
  
  
  
  
  

