function y=norm(x) 
  // en attendant 
  y=sqrt(sum(x.*x))
endfunction

function [zcol, zmin, zmax] = color_scaling(z, n1, n2)
   // a function to associate (linearly) a color number
   // to real values... 
   zmin = min(z); zmax = max(z);
   if zmin==zmax then zcol=n1*ones(size(z));return ;end 
   zcol = bsearch(z, linspace(zmin,zmax,n2-n1+1)) + (n1-1)
endfunction

function [W, Pa] = param_vue1(G, phi, theta, d)
   nor = -[sin(phi)*cos(theta);...
	   sin(phi)*sin(theta);...
	   cos(phi)];
   W = G - d*nor;
   u = [-sin(theta);cos(theta);0]
   v = -prod_vect(nor, u)
   Pa = [u v nor];
endfunction

function [W, Pa] = param_vue2(G, W, Pa, angle_x, angle_y)
   // rotation d'angle_x , d'axe v 
   R = mat_rot(angle_x, Pa(:,2))
   Pa = R*Pa
   W = G + R*(W-G)
   R = mat_rot(angle_y, Pa(:,1))
   Pa = R*Pa
   W = G + R*(W-G)
endfunction

function [phi, theta] = spherical_angles(W,G)
   nor = W-G
   nor = nor/norm(nor);
   phi = acos(nor(3))
   l = sqrt(nor(1)^2 + nor(2)^2)
   if l > %eps then
      theta = sign(nor(2))*acos(nor(1)/l)
   else
      theta = %pi/4 // an arbitrary value
   end
endfunction

function [R] = mat_rot(theta, n)
   //
   n = n/sqrt(n'*n)
   [nm, im] = max(abs(n))
   ind = 1:3; ind(im)=[]
   u = ones_new(3,1)
   u(im) = -sum(n(ind))/n(im)
   u = u/sqrt(u'*u)
   v = prod_vect(n,u)
   P = [u v n]
   ct = cos(theta); st = sin(theta)
   R = P*[ct -st  0;...
	  st  ct  0;...
	  0    0  1]*P'
endfunction
   
function [Gmin, Gmax] = emboite(listObj)
   Gmin = %inf*[1;1;1];Gmax = -%inf*[1;1;1] 
   for Obj = listObj
      m = min(Obj.coord,"c")
      M = max(Obj.coord,"c")
      Gmin = min([Gmin m],"c")
      Gmax = max([Gmax M],"c")      
   end
endfunction

function [v]=prod_vect(v1,v2)
  // produit vectoriel v = v1 /\ v2
  v = zeros(size(v1))
  v(1,:) = v1(2,:).*v2(3,:) - v1(3,:).*v2(2,:)
  v(2,:) = v1(3,:).*v2(1,:) - v1(1,:).*v2(3,:)
  v(3,:) = v1(1,:).*v2(2,:) - v1(2,:).*v2(1,:)
endfunction

function [C, L, LL, Q] = obj1()
   //
   //  un cube plus des lignes brisées et des points
      
   //  Le cube :
   P = [0 1 1 0 0 1 1 0;...
	0 0 1 1 0 0 1 1;...
	0 0 0 0 1 1 1 1];

   face = [1 5 1 5 4;...
	   4 6 5 6 8;...
	   3 7 8 7 7;...
	   2 8 4 8 3];
   color = [2 3 4 5 6];
   back_color = 3;
   C = tlist(["polyhedron" "coord" "face" "color" "back_color"], P, face, ...
	     color, back_color);
   
   // La ligne autour :
   P = [-0.1  1.1  1.1 ,-0.1 ,-0.1;...
	-0.1 ,-0.1  1.1  1.1 ,-0.1;...
	 0.5  0.5  0.5  0.5  0.5];

   color = 1;
   L = tlist(["polyline" "coord" "color"], P, color);

   // La ligne à l'intérieur (du cube) :
   P = [ 0.1  0.9  0.9  0.1  0.1;...
	 0.1  0.1  0.9  0.9  0.1;...
	 0.5  0.5  0.5  0.5  0.5];

   color = 1;
   LL = tlist(["polyline" "coord" "color" "mark"], P, color);
   
   // Les points
   xset('mark size',10)
   P = [-0.1  0.5  1.1  0.5  0.5;...
	 0.5 ,-0.1  0.5  1.1  0.5;...
	 0.5  0.5  0.5  0.5  0.5];

   color = 1;
   mark = 9;
   Q = tlist(["points" "coord" "color"], P, color, mark);
   
endfunction

function [C] = obj2()
   //
   //  un carre
   P = [0.1 0.9 0.9 0.1;...
	0.1 0.1 0.9 0.9;...
	0.5 0.5 0.5 0.5];
   face = [4 ; 3 ; 2 ; 1];
   color = [2];
   back_color = 3;
   C = tlist(["polyhedron" "coord" "face" "color" "back_color"], P, face, ...
	     color, back_color);
endfunction


function [P] = zsurf_to_polyhedron(x, y, z, color_low, color_high, back_color)
   //
   //   une fonction pour transformer les donnees d'un plot3d basique
   //   en objet polyedre
   //   face k = [x_i x_i+1]x[y_j y_j+1] (z(i,j), z(i+1,j), z(i+1,j+1), z(i,j+1))
   //   
   //   X(nx*(i-1)+j) = X(i,j) = x_i
   //
   if ~exists("back_color","local") then
      back_color = 0
   end
   [nx, ny] = size(z)
   [X, Y]=ndgrid(x, y);
   coord = [X(:)' ; Y(:)'; z(:)'];
   face = zeros_new(4,(nx-1)*(ny-1));
   for j=1:ny-1
      ix = nx*(j-1)+1:nx*j-1
      face(:,(nx-1)*(j-1)+1:(nx-1)*j) = [ix ; ix+1 ; ix+nx+1 ; ix+nx]
   end
   zm = 0.25*(coord(3,face(1,:))+coord(3,face(2,:))+coord(3,face(3,:))+coord(3,face(4,:)));
   color = color_scaling(zm, color_low, color_high);
   if back_color == 0 then
      back_color = color
   end
   P = tlist(["polyhedron" "coord" "face" "color" "back_color"], coord, face, color, back_color)
endfunction

function [P] = zsurf_to_spolyhedron(x, y, z, color_low, color_high, back_color)
   //
   //   une fonction pour transformer les donnees d'un plot3d basique
   //   en objet spolyedre
   //   face k = [x_i x_i+1]x[y_j y_j+1] (z(i,j), z(i+1,j), z(i+1,j+1), z(i,j+1))
   //   
   //   X(nx*(i-1)+j) = X(i,j) = x_i
   //
   if ~exists("back_color","local") then
      back_color = -1
   end
   [nx, ny] = size(z)
   [X, Y]=ndgrid(x, y);
   coord = [X(:)' ; Y(:)'; z(:)']
   face = zeros_new(4,(nx-1)*(ny-1))
   for j=1:ny-1
      ix = nx*(j-1)+1:nx*j-1
      face(:,(nx-1)*(j-1)+1:(nx-1)*j) = [ix ; ix+1 ; ix+nx+1 ; ix+nx]
   end
   val = z(:);
   valminmax = [min(val), max(val)]
   colminmax = [color_low color_high]
   colout = [color_low, color_high];
   P = tlist(["spolyhedron" "coord" "face" "val" "valminmax" "colminmax" "colout" "back_color"], ...
	     coord, face, val, valminmax, colminmax, colout, back_color)
endfunction


function [P] = psurf_to_spolyhedron(u, v, func_surf, color_low, color_high, back_color)
   // une fonction pour transformer une surface parametrique
   // en objet polyedre
   if ~exists("back_color","local") then
      back_color = -1
   end
   nu = size(u,'*')
   nv = size(v,'*')
   face = zeros_new(4,(nu-1)*(nv-1))
   for j=1:nv-1
      iu = nu*(j-1)+1:nu*j-1
      face(:,(nu-1)*(j-1)+1:(nu-1)*j) = [iu ; iu+1 ; iu+nu+1 ; iu+nu]
   end

   [U,V]=ndgrid(u,v);
   U = matrix(U,1,nu*nv);
   V = matrix(V,1,nu*nv);   
   [x, y, z] = func_surf(U,V);
   coord = [x ; y ; z]
   val = z(:);
   valminmax = [min(val), max(val)]
   colminmax = [color_low color_high]
   colout = [color_low, color_high];
   P = tlist(["spolyhedron" "coord" "face" "val" "valminmax" "colminmax" "colout" "back_color"], ...
	     coord, face, val, valminmax, colminmax, colout, back_color)
endfunction

function [P] = psurf_to_polyhedron(u, v, func_surf, color_low, color_high, back_color)
   // une fonction pour transformer une surface parametrique
   // en objet polyedre
   if ~exists("back_color","local") then
      back_color = 0
   end
   nu = size(u,'*')
   nv = size(v,'*')
   face = zeros_new(4,(nu-1)*(nv-1))
   for j=1:nv-1
      iu = nu*(j-1)+1:nu*j-1
      face(:,(nu-1)*(j-1)+1:(nu-1)*j) = [iu ; iu+1 ; iu+nu+1 ; iu+nu]
   end
   [U,V]=ndgrid(u,v);
   U = matrix(U,1,nu*nv);
   V = matrix(V,1,nu*nv);   
   [x, y, z] = func_surf(U,V);
   coord = [x ; y ; z]
   //xm = 0.25*(coord(1,face(1,:))+coord(1,face(2,:))+coord(1,face(3,:))+coord(1,face(4,:)))
   //ym = 0.25*(coord(2,face(1,:))+coord(2,face(2,:))+coord(2,face(3,:))+coord(2,face(4,:)))   
   zm = 0.25*(coord(3,face(1,:))+coord(3,face(2,:))+coord(3,face(3,:))+coord(3,face(4,:)))
   color = color_scaling(zm, color_low, color_high)
   if back_color == 0 then
      back_color = color
   end
   P = tlist(["polyhedron" "coord" "face" "color" "back_color"], coord, face, color, back_color)
endfunction

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
   D = tlist(["polyhedron" "coord" "face" "color" "back_color"], V, F, ...
	     2:13, 14:25);
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

   O = tlist(["polyhedron" "coord" "face" "color" "back_color"], V, F, ...
	     2:9,3)
endfunction

function [T] = tetrahedron()
   P = [ 1 ,-1 ,-1  1;...
	-1  1 ,-1  1;...
	-1 ,-1  1  1]
   F = [1  1  2  1;...
	3  4  3  2;...
	2  3  4  4];
   T = tlist(["polyhedron" "coord" "face" "color" "back_color"], P, F, ...
	     2:5,3)
endfunction

function [C] = cube()
   P = [ 1  1 ,-1 ,-1  1  1 ,-1 ,-1;...
	-1  1  1 ,-1 ,-1  1  1 ,-1;...
         1  1  1  1 ,-1 ,-1 ,-1 ,-1];

   F = [1 5 1 2 3 1;... 
	2 8 4 6 7 5;...
	3 7 8 7 8 6;...
	4 6 5 3 4 2];
   C = tlist(["polyhedron" "coord" "face" "color" "back_color"], P, F, ...
	     2:7,3)
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
   I = tlist(["polyhedron" "coord" "face" "color" "back_color"], V, F, ...
	     2:21,1)
endfunction

function [D] = dino_steph()
   load('dinosaure.dat')
   coord = [x' ; y'; z'];
   D = tlist(["spolyhedron" "coord" "face" "val" "valminmax" "colminmax" "colout" "back_color"], ...
	      coord, nodes, x, [min(x), max(x)], [1 128], [1 1],1)
endfunction

function [D] = dino_steph_nsp()
   load('dinosaure.nsp')
   coord = [x' ; y'; z'];
   D = tlist(["spolyhedron" "coord" "face" "val" "valminmax" "colminmax" "colout" "back_color"], ...
	      coord, nodes, x, [min(x), max(x)], [1 128], [1 1],1)
endfunction
   


function [S] = sphere_bis(C,r,level)   
   //  computes a polyhedronl approximation of a sphere from an icosaedre
   I = icosahedron(); V=I.coord; F = I.face
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
   S = tlist(["polyhedron" "coord" "face" "color" "back_color"], V, F, ...
	     2,3)
endfunction

function [S] = sphere_ter(C,r,level, color_low, color_high, back_color)   
   //  computes a polyhedron approximation of a sphere from an icosaedre
   I = icosahedron(); V=I.coord; F = I.face
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
   S = tlist(["spolyhedron" "coord" "face" "val" "valminmax" "colminmax" "colout" "back_color"], ...
	     V, F, val, valminmax, colminmax, colout, back_color)
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




