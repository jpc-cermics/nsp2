// -*- Mode: scilab -*- 

M=rand(4,4);
A={8,9,M,"nsp"}; // creation 

// extraction of elements 
//-----------------------
if A{1}<> 8 then;pause;end 	
if A{4}<> "nsp" then;pause;end 	

[a,b]=A{[1,3]};
if a<>8 then;pause;end 	
if or(b<>M) then;pause;end

// insertion 
//----------

A{1:2}=(M,78) ;// changer les elts 1 et 2 
if A{2}<>78 then;pause;end 
if or(A{1}<>M) then;pause;end 

A{1}=8;
A{1}(2,2)=5 ; // changer l'element 1 
if or(A{1}<>diag([8,5])) then pause;end

A{5} = 6; // Attention pas de Warning et il se passe rien 
	  // doit-on considerer que la taille de la cell 
	  // doit croitre ? 

A{:}; // extrait tous les elements (utile dans f(A{:}) ? ) 
A{:} = (4,5,6,7); // affectation 
A{:} = (4,5,6,7,8); // affectation le 7 est oublié 
A{:}=(4,6); // XXX pb detecté mais un print en trop ..

A={8,9;M,"nsp"}; // creation 
A{1:2,1}=(56,67);
if A{1}<>56 then pause;end 
if A{2}<>67 then pause;end 

A{2,1:2}=(56,67);
if A{2,1}<>56 then pause;end 
if A{2,2}<>67 then pause;end 

A{1:2,1:2}=(1,2,3,4);
if or(A<>{1,2,3,4}) then pause;end 

// A{70} // indice out of bounds 

// quand D n'existe pas 
// --------------------
D{:}=8 // et D est vide c'est coherent avec D(:)=8 
	// qui fait pareil 

// Dans matlab quand la taille de la cell croit 
// les elts non existant sont mis a [] 
// ce qui est pas forcement logique. 

function y=f(varargin);y=length(varargin);endfunction;

A={8,9,M,"nsp"}; // creation 
f(A{:}) // répond 3 les arguments sont mis sur la pile 

// action qui redonnes des cells 
A(1) // extraction d'une sous-cell de cell
A([1:3]) // extraction d'une sous-cell de cell
A([1,3]) = {7,8} // affectation 
A(1,2) = {89} // meme chose finalement que A{1,2} = 89;
A(5)= {78} // noter que la taille de la cell croit 
	   // et la cell peut contenir des elts nuls 
// la bonne question c'est est-ce qu'on peut utiliser {} 
// pour tous les objets et quel sens lui donner ? 

// Pour les matrices comme on confonf scalaires et matrices 
// () et {} avec un argument scalaires sont equivalents 
// par contre avec plusieurs arguments on peut avoir 
// interet a implémenter le {} 
// A{:} pourrait metre les elts associés 
// sur le stack ce qui peut-etre utile ds un appel de 
// fonction f(A{:}) au lieu de f(A(1),A(2)) 
// de meme si on veut affecter A avec une fonction qui renvoit deux 
// valeurs A{:} = f()

// Pour les listes L(:) remplit déjà le role de {:}  
// On pourrait imaginer le meme comportement que pour les cells 
// mais on s'eloigne de la syntaxe Scilab .....
// ça donne un truc un peu incoherent 
// par exemple faire une sous liste d'une liste ne se fait pas 
// naturellement avec des () 
// mais avec list(L([1,2]) par exemple 
// noter que pour une cell on peut toujours faire 
// { A{1,2}} mais A(1,2) est plus naturel 
// noter aussi que c'est pas exactement la meme chose 

A={1,2;3,4}
{ A{:,1}}   // 1x2 
A(:,1)      // 2x1 
 
// Pour les tables de hachage c'est un peu la meme chose 
// la parenthese joue le role de {} et (:) a un sens 
// un peu particulier car il permet les arguments optionnels 
// nommés. 
// {H(:)} transforme une hash en cell (on perds les noms) 
// H.__keys donne la table des noms 

// enlarge size by affectation 
// standard case for cells 

C=cells_create(1,0);C{3}= 6;
if or(size(C)<>[1,3]) then pause;end // OK
if C{3}<>6 then pause;end 
C=cells_create(1,1);C{3}= 6;
if or(size(C)<>[1,3]) then pause;end // OK 
if C{3}<>6 then pause;end 
C=cells_create(1,2);C{3}= 6;
if or(size(C)<>[1,3]) then pause;end // OK 
if C{3}<>6 then pause;end 

C=cells_create(0,0);C{3}= 6;
if or(size(C)<>[1,3]) then pause;end // BUG 
if C{3}<>6 then pause;end 
C=cells_create(0,1);C{3}= 6;
if or(size(C)<>[1,3]) then pause;end // BUG 
if C{3}<>6 then pause;end 
C=cells_create(0,2);C{3}= 6;
if or(size(C)<>[1,3]) then pause;end // BUG 
if C{3}<>6 then pause;end 

C=cells_create(2,0);C{3}= 6;
if or(size(C)<>[3,1]) then pause;end // OK 
if C{3}<>6 then pause;end 
C=cells_create(2,1);C{3}= 6;
if or(size(C)<>[3,1]) then pause;end // OK 
if C{3}<>6 then pause;end 


// XXX we need the same when B is a vector or a matrix 
// 


// enlarge size by affectation 
// path extract case for cells 

A=[];A(2,2)=6;

C=cells_create(1,0);C{3}(2,2)= 6;
if or(size(C)<>[1,3]) then pause;end 
if or(C{3}<>A) then pause;end 
C=cells_create(1,1);C{3}(2,2)= 6;
if or(size(C)<>[1,3]) then pause;end 
if or(C{3}<>A) then pause;end 
C=cells_create(1,2);C{3}(2,2)= 6;
if or(size(C)<>[1,3]) then pause;end 
if or(C{3}<>A) then pause;end 

C=cells_create(0,0);C{3}(2,2)= 6;
if or(size(C)<>[1,3]) then pause;end 
if or(C{3}<>A) then pause;end 
C=cells_create(0,1);C{3}(2,2)= 6;
if or(size(C)<>[1,3]) then pause;end 
if or(C{3}<>A) then pause;end 
C=cells_create(0,2);C{3}(2,2)= 6;
if or(size(C)<>[1,3]) then pause;end 
if or(C{3}<>A) then pause;end 
	
C=cells_create(2,0);C{3}(2,2)= 6;
if or(size(C)<>[3,1]) then pause;end 
if or(C{3}<>A) then pause;end 
C=cells_create(2,1);C{3}(2,2)= 6;
if or(size(C)<>[3,1]) then pause;end 
if or(C{3}<>A) then pause;end 

// two indices 

C=cells_create(0,0);C{3,4}(2,2)= 6;
if or(size(C)<>[3,4]) then pause;end 
if C{3,4}<>6 then pause;end 

// function map 

A=randn(4,5);

function y=myabs(x);y=abs(x);endfunction 
  
C=A.to_cells[];
C1=map(C,myabs);
if norm(abs(A) - ce2m(C1)) > 10*%eps then pause;end 

function R=mapce(C,f)
  R=C;
  for i=1:size(C,'*')
    R(i) = {f(C(i).get[])}
  end
endfunction

function R=mapce1(C,f)
  R=C;
  for i=1:size(C,'*')
    R{i} = f(C{i});
  end
endfunction

C2=mapce(C,myabs);
if ~C1.equal[C2] then pause;end 

C3=mapce(C,myabs);
if ~C1.equal[C3] then pause;end 




  
