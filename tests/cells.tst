// avec C{1,2} on peut aussi faire C{1,2}(2,2) = 3 etc.....
// // dans un premier temps 
// // on peut implemter .obj 
// // t.q 
// C(A,B).obj <=> C{A,B} 

A={8,9,rand(4,4)} // creation 

// action au niveau des objets stockes 
A{1}	// extraction de l'element 1 
A{1:2}=(rand(4,4),78) // changer les elts 1 et 2 
[a,b]=A{1:2} // extraction de 2 elements 
A{1}(2,2)=5 ; // changer l'element 1 
A{5} = 6; // Attention pas de Warning et il se passe rien 
	  // doit-on considerer que la taille de la cell 
	  // doit croitre ? 

A{:} // extrait tous les elements (utile dans f(A{:}) ? ) 
A{:} = (4,5,6) // affectation 
A{:} = (4,5,6,7) // affectation le 7 est oubli� 
A{:}=(4,6) // XXX pb detect� mais un print en trop ..
A{70} // renvoit rien si pas d'objet mais qu'on est ds les bounds 
	// .... faut-il plutot renvoyer None

// quand D n'existe pas 
// --------------------
D{:}=8 // et D est vide c'est coherent avec D(:)=8 
	// qui fait pareil 

// Dans matlab quand la taille de la cell croit 
// les elts non existant sont mis a [] 
// ce qui est pas forcement logique. 

function y=f(varargin);y=length(varargin);endfunction;
f(A{:}) // r�pond 3 les arguments sont mis sur la pile 

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
// interet a impl�menter le {} 
// A{:} pourrait metre les elts associ�s 
// sur le stack ce qui peut-etre utile ds un appel de 
// fonction f(A{:}) au lieu de f(A(1),A(2)) 
// de meme si on veut affecter A avec une fonction qui renvoit deux 
// valeurs A{:} = f()

// Pour les listes L(:) remplit d�j� le role de {:}  
// On pourrait imaginer le meme comportement que pour les cells 
// mais on s'eloigne de la syntaxe Scilab .....
// �a donne un truc un peu incoherent 
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
// nomm�s. 
// {H(:)} transforme une hash en cell (on perds les noms) 
// H.__keys donne la table des noms 



