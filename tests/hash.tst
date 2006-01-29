// -*- Mode: scilab -*- 
// il faudrait integrer ce aui suit ds le code 
// avec des mechanismes d'extraction 
// ---------------------------------------------
// test des tables de Hashage 
// C'est un archetype d'objet simple 
// pour lequel on dispose de 
// methodes H.methode[args] <=> $methode(H,arg1,...) 
// et des attributs 
// Points en suspens 
//   les methodes sont des fonctions comme les autres 
//   on peut en rajouter dynamiquement en creant des fonctions 
//   mais attention tout se passe comme si le premier 
//   argument etait passe par reference 
//
// Les attributs sont controles par l'operateur dot 
// qui est en dur pour une table de hashage 
// peut-etre changer son nom en retirant le dollar 
// qui indique les methodes 
//
// 

h = hcreate(10) 
h.enter[a=[20,30],b='foo']
h1= hcreate(10);
h1.enter[h,b=[20,30],c='foo'] 
// XXX bug, quand h.find trouve un objet il faut 
// qu'il le copie sans nom  ds la pile 
[a,b]=h.find[['a','b']];
// ok 
[x,y]=h.find[['a','b']];
[a,b]=h.find['a','b'];
c= h1.find['h'].find['b']
c= h1.h.b
info(h)
print(h);
h2= hcreate(4)
h2.enter[d=90,f=90]
//merge(h1,h2)
h1.merge[h2]
h1.__keys 
h1.__attrs
h.a
h('a')
h('a')=90
h.a = 78 
h.delete[['a','b']]  
h.delete['a','b'] 
h.a=[10,20]

// PathExtract est utilise ici 
// i.e quand un chemin doit etre 
// utilise dans un membre gauche d'affectation 
// cette operation est interne car elle se fait avec 
// passage par reference 

h.a(1,1)=100 ;
// rajout d'une <<methode>>
function H=$meth_h(H,x)
 $enter_h(H,a=x)
endfunction

// changement de la fonction qui gere les attributs 
// noter que ca ecrase la fonction de base 
function x=$dot_h(H,poo) 
   info(H)
endfunction 

// test of collisions 

h=hcreate(abcd=1,abdc=2,adbc=3);
h.delete['abcd'];
if h.iskey['adbc']== %f then pause;end 
if length(h)<>2 then pause;end 
h.abcd = 1;
if length(h)<>3 then pause;end 
h.delete['abdc'];
if length(h)<>2 then pause;end 
h.abdc=2;
if length(h)<>3 then pause;end 
if h.abdc <> 2 then pause;end 
if h.adbc <> 3  then pause;end 







				
