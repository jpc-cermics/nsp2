// -*- Mode: scilab -*- 

// Quelques tests qui doivent marcher 

L=list(1,2,3)

L(1) = 10
L(3:5) = (4,5,6)
// Corriger le XXX de Eval
//L(:) = (....)

// plus complique 

[x,L(1:2),z]=(1,67,68,2) 

// tlist : are implemented as hash tables 
//         the type is stored as .type
L=tlist(['a','b','c','d'],20,3,4);
L.b = 78 
L('c') = 10 

L(['c','d'])=( 10,20) 

 



