// -*- Mode: scilab -*- 

//-------------- find mat -------------------

As=sprand(40,50,0.3);
A=full(As);

[I,J,v]=find(A);
IJ=find(A);
[I1,J1]=find(A);
if ~I1.equal[I] then pause;end 
if ~J1.equal[J] then pause;end 
IJ1=sort((I1-1)+ size(A,'r')*(J1-1)+1);
if ~IJ1.equal[sort(IJ)] then pause;end 
if norm(A-full(sparse([I;J]',v,size(A)))) > 10*%eps then pause;end 

//-------------- find bmat -------------------

[I1,J1]=find(A<>0);
if ~I1.equal[I] then pause;end 
if ~J1.equal[J] then pause;end 
IJ1=find(A<>0);
if ~IJ1.equal[IJ] then pause;end 


//-------------- find sparse -------------------

[I1,J1]=find(As)
if ~I1.equal[I] then pause;end 
if ~J1.equal[J] then pause;end 
IJ1=find(As);
if ~IJ1.equal[IJ] then pause;end 

[I1,J1,V1]=find(As)
if norm(A-full(sparse([I1;J1]',V1,size(A)))) > 10*%eps then pause;end 


