
// prepare data to be reloaded by sci_load 
// this file is to be executed byscilab !!

A=1:10;A=matrix(A,2,5);
Ac=(1:10)+%i*(10:-1:1);
Asp=A; Asp(A>=5)=0; Asp=sparse(Asp);
Aspc=Ac; Aspc(A>=5)=0; Aspc=sparse(Aspc);
Ab=A>=4;
As=string(A);
L=list(A,As,Ab);
T=tlist(['poo','A','B'],A,Ab);
save('sci_data_saved.dat',A,Ac,Asp,Aspc,Ab,As,L,T);
