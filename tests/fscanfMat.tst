// Copyright ENPC
// test of put and get matrix 
// --------------------------------
// FIXME: get_matrix when the put_matrix used a sep = ';' 

n=50;
a=rand(n,n,'u');
// now the data 
fd=fopen('TMPDIR/Mat',mode='w');
texte=['Some text ';'Some more text'];
for t=texte 
  fprintf(fd,'%s\n',t);
end 
for i=1:n ,
  for j=1:n, fprintf(fd,'%5.2f ',a(i,j));end;
  fprintf(fd,'\n');	
end
fd.close[];

fd=fopen('TMPDIR/Mat',mode='r');
a1=fd.get_matrix[];
fd.close[];
if max(a1-a) > 1.e-1 then pause,end 

fd=fopen('TMPDIR/Mat',mode='r');
[a1,txt]=fd.get_matrix[];
fd.close[];

if ~and(txt==texte) then pause,end 
if max(a1-a) > 1.e-1 then pause,end 

fd=fopen('TMPDIR/Mat',mode='r');
[a1,txt]=fd.get_matrix[format='%5.2f'];
fd.close[];

if ~and(txt==texte) then pause,end 
if max(a1-a) > 1.e-1 then pause,end 

// ---- test with put_matrix
a=rand(n,n,'u');
fd=fopen('TMPDIR/Mat',mode='w');
// options 
// fd.put_matrix[a,format='%5.2f',title="// poo",sep=' '];
fd.put_matrix[a]
fd.close[];

fd=fopen('TMPDIR/Mat',mode='r');
a1=fd.get_matrix[];
fd.close[];

if ~and(txt==texte) then pause,end 
if max(a1-a) > 1.e-1 then pause,end 

// ---- test with put_matrix 
a=rand(n,n,'u');
fd=fopen('TMPDIR/Mat',mode='w');
text="// my title ";
fd.put_matrix[a,format='%5.2f',title=texte,sep=' '];
fd.close[];

fd=fopen('TMPDIR/Mat',mode='r');
[a1,txt]=fd.get_matrix[];
fd.close[];

if ~and(txt==texte) then pause,end 
if max(a1-a) > 1.e-1 then pause,end 








