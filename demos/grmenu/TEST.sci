A=glob('*.bin');
for i=1:size(A,1),load(A(i,1));end; 
exec('../scicos/00util.sci');



