// -*- Mode: scilab -*- 
// load everything you need for testing scicos 

nsp = getenv('SCI');
A=glob(nsp+'/macros/blocks/*/*.bin');
for i=1:size(A,1),load(A(i,1));end; 

A=glob(nsp+'/macros/scicos/*.bin');
for i=1:size(A,1),load(A(i,1));end; 

A=glob(nsp+'/macros/xdess/*.sci');
for i=1:size(A,1),exec(A(i,1));end; 
