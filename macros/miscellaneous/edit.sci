
function edit(x,varargopt) 
  if ~exists(x,'callers') then return;end 
  M=acquire(x); 
  // we need a way to create unique files
  fd=fopen('TMPDIR/_editvar.txt',mode="w");
  fprint(fd,M,as_read=%t,name=x,color=%f);
  fd.close[];
  editfile('TMPDIR/_editvar.txt');
endfunction
  
