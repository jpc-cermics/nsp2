function subplot(m,n,p)
// 
  if nargin == 1 then
    p=modulo(m,10)
    n=modulo((m-p)/10,10)
    m=(m-p-10*n)/100
  end
  j=int((p-1)/n)
  i=p-1-n*j
  xsetech([i/n,j/m,1/n,1/m])
endfunction
