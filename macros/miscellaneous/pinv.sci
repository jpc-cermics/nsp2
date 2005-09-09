function [B] = pinv(A,tol=[])

// computes the pseudo inverse of the matrix A
// rmk: uses at the end the new .* feature
   
  if type(A,"short")~="m" then
    error("first arg must be a matrix of numbers"), 
  end
  
  if tol == [] then, tol = max(size(A))*%eps, end
  
  [m,n] = size(A)
  
  [U,s,V] = svd(A,mode="e")
  if s(1) == 0 then
     B = zeros(n,m)
  else
     k = max(find(s >= s(1)*tol))
     si = 1 ./s(1:k)
     if k < min(m,n) then
	B = V(:,1:k).*si'*U(:,1:k)'
     else
	B = V.*si'*U'
     end
  end
  
endfunction
