function M=sylvester_p_p(p,q)
  if size(p,'*')<>1 || size(q,'*') <> 1 then 
    error('polynomial matrices should be of size 1x1\n");
    return;
  end
  if p.degree[]== 0 && q.degree[]== 0 then 
    error('polynomial should not be both of degree zero\n");
  end
  cp = p.coeffs{1}; cp = cp($:-1:1); p0 = zeros(1,p.degree[]-1)
  cq = q.coeffs{1}; cq = cq($:-1:1); q0 = zeros(1,q.degree[]-1)
  
  M = toeplitz([cp(1),q0], [cp,q0]);
  M = [M; toeplitz([cq(1),p0], [cq,p0])];
endfunction


