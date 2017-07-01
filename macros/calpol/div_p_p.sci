function f=div_p_p(p1,p2)
  // p1/p2 <=> f= p1*(p2^(-1)) 
  // p1 polynomial matrix
  // p2 polynomial matrix
  //!
  // Copyright INRIA
  if size(p2,'*') <>1 then 
    f=p1*inv(p2)
  else
    [p1,p2]=simp(p1,p2*ones(size(p1)));
    f= p2r(p1,p2);
  end
endfunction
