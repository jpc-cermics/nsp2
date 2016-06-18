// + 

function r=plus_r_r(r1,r2)
  if size(r1,'*')==1 || size(r2,'*')==1 || size(r1).equal[size(r2)] then 
    [n1,d1]=simp( r1.den.*r2.num + r2.den.*r1.num, r1.den.* r2.den);
    r=p2r(n1,d1);
  else
    error("Error: arguments should have compatible sizes\n");
    return;
  end
endfunction

function r1=plus_r_m(r,m)
  if size(m,'*')==1 || size(r).equal[size(m)] then 
    n=r.num;d = r.den;
    [n1,d1]=simp(n+m.*d,d);
    r1=r; r1.set_num[n1]; r1.set_den[d1];
  else
    error("Error: arguments should have compatible sizes\n");
    return;
  end
endfunction

function r1=plus_m_r(m,r)
  r1=plus_r_m(r,m);
endfunction

function r1=plus_r_p(r,p)
  if size(p,'*')==1 || size(r).equal[size(p)] then 
    n=r.num;d = r.den;
    [n1,d1]=simp(n+p*d,d);
    r1=r; r1.set_num[n1]; r1.set_den[d1];
  else
    error("Error: arguments should have compatible sizes\n");
    return;
  end
endfunction

function r1=plus_p_r(p,r)
  r1=plus_r_p(r,p);
endfunction

// * 

function r=mult_r_r(r1,r2)
  [n1,d1]=simp(r1.num*r2.num,r1.den*r2.den);
  r=p2r(n1,d1);
endfunction

function r1=mult_r_m(r,m)
  [n1,d1]=simp(r.num*m,r.den);
  r1=p2r(n1,d1);
endfunction

function r1=mult_m_r(m,r)
  r1=mult_r_m(r,m);
endfunction

function r1=mult_r_p(r,p)
  [n1,d1]=simp(r.num*p,r.den);
  r1=p2r(n1,d1);
endfunction

function r1=mult_p_r(p,r)
  r1=mult_r_p(r,p);
endfunction

// .* 


// ./

// - 



// ^

// .^


