function Gt=gtild(G,flag)
// input:
// G(s): a polynomial matrix or a rational matrix
// represented in state-space or in transfer form
//
// Gt=gtild(G) or  Gt=gtild(G,'c')
// returns Gt = G(-s)' (in transfer form or in state-space)
// for continuous time system G
//or 
// Gt=gtild(G) or  Gt=gtild(G,'d')
// returns Gt = z^n * G(1/z)' (n = maximum degree of G)
// for discrete-time matrix polynomials
//!
// Copyright INRIA

  function mpt=dp_tilde(mp)
  //mp(z) =  polynomial matrix 
  // mpt(i,j)= z^n*conj(mp(j,i))(1/z)
    [m,n]=size(mp);
    z= mp.get_var[];
    //max degree
    nmax=max(mp.degree[]);
    mpt= m2p([],var = z);
    for i=1:m
      for j=1:n
	mpt(j,i)=poly(coeff(conj(mp(i,j)),nmax:-1:0),z,'c')
      end
    end
  endfunction
  
  function mpt=cp_tilde(mp)
  //mp(s) =  polynomial matrix 
  // mpt(i,j)= conj(mp(j,i))(s)
    s=poly(0,(mp.get_var[]));
    pr=real(mp);pi=imag(mp);
    mpt=horner(pr',-s,ttmode=%t);
    if pi==0*s then return;end
    mpt=mpt+%i*horner(pi',-s,ttmode=%t);
  endfunction
    
  typ_G= type(G,'short');
  
  if nargin==1 then
    select typ_G 
     case 'r' then flag= G.dom ;
     case 'linearsys' then flag = G.dom;
     case 'p' then flag= 'u';
    else 
      error("Expecting a polynomial, or rational or linear system argument");
      return;
    end
  end
  if isempty(flag) then flag='u';end 
  
  select typ_G
   case 'p' then 
    select flag 
     case 'c' then Gt=cp_tilde(G);
     case 'd' then Gt=dp_tilde(G);
     case 's' then Gt=dp_tilde(G);
     case 'u' then Gt=cp_tilde(G);
    end
    return;
   
   case 'r'
    v=[G.num,G.den];v=v.get_var[]; s=poly(0,v);
    select flag 
     case 'c' then Gt=horner(G',-s,ttmode=%t);
     case 'd' then Gt=horner(G',1/s,ttmode=%t);
     case 's' then Gt=horner(G',1/s,ttmode=%t);
     case 'u' then Gt=cp_tilde(G);
    end
    return;
   
   case 'linearsys'
    [A,B,C,D]=abcd(G);
    if flag == 'u' then flag = 'c';end 
    if flag == 's' then flag = 'd';end 
    select flag 
     case 'c' then
      if type(D,'short') =='p'
	var = D.get_var[];
	Dp=horner(D,-poly(0,var),ttmode=%t);
	Gt=syslin(flag,-A',-C',B',Dp');
	return;
      else
	Gt=syslin(flag,-A',-C',B',D');
	return
      end
     case 'd' then 
      if type(D,'short') =='p'
	var = D.get_var[];
        Dp=horner(D,1/poly(0,var),ttmode=%t);
	Dp=tf2ss(Dp);
	[A,B,C,D]=abcd(G');
	w=tlist(['des','A','B','C','D','E'],eye(size(A)),B,C,0*C*B,A);
	sl= des2ss(w);
	sl.D.set_var[var];
	z=poly(0,sl.D.get_var[]);zss=-tf2ss(z*eye(size(Dp')));zss.dom='d';
	Gt=zss*sl+Dp';
      else
        [A,B,C,D]=abcd(G');  //lazy way for transposition...
	sl=G';D=sl.D; // get a polynomial 
	var = D.get_var[];
        w=tlist(['des','A','B','C','D','E'],eye(size(A)),B,C,0*D,A);
	sl=des2ss(w);
	sl.D.set_var[var];
	z=poly(0,sl.D.get_var[]);zss=-tf2ss(z*eye(size(D')));zss.dom='d';
	Gt=zss*sl+D;   //-z*C*inv(I-z*A)*B + D
      end
    end
    //
   case 'm'
    Gt=G';
    return;
  end
endfunction

