function sl=cont_frm(num,den)
  //Controllable state-space form of the transfer num/den
  //!
  // Copyright INRIA
  if type(num,'short')=='linearsys' then
    tf=ss2tf(num);
    num=tf.num;
    den=tf.den;
  end
  if type(num,'short')=='m' then
    if type(den,'short')=='m' then
      sl=syslin([],[],[],[],num ./den,[])
      return
    else
      num(1,1)=poly(num(1,1),den.get_var[],'c'),
    end
  end
  if type(num,'short') <> 'p' then
    error("Error: first argument must be a linear system or a polynom");
    return;
  end
  [ns,ne]=size(num);
  nd=den.degree[];
  // normalization
  dnd=coeff(den,nd);den=den/dnd;num=num/dnd
  // D(s)
  d=0*num;
  for l=1:ns do
    for k=1:ne do
      [nl,dl]=pdiv(num(l,k),den),
      num(l,k)=nl,d(l,k)=dl,
    end,
  end
  if max(d.degree[])==0 then d=coeff(d),end
  //matrices a b and c
  if nd <> 0 then
    den=coeff(den);c=coeff(num,0:nd-1)
    a=[];
    for k=1:nd do a=[a,-den(k)*eye(ne,ne)];end
    a=[0*ones((nd-1)*ne,ne),eye(ne*(nd-1),ne*(nd-1));a];
    b=[0*ones((nd-1)*ne,ne);eye(ne,ne)]
  else
    a=[];b=[];c=[]
  end
  [n,vn]=size(a);
  sl=syslin([],a,b,c,d,0*ones(n,1))
endfunction
