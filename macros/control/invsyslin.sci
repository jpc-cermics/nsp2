function it=invsyslin(t)
  if type(t,'short') <> 'linearsys' then
    error("Error: argument has wrong type, linear system expected");
  end
  [p,m]=size(t(5));
  if p <> m then printf('Warning: non square D matrix\n'),end
  if type(t(5),'short')=='m' then
    d=pinv(t(5));
  else
    d=inv(t(5));
  end
  a=t.A-t.B*d*t.C
  b=t.B*d;
  c=-d*t.C;
  it=linear_system(a,b,c,d,t.X0,dom = t.dom,sample = t.dt);
endfunction
