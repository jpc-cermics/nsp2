function kp=krac2(sl)
  // Copyright INRIA
  select type(sl,'short')
    case 'r' then n=sl.num,d=sl.den;dom=sl.dom;
    case 'linearsys' then sl=ss2tf(sl);n=sl.num,d=sl.den;dom=sl.dom;
    else
      error("Error: first argument must be a linear system or transfer matrix");end

  if ~or(dom==['c','u']) then error('Error: System must be continuous'),end
  if size(n,'*') <> 1 then error("Error: argument should be a siso system");end
  x=[];
  q1=derivat(n ./d);
  s=roots(q1.num);
  //
  for a=s' do
    if abs(imag(a)) <= 10*%eps then
      x=[x;a],
    end,
  end
  //x(x==0)=[]
  if isempty(x) then return,end
  kp=sort(-real(freq(d,n,real(x))))
endfunction
