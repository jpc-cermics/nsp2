function [gm,fr]=g_margin(h)
  // Copyright INRIA

  select type(h,'short')
    case 'r' then %t;
    case 'linearsys' then
     h=ss2tf(h)
    else
      error("Error: first argument must be a linear system or transfer matrix");end
  //
  //if h(4)<>'c' then error(93,1),end
  // [n,d]=h(['num','den']);
  n=h.num;
  d=h.den;
  if type(n,'short')=='m' then n=poly(n,d.get_var[],'c'),end
  // get w for which imaginary part is zero
  w=roots(imag(horner(n,%i*poly(0,'w'),ttmode = %t)* ...
               conj(horner(d,%i*poly(0,'w'),ttmode = %t))))
  eps=1.E-7
  ws=[];
  for i=w' do
    if abs(imag(i)) < eps then
      if real(i) < 0 then ws=[ws;real(i)],end
    end,
  end

  if isempty(ws) then gm=%inf,fr=[],return,end

  //
  mingain=real(freq(n,d,%i*ws))
  k=find(mingain < 0)
  if isempty(k) then gm=%inf,fr=[],return,end
  mingain=mingain(k);ws=ws(k)
  gm=-20*log(abs(mingain))/log(10)
  fr=abs(ws/(2*%pi));// choix de la frequence positive
endfunction
