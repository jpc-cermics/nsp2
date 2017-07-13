function [phm,fr]=p_margin(h)
  select type(h,'short')
    case 'r' then %t
    case 'linearsys' then h=ss2tf(h)
    else
      error("Error: first argument must be a linear system or transfer matrix");end
  //
  //[n,d]=h(['num','den']);
  n=h.num;d=h.den;
  w=poly(0,'w')
  // 
  niw=horner(n,%i*w,ttmode = %t);diw=horner(d,%i*w,ttmode = %t)
  w=roots(real(niw*conj(niw)-diw*conj(diw)))
  //recherche des racines reelles positives
  eps=1.E-7
  ws=w(find((abs(imag(w)) < eps) & (real(w) > 0)))
  if isempty(ws) then phm=[],fr=[],return,end
  //
  f=freq(n,d,%i*ws);
  phm=atan(imag(f),real(f))
  phm=180*phm(:)/%pi
  fr=real(ws)/(2*%pi)
endfunction
