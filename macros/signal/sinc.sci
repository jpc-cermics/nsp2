function y=sinc(x,fl)
//               [  sin(x(i))/x(i) if x(i)~=0
// computes y(i)=[
//               [  1 if x(i)~=0

  if nargin == 2 then 
    // printf("Warning: deprecated use of sinc, use filt_sinc instead\n");
    y=filt_sinc(x,fl)
    return
  end
  y=ones(x)
  kz=find(x<>0)
  y(kz)=sin(x(kz))./(x(kz));
endfunction

