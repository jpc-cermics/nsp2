function s1=cls2dls(s,t,fp)
  //Syntax : s1=cls2dls(sl,t [,fp])
  //
  // Given sl=[a,b,c,d] (syslin list) continuous time system, cls2dls
  // returns the sampled system obatined by the bilinear transform
  // s=(2/t)*(z-1)/(z+1).
  //
  // t sampling period
  // fp prevarping frequency in hertz
  //!
  // Copyright INRIA
  if type(s,'short') <> 'linearsys' then
    error("Error: expecting a linear system");
  end
  dom=s.dom;
  if s.dom=='u' then
    printf("warning: system assumed to be continuous time\n");
    dom='c';
  end
  if ~(dom=='c') then
    printf('warning: needs a continuous system as input.\n');
  end
  fs=1/t
  if nargin==3 then fp=2*%pi*fp;fs=fp/tan(fp/fs/2)/2,end;//prewarping
  a=2*fs*eye(size(s(2)))-s(2)
  ad=a\(2*fs*eye(size(s(2)))+s(2));
  b=(ad+eye(size(ad)))/a*s(3);
  d=s(5)+s(4)/a*s(3)
  s1=syslin('d',ad,b,s(4),d,s(6))
endfunction
