function []=zgrid()
//
// Copyright INRIA
  xsetech(frect=1.1*[-1,-1,1,1],iso=%t,axesflag=0);
  xtitle( ['loci with constant damping and constant frequencies';...
	   'in discrete plane'],' ',' ');
  xsi=0:0.1:1;
  //roots of s^2+2*xsi*w0*s+w0^2
  //given by : w0*(-xsi+-%i*sqrt(1-sxi*xsi))
  raci=((0:0.05:1)*%pi)'*(-xsi+%i*sqrt(ones_new(size(xsi))-xsi.*xsi))
  // continuous --> discrete
  raci=exp(raci);
  [mr,nr]=size(raci);
  for l=1:nr,
    xstring(real(raci(mr-10,l)),-imag(raci(mr-10,l)),' '+string(xsi(l)),0,0);
  end
  plot2d(real(raci),imag(raci),style=1*ones_new(1,nr));
  plot2d(real(raci),-imag(raci),style=1*ones_new(1,nr));
  //
  w0=(0:0.1:1)*%pi; //
  e_itheta=exp(%i*(%pi/2:0.05:%pi)')
  zw=exp(e_itheta*w0);[mz,nz]=size(zw)
  for l=1:nz,
    xstring(real(zw(1,l)),imag(zw(1,l)),' '+string(w0(l)/(2*%pi)),0,0);
  end
  //-- the curves
  plot2d(real(zw),imag(zw),style=1*ones_new(1,nr));
  plot2d(real(zw),-imag(zw),style=1*ones_new(1,nr));
endfunction
