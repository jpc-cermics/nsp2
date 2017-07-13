function [K,r]=obscont(P,Kc,Kf)
  //Returns the observer-based controller associated with a 
  //plant P=[A,B,C,D]. The full-state control gain is Kc and filter
  //gain is Kf. A+B*Kc and A+C*Kf are assumed stable.
  // Copyright INRIA
  [A,B,C,D]=abcd(P);
  K=linear_system(A+B*Kc+Kf*C+Kf*D*Kc,-Kf,Kc,zeros(size(Kc,1),size(-Kf,2)),P.X0, ...
                  dom = P.dom,sample = P.dt)
  if nargout <= 1 then r=[];return;end
  zro=0*Kc*Kf;I1=eye(size(Kc*B));I2=eye(size(C*Kf));
  K=syslin(P(7),A+B*Kc+Kf*C+Kf*D*Kc,[-Kf,B+Kf*D],[Kc;-C-D*Kc],[zro,I1;I2,-D]);
  r=size(D);
endfunction
