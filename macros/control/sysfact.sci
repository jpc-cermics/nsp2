function [S,Series]=sysfact(Sys,Gain,flag)
  //For Sys=[A,B,C,D], 
  //if flag=='post' returns S = [A+B*Gain, B , Gain, I]
  //                and  Series = minss(Sys*S)
  //if flag=='pre' returns  S=[A+Gain*C, Gain , C, I]
  //                and  Series = minss(S*Sys)
  if flag=='post' then
    S=linear_system(Sys.A+Sys.B*Gain,Sys.B,Gain,eye(size(Gain*Sys.B)),Sys.X0, ...
                    dom = Sys.dom,sample = Sys.dt);
    //Series=minss(Sys*S);
    D=Sys(5);
    Series=linear_system(Sys.A+Sys.B*Gain,Sys.B,Sys.C+D*Gain,Sys.D,Sys.X0,dom = 'c');
  end
  if flag=='pre' then
    S=linear_system(Sys.A+Gain*Sys.C,Gain,Sys.C,eye(Sys.C*Gain),Sys.X0,dom = Sys.dom, ...
                    sample = Sys.dt)
    //Series=minss(S*Sys);
    D=Sys(5);
    Series=linear_system(Sys.A+Gain*Sys.C,Sys.B+Gain*D,Sys.C,Sys.D,Sys.X0,dom = 'c');
  end

  //Example:
  //Sys=ssrand(3,2,4);Sys.D')=rand(3,2);
  //S=sysfact(Sys,lqr(Sys),'post');
  //ww=minss(Sys*S);
  //ss2tf(gtild(ww)*ww),Sys.D')'*Sys.D')

  //Syst=Sys';
  //S1=sysfact(Syst,lqe(Syst),'pre');
  //ww=minss(S1*Syst);
  //ss2tf(ww*gtild(ww)),Syst.D')*Syst.D')'

  //Sys=ssrand(2,3,4);
  //[X,d,F,U,k,Z]=abinv(Sys);
  //ss2tf(Sys*Z)
  //ss2tf(Sys*sysfact(Sys,F,'post')*U)
endfunction
