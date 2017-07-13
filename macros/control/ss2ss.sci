function [Sl1,right,left]=ss2ss(Sl,T,F,G,flag)
  // State-space to state-space conversion
  // Returns the linear system Sl1=[A1,B1,C1,D1]
  // where A1=inv(T)*A*T,B1=inv(T)*B,C1=C*T,D1=D.
  // Optional parameters F and G are state feedback
  // and output injection respectively. For example,
  // Sl1=ss2ss(Sl,T,F) returns Sl1=[A1,B1,C1,D1] with
  // A1=inv(T)*(A+B*F)*T;B1=inv(T)*B;C1=(C+D*F)*T;D1=D;
  // If F is given as input then right is a non singular 
  // linear system such that Sl1=Sl*right. 
  // Sl1*invsyslin(right) is a factorization of Sl.
  // Idem for left: if F and G are given, Sl1=left*Sl*right.
  // Example: Sl=ssrand(2,2,5); trzeros(Sl);
  // Sl1=ss2ss(Sl,rand(5,5),rand(2,5),rand(5,2)); 
  // trzeros(Sl1), trzeros(rand(2,2)*Sl1*rand(2,2))
  // See also : projsl
  // Copyright INRIA

  select nargin
    case 2 then
     Sl1=linear_system(inv(T)*Sl.A*T,inv(T)*Sl.B,Sl.C*T,Sl.D,inv(T)*Sl.X0,dom = Sl.dom, ...
                       sample = Sl.dt);
     right=eye(size(Sl.A));left=right;
    case 3 then
     D=Sl(5);// Sl(5) will return a matrix if degree is 0
     A1=Sl.A+Sl.B*F;C1=Sl.C+D*F;
     A1=inv(T)*A1*T;B1=inv(T)*Sl.B;C1=C1*T;D1=D
     Sl1=linear_system(A1,B1,C1,D1,Sl.X0,dom = Sl.dom,sample = Sl.dt);
     right=linear_system(Sl.A+Sl.B*F,Sl.B,F,eye(size(F*Sl.B)),Sl.X0,dom = Sl.dom, ...
                         sample = Sl.dt);
     left=eye(size(Sl.C,1),size(Sl.C,1));
    case 4 then
     D=Sl(5);// Sl(5) will return a matrix if degree is 0
     A1=Sl.A+Sl.B*F+G*Sl.C+G*D*F;C1=Sl.C+D*F;B1=Sl.B+G*D
     A1=inv(T)*A1*T;B1=inv(T)*B1;C1=C1*T;D1=D;
     Sl1=linear_system(A1,B1,C1,D1,Sl.X0,dom = Sl.dom,sample = Sl.dt);
     right=linear_system(Sl.A+Sl.B*F,Sl.B,F,eye(size(F*Sl.B)),Sl.X0,dom = Sl.dom, ...
                         sample = Sl.dt);
     // Warning left is computed as [ At + G*Ct,G;Ct,I]
     // where [At Bt; Ct Dt] is Sl1*right 
     At=Sl.A+Sl.B*F;Ct=Sl.C+D*F
     left=linear_system(At+G*Ct,G,Ct,eye(size(Ct*G)),Sl.X0,dom = Sl.dom,sample = Sl.dt);
    case 5 then
     if flag==1 then
       // x in R^n , y in R^p, u in R^m 
       // output injection [ A + GC, (B+GD,-G)]
       //                  [   C   , (D   , 0)]
       // then feeback ( since output injection increase the 
       // size of the feedback the F matrix must be of size 
       // (m+p,n) --> F=[F1;F2] with F1 (m,n) and F2 (p,n) 
       // 
       // Sl1= [ A+GC +BF1+G*D*F1 -GF2, (B+GD,-G)]
       // 	[ C+D*F1	       , (D   , 0)]
       //
       // We have then the following property 
       // Sl1 equiv  left*sysdiag(sys,eye(p,p))*right 
       //
       // 
       D=Sl(5);// Sl(5) will return a matrix if degree is 0
       n=size(Sl.A,'r');p=size(Sl.C,'r');
       A1=Sl.A+G*Sl.C+[Sl.B+G*D,-G]*F;B1=[Sl.B+G*D,-G];C1=Sl.C+[D,zeros(p,p)]*F;
       D1=[D,zeros(p,p)];
       A1=inv(T)*A1*T;B1=inv(T)*B1;C1=C1*T;D1=D1
       Sl1=linear_system(A1,B1,C1,D1,Sl.X0,dom = Sl.dom,sample = Sl.dt);
       left=linear_system(Sl.A+G*Sl.C,[G,-G],Sl.C,[eye(p,p),zeros(p,p)],Sl.X0, ...
                          dom = Sl.dom,sample = Sl.dt);
       // Now we compute the right associated to left*Sl1
       A1=Sl.A+G*Sl.C;B1=[Sl.B+G*D,-G];C1=Sl.C;D1=[D,zeros(p,p)];
       right=linear_system(A1+B1*F,B1,F,eye(size(F*B1)),Sl.X0,dom = Sl.dom,sample = Sl.dt);
       return
     end
     if flag==2 then
       // x in R^n , y in R^p, u in R^m 
       // feedback first F of size(m,n) 
       //   		    [ A+BF,B]
       //                  [ C+DF,D]
       // then output injection 
       // Sl1= [ A+GC +BF+G*D*F, (B+GD,-G)]
       // 	[ C+D*F   	, (D   , 0)]
       // this is a generalisation of the case 4 
       // We have then the following property 
       // Sl1 equiv left*sysdiag(sys*right,eye(p,p)))
       // 
       D=Sl(5);
       A1=Sl.A+Sl.B*F+G*Sl.C+G*D*F;
       C1=Sl.C+D*F;
       D1=[D,zeros(p,p)];
       B1=[Sl.B+G*D,-G];
       A1=inv(T)*A1*T;B1=inv(T)*B1;C1=C1*T;D1=D1
       Sl1=linear_system(A1,B1,C1,D1,Sl.X0,dom = Sl.dom,sample = Sl.dt);
       right=linear_system(Sl.A+Sl.B*F,Sl.B,F,eye(size(F*Sl.B)),Sl.X0,dom = Sl.dom, ...
                           sample = Sl.dt);
       // Warning left is computed as [ At + G*Ct,(G,-G);
       //                             [    Ct    ,(I, 0)]
       // where [At Bt; Ct Dt] is Sl1*right 
       At=Sl.A+Sl.B*F;Ct=Sl.C+D*F
       left=linear_system(At+G*Ct,[G,-G],Ct,[eye(size(Ct*G)),zeros(size(Ct*G))],Sl.X0, ...
                          dom = Sl.dom,sample = Sl.dt);
     end
  end
endfunction
