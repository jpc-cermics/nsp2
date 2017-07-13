function [nt,dt,rk]=trzeros(Sl)
  // Transmission zeros of Sl = nt./dt
  // Syntax : [nt,dt]=trzeros(Sl)
  //!
  // Copyright INRIA

  if type(Sl,'short')=='p' then
    D=Sl;
    [m,n]=size(D);
    if m <> n then
      error('trzeros: Polynomial matrix--> must be square');
      return;
    end
    chis=det(D);nt=roots(chis);dt=ones(size(nt));
    if nargout==1 then
      nt=nt ./dt;dt=[];rk=[];
    end
    return;
  end
  if type(Sl,'short') <> 'linearsys' && type(Sl,'short') <> 'r' then
    error('Error: expecting a linear system or polynomial matrix');
  end
  if type(Sl,'short')=='r' then
    if size(Sl,'*')==1 then nt=roots(Sl.num);dt=[];rk=1;return;end
    Sl=tf2ss(Sl);
  end
  //Sl=minss(Sl);
  [A,B,C,D]=abcd(Sl);
  if type(D,'short')=='p' then
    [m,n]=size(D);
    if m <> n then error('Trzeros: Polynomial D matrix -->must be square');return;end
    chis=det(systmat(Sl));nt=roots(chis);dt=ones(nt);
    if nargout==1 then nt=nt ./dt;dt=[];rk=[];end
    return;
  end
  if size(A,'*')==0 then
    if type(D,'short')=='m' then nt=[];dt=[];return;end
    if type(D,'short')=='p' then
      [m,n]=size(D);
      if m <> n then error('Error: D(s) must be square');return;end
      chis=det(D);nt=roots(chis);dt=ones(nt);
      if nargout==1 then nt=nt ./dt;dt=[];rk=[];end
      return;
    end
  end
  [ld,kd]=size(D);
  if norm(D,1) < sqrt(%eps) | ld==kd then
    [nt,dt,rk]=tr_zer(A,B,C,D);
    if norm(dt,1) > 1.E-10 then
      if nargout==1 then nt=nt ./dt;dt=[];rk=[];end
      return;
    end
  end
  DP=D*pinv(D);
  if ld < kd && norm(DP-eye(size(DP)),1) < 1.E-10 then
    //nt=spec(A-B*pinv(D)*C);dt=ones(nt);
    [nt,dt]=tr_zer(A,B,C,D);
    rk=ld;
    if nargout==1 then nt=nt ./dt;end
    return;
  end
  DP=pinv(D)*D;
  if ld > kd & norm(DP-eye(size(DP)),1) < 1.E-10 then
    //nt=spec(A-B*pinv(D)*C);dt=ones(nt);
    [nt,dt]=tr_zer(A,B,C,D);
    rk=kd;
    if norm(dt,1) > 1.E-10 then
      if nargout==1 then nt=nt ./dt;dt=[];rk=[];end;return;
    end
  end
  //printf("Warning: Trzeros:non-square system with D non zero and not full\n')
  //By kronecker form
  s=poly(0,'s');
  syst_matrix=systmat(Sl);//form system matrix
  [Q,Z,Qd,Zd,numbeps,numbeta]=kroneck(syst_matrix);
  ix=Qd(1)+Qd(2)+1:Qd(1)+Qd(2)+Qd(3);
  iy=Zd(1)+Zd(2)+1:Zd(1)+Zd(2)+Zd(3);
  finitepencil=Q(ix,:)*syst_matrix*Z(:,iy);
  [E,A]=pen2ea(finitepencil);
  [nt,dt]=gspec(A,E);rk=[];
  if nargout==1 then nt=nt ./dt;dt=[];rk=[];end
endfunction
