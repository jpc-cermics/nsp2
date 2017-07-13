function Sli=invrs(Sl,alfa)
  // Sli=invrs(Sl,alfa) computes Sli, the PSSD
  // inverse of PSSD Sl.
  //!
  // Copyright INRIA
  D=Sl(5);
  if type(D,'short')=='p' then
    s=poly(0,D.get_var[]);
    Sl.D=horner(Sl(5),s+alfa,ttmode = %t);
  end
  Sl.A=Sl.A-alfa*eye(size(Sl(2)));//Slnew(s)=Slold(s+alfa)

  [Sreg,Wss]=rowregul(Sl,0,0);
  if rcond(Sreg(5)) > 1.E-6 then
    Sli=invsyslin(Sreg)*Wss;
  else
    error('Error: square but singular system');
  end
  [Q,M]=pbig(Sli(2),0.001,'d');
  Sli=projsl(Sli,Q,M);//Remove poles at zero.

  if ~isempty(Sli(2)) then Sli.A=Sli(2)+alfa*eye(size(Sli(2)));end
  if type(Sli(5),'short')=='p' then
    Sli.D=horner(Sli(5),s-alfa,ttmode = %t);
  end
endfunction
