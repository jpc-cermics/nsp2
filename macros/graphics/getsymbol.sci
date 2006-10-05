function rep=getsymbol(title)
// Copyright INRIA
  if nargin < 1 then title="",end
  marksIds=['.','+','x','*','diamond fill.','diamond',..
	    'triangle up','triangle down','trefle','circle'];
  fontsSiz=['08' ,'10','12','14','18','24'];
  mm=xget('mark');
  lmid=list('combo','markId',mm(1)+1,marksIds);
  lmsiz=list('combo','marksize',mm(2)+1,fontsSiz);
  xch_l = list(lmid,lmsiz);
  entval=[lmid(2),lmsiz(2)];
  rep=x_choices(title,xch_l)-[1;1];
endfunction
