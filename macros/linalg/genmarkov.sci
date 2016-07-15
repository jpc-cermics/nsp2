function M=genmarkov(rec,tr,flag)
// returns a random Markov transition probability matrix
// with size(rec,1) recurrent classes with rec(1),...rec($) 
// entries respectively and tr transient states.
// If the optional parameter flag='perm' is entered a random
// permutation of the states is performed.
// 
  if nargin==2 then flag='noperm';end
  M=[];r=sum(rec);
  for k=rec
    m=rand(k,k);
    m=m./(sum(m,'c')*ones(1,k));
    M=[M # m];
  end
  if type(tr,'short')=='m' then
    n=r+tr;
    MT=rand(tr,n);MT=MT./(sum(MT,'c')*ones(1,n));
    M=[[M,zeros(r,tr)];MT];
  else
    // tr=list(n1,[a1,a2,...ar],n2,[b1,...br],...)
    l=size(tr)/2;   //2*size(rec,2)
    Q=[];
    for kk=1:l
      nt=tr(1+2*(kk-1));
      Q=sysdiag(Q,rand(nt,nt));
    end
    Nt=size(Q,1);
    L=[];
    nclrec=size(rec,'*');
    for kk=1:l
      L1=[];indi=tr(2+2*(kk-1));nt=tr(1+2*(kk-1));
      for i=1:nclrec 
	if indi(i)==0 then
	  L1=[L1,zeros(nt,rec(i))]; else
	  L1=[L1,rand(nt,rec(i))];end
      end   
      L=[L;L1]
    end
    LQ=[L,Q];
    LQ=LQ./(sum(LQ,'c')*ones(1,size(LQ,2)));
    M=[[M,zeros(size(M,1),size(Q,2))];LQ]; 
  end
  if flag=='perm' then p=grand(1,"perm",n);M=M(p,p);end
endfunction
