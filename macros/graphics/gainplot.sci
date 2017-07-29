function []=gainplot(sl,fmin,fmax,pas,comments)
  // Copyright Cecil (INRIA) from scilab
  dom='c';
  //---------------------
  pas_def='auto'; // default
  //
  noxtitle=%f;
  //
  comments = "";
  ilf=0
  sl_type=type(sl,'short');
  select sl_type
    case {'r','linearsys'} then
      // sl,fmin,fmax [,pas] [,comments]
      if sl_type =='r' then dom= sl.get_dom[]; else dom = sl.dom;end
      if sl_type =='r' then dt= sl.get_dt[]; else dt = sl.dt;end
      if dom=='u' then dom = 'c';end
      select nargin
	case 1 then
	  //sl
	  fmin_default=1.d-3;
	  fmax_default=1.d3;
	  if dom=='c' then fmax_default=1.d3 else fmax_default=1/(2*dt),end
	  [frq,repf]=repfreq(sl,fmin=fmin_default,fmax=fmax_default);sl=[] 
	  [d,phi]=dbphi(repf);
	case 2 then
	  // sl,frq
	  if min(fmin)<=0 then
	    error('bode: requires strictly positive frequency vector')
	  end   
	  [frq,repf]=repfreq(sl,fmin=fmin);fmin=[];sl=[]
	  [d,phi]=dbphi(repf);
	case 3 then
	  //sl,frq,comments ou sl,fmin,fmax
	  if type(fmax,'short')=='m' then
	    if fmin<=0 then
              error('bode: requires strictly positive frequency range')
	    end      
	    [frq,repf]=repfreq(sl,fmin=fmin,fmax=fmax,step=pas_def),sl=[]
	    [d,phi]=dbphi(repf);
	  else
	    comments=fmax;
	    if min(fmin)<=0 then
	      error('bode: requires strictly positive frequency vector')
	    end
	    if dom == 'd' then
	      nyq_frq=1/2/dt;
	      if ~isempty(find(fmin>nyq_frq)) then 
		warning('There are frequencies beyond Nyquist f!');
	      end
	    end
	    [frq,repf]=repfreq(sl,fmin=fmin);fmin=[];sl=[]
	    [d,phi]=dbphi(repf);
	  end
	case 4 then 
	  if type(pas,'short')=='s' then 
	    comments=pas;pas=pas_def;
	  end,
	  if min(fmin)<=0 then
	    error('bode: requires strictly positive frequency vector')
	  end    
	  [frq,repf]=repfreq(sl,fmin=fmin,fmax=fmax,step=pas)
	  [d,phi]=dbphi(repf);
	case 5 then,
	  if min(fmin)<=0 then
	    error('bode: requires strictly positive frequency vector')
	  end    
	  [frq,repf]=repfreq(sl,fmin=fmin,fmax=fmax,step=pas)
	  [d,phi]=dbphi(repf);
	else 
	  error('Error: Invalid number of arguments sys,fmin,fmax [,pas] [,com]')
      end;
    case 'm' then
      //frq,db,phi [,comments] ou frq, repf [,comments]
      noxtitle=%t;
      select nargin
	case 2 , //frq,repf
	  [phi,d]=phasemag(fmin),fmin=[]
	case 3 then
	  if type(fmax,'short')=='m' then
	    d=fmin,fmin=[]
	    phi=fmax,fmax=[]
	  else
	    [phi,d]=phasemag(fmin);fmin=[]
	    comments=fmax
	  end;
	case 4 then 
	  comments=pas;d=fmin;fmin=[];phi=fmax;fmax=[]
	else 
	  error('inputs:frq,db,phi,[com] or frq,repf,[com]')
      end;
      frq=sl;sl=[];[mn,n]=size(frq);
      if min(frq)<=0 then
	error('bode: requires strictly positive frequencies')
      end   
      if mn<>1 then
	ilf=1;//un vecteur de frequences par reponse
      else
	ilf=0;//un seul vecteur de frequence
      end;
    else 
      error('gainplot: invalid plot')
  end;
  [mn,n]=size(phi)
  //
  rect=[min(frq),min(d),max(frq),max(d)]
  if length(comments) == 0 then
    plot2d1(frq',d',style=[1,3:mn+1],logflag='ln',leg=strcat(comments,'@'),rect=rect);
  else
    plot2d1(frq',d',style=[1,3:mn+1],logflag='ln',leg=strcat(comments,'@'),rect=rect);
  end
  xgrid();
  if ~noxtitle then
    xtitle(' ','Hz','db');
  end
endfunction
