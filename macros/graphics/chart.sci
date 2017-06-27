function chart(varargin,varargopt)
//  chart - Nichols chart
//  chart([flags])
//  chart(gain [,flags])
//  chart(gain,phase [,flags])
//flags: a list of at most 4 flags list(sup [,leg [,cm [,cphi]]])
//sup: 1 indicates superposition on the previous plot 0 no superposition is done
//leg: 1 indicates that legends are drawn, o: no legends
//cm: color number (see plot2d) for gain curves
//cphi: color number (see plot2d) for phase curves

  if ~varargopt.iskey['superpose'] then 
    varargopt.superpose = %t;
  end
  titre="Amplitude and phase contours of y/(1+y)"
  l10=log(10);
  ratio=%pi/180;
  
  attenu=[-12,-8,-6,-5,-4,-3,-2,-1.4,-1,-.5,...
	  0.25,0.5,0.7,1,1.4,2,2.3,3,4,5,6,8,12];
  angl= [-(1:10),-(20:10:160)]*ratio;
    
  if length(varargin) >= 1 then attenu = varargin(1);end
  if length(varargin) >= 2 then angl = -varargin(2)*ratio;end
  
  if varargopt.iskey['superpose'] && varargopt.superpose then 
    [va,frect]=xgetech()
  else
    frect = [-360,-20,0,40];
    xsetech(frect=frect);
  end
  phi_min = frect(1); phi_max=frect(3);
  k1=floor(phi_min/180)
  k2=ceil(phi_max/180)
  //
  if varargopt.iskey['title'] then 
    xtitle(varargopt.title,"phase(y) (degree)","magnitude(y) (Db)"),
  end
  
  //isogain curves
  lambda=exp(l10*attenu/20)
  rayon=lambda./(lambda.*lambda-ones(size(lambda)))
  centre=-lambda.*rayon
  //
  if ~isempty(attenu)
    p=cell(0,0);m=cell(0,0);
    for i = 1:prod(size(attenu)),
      att=attenu(i);
      N= %pi/0.01;
      if att < 0 then 
	w=linspace(100*%eps,%pi,N);
      else 
	w=linspace(-%pi,- 100*%eps,N);
      end
      n=prod(size(w))
      rf=centre(i)*ones(size(w))+rayon(i)*exp(%i*w);
      phi=atan(imag(rf),real(rf))/ratio; //phi is in [-180 0]
      module=20*log(abs(rf))/l10;
      //use symetry and period to extend the curve on [k1*180 k2*180]
      for k=k1:k2-1
	if modulo(k,2)==0 then
	  pm= [k*180-phi($:-1:1)];
	  p{$+1}=pm;
	  mm= [module($:-1:1)]
	  m{$+1}=mm;
	  if att>0 then 
	    xstring(pm($),mm($),string(att));//,0,0),
	  end
	else
	  pm = [((k+1)*180)+phi];
	  p{$+1}=pm;
	  mm = module;
	  m{$+1}=mm;
	  if att<0 then 
	    xstring(pm($),mm($),string(att));//0,0),
	  end
	end
      end
    end
    for pi=1:size(p,'*'); xpoly(p{pi},m{pi},color=xget('lastpattern')+3);end
  end
  //isophase curves
  if ~isempty(angl) then
    p=cell(0,0);m=cell(0,0);
    eps=10*%eps;
    for teta=angl,
      if teta < -%pi/2 then
	last=teta-eps;
      else
	last=teta+eps;
      end
      w=[-(175*ratio):0.03:last,last]
      n=prod(size(w));
      module=real(20*log((sin(w)*cos(teta)/sin(teta)-cos(w)))/l10)
      w=w/ratio
      for k=k1:k2-1
	if modulo(k,2)==0 then
	  p{$+1}=[k*180-w($:-1:1)];
	  m{$+1}=[module($:-1:1)];
	else
	  p{$+1}=[((k+1)*180)+w];
	  m{$+1}=[module];
	end
      end
    end
    for pi=1:size(p,'*');
      I=isinf(m{i});p{i}(I)=[];m{i}(I)=[];
      if size(p{i},'*')> 1 then  xpoly(p{pi},m{pi},color=xget('lastpattern')+3);end
    end
  end
endfunction
