function []=evans(n,d,kmax)
// Copyright Cecil (INRIA) from scilab
// smax, smin: Seuil maxi et mini (relatifs) de discretisation en espace

  smax=0.002;smin=smax/3;
  nptmax=2000 //nbre maxi de pt de discretisation en k

  if nargin <= 0 then
    s_mat=['xbasc();';
	'n=real(poly([0.1-%i 0.1+%i,-10],''s''));';
	'd=real(poly([-1 -2 -%i %i],''s''));';
	'evans(n,d,80);'];
    print(s_mat);execstr(s_mat);
    return;
  end

  if nargin <= 2 then kmax=0;end
  if type(n,'short') <> 'p' || size(n,'*')<>1 then
    error("first argument should be a 1x1 polynomial matrix');
    return;
  end
  if type(d,'short') <> 'p' || size(d,'*')<>1 then
    error("second argument should be a 1x1 polynomial matrix');
    return;
  end

  if kmax<=0 then
    nm=min (n.degree[],d.degree[]);
    fact=norm(d.coeffs{1},2)/norm(n.coeffs{1},2);
    kmax=round(500*fact);
  end

  //calcul de la discretisation en k et racines associees
  nroots=roots(n);racines=roots(d);
  if isempty(nroots) then
    nrm=max([norm(racines,1),norm(roots(d+kmax*n),1)])
  else
    nrm=max([norm(racines,1),norm(nroots,1),norm(roots(d+kmax*n),1)])
  end
  md=d.degree[];
  //
  ord=1:md;kk=0;nr=1;k=0;pas=0.99;fin='no';

  while fin=='no' then
    k=k+pas
    r=roots(d+k*n);r=r(ord)
    dist=max(abs(racines(:,nr)-r))/nrm
    //
    point='nok'
    if dist <smax then //pas correct
      point='ok'
    else //pas trop grand ou ordre incorrect
      // on cherche l'ordre qui minimise la distance
      ix=1:md
      ord1=[]
      for ky=1:md
	yy=r(ky)
	mn=10*dist*nrm
	for kx=1:md
	  if ix(kx)>0 then
	    if  abs(yy-racines(kx,nr)) < mn then
	      mn=abs(yy-racines(kx,nr))
	      kmn=kx
	    end
	  end
	end
	ix(kmn)=0
	ord1=[ord1 kmn]
      end
      r(ord1)=r
      dist=max(abs(racines(:,nr)-r))/nrm
      if dist <smax then
	point='ok',
	ord(ord1)=ord
      else
	k=k-pas,pas=pas/2.5
      end
    end
    if dist<smin then
      //pas trop petit
      pas=2*pas;
    end
    if point=='ok' then
      racines=[racines,r];kk=[kk,k];nr=nr+1
      if k>kmax then fin='kmax',end
      if nr>nptmax then fin='nptmax',end
    end
  end
  //draw the axis
  x1 =[nroots;matrix(racines,md*nr,1)];
  [xmin,xmax]=minmax(real(x1));
  [ymin,ymax]=minmax(imag(x1));
  dx=abs(xmax-xmin)*0.05
  dy=abs(ymax-ymin)*0.05
  if dx< 1d-10, dx=0.01,end
  if dy< 1d-10, dy=0.01,end
  legs=[],lstyle=[];
  rect=[xmin-dx;ymin-dy;xmax+dx;ymax+dy];
  if new_graphics() then
    xsetech(frect=[rect(1) rect(2);rect(3) rect(4)],fixed=%t);
    xset("mark size",3);
    if ~isempty(nroots) then
      plot2d(real(nroots),imag(nroots),style=-5,leg='open loop zeroes')
      //e=gce();e=e.children;e.mark_size_unit="point";e.mark_size=7;
      //legs=[legs 'open loop zeroes'];
    end
    if ~isempty(racines) then
      plot2d(real(racines(:,1)),imag(racines(:,1)),style=-2,rect=rect,leg='open loop poles')
      //e=gce();e=e.children;e.mark_size_unit="point";e.mark_size=7;
      //legs=[legs,'open loop poles']
    end
  else
    // old graphics
    plot2d([],[],rect=rect,frameflag=7)
    xx=xget("mark")
    xset("mark",xx(1),xx(1)+3);
    if ~isempty(nroots) then
      plot2d(real(nroots),imag(nroots),style=-5,frameflag=0,axesflag=0)
      legs=[legs 'open loop zeroes'],lstyle=[lstyle, [-5;0]];
    end
    //plot the poles locations
    if ~isempty(racines) then
      plot2d(real(racines(:,1)),imag(racines(:,1)),style=-2,frameflag=0, ...
	     axesflag=0)
      legs=[legs,'open loop poles'],lstyle=[lstyle, [-2;0]];
    end

  end
  dx=max(abs(xmax-xmin),abs(ymax-ymin));
  //plot the zeros locations
  //computes and draw the asymptotic lines
  m=n.degree[];q=md-m
  if q>0 then
    la=0:q-1;
    so=(sum(racines(:,1))-sum(nroots))/q
    i1=real(so);i2=imag(so);
    if prod(size(la))<>1 then
      ang1=%pi/q*(ones(size(la))+2*la)
      x1=dx*cos(ang1),y1=dx*sin(ang1)
    else
      x1=0,y1=0,
    end
    if md==2,
      if coeff(d,md)<0 then
	x1=0*ones(1,1),y1=0*ones(1,1)
      end,
    end
    if max(k)>0 then
      if new_graphics() then
	// drawing the asymptotic directions should not change the
        // rectangle
	plot2d([i1*ones(1,q);x1+i1],[i2*ones(1,q);y1+i2],style=ones(1,q),rect=rect,leg= 'asymptotic directions');
	// for i=1:q,xsegs([i1,x1(i)+i1],[i2,y1(i)+i2]),end
      else
	plot2d(i1,i2,style=1,frameflag=0,axesflag=0);
	legs=[legs,'asymptotic directions'],lstyle=[lstyle, [1;1]];
	xset("clipgrf");
	for i=1:q,xsegs([i1,x1(i)+i1],[i2,y1(i)+i2]),end,
	xclip();
      end
    end
  end
  // evals locus
  [n1,n2]=size(racines);
  if  new_graphics() then
    plot2d(real(racines)',imag(racines)',style=2+(1:n2),rect=rect);
    // legend(legs,1);
    xtitle('Evans root locus','Real axis','Imag. axis');
  else
    plot2d(real(racines)',imag(racines)',style=2+(1:n2),frameflag=0,axesflag=0);
    // XXX: a faire
    // legends(legs,lstyle,1);
    xtitle('Evans root locus','Real axis','Imag. axis');
    xset("mark",xx(1),xx(2));
  end

  if fin=='nptmax' then
    printf('evans : too many points required')
  end
  //   gain corresponding to a selected point of the locus
  //[l1,l2]=min(abs(racines(:)-selected));
  //col=int(l2/n1)+1;//row=modulo(l2,n1); racines(row,col) <-> seleceted
  //gain=kk(col);
endfunction


function evans_r(r,kmax)
  if nargin <= 2 then kmax=0;end
  evans(r.num,r.den,kmax);
endfunction
