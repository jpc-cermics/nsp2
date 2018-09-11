function plzr(a,b,c,d)
//
// Copyright INRIA
  if type(a,'short')=='r' then
    sl=tf2ss(a),
    a=sl.A,b=sl.B,c=sl.C,d=sl(5);// 
  elseif type(a,'short')== 'linearsys' then 
    sl=a;
    a=sl.A,b=sl.B,c=sl.C,d=sl(5);//
  end
  dr=spec(a)
  [al,be]=tr_zer(a,b,c,d)
  nr=al./be; nr=nr(:);
  ni=imag(nr);nr=real(nr)
  di=imag(dr);dr=real(dr)
  //
  mxx=max([nr;dr;1]*1.1)
  mnx=min([nr;dr;-1]*1.1)
  my=max(abs([ni;di;1])*1.1)
  xselect();
  rect=[mnx, -my, mxx, my];
  wdim=xget('wdim')
  dx=(mxx-mnx)/wdim(1);dy=2*my/wdim(2)
  if dy>dx then 
    ax=(dy*wdim(1)-mxx+mnx)/2
    mxx=mxx+ax;mnx=mnx-ax
  elseif dy<dx then
    ay=(dx*wdim(2)-2*my)/2
    my=my+ay
  end
  rect=[mnx, -my, mxx, my];
  xsetech(frect=rect);
  if prod(size(nr))<>0 then
    plot2d(nr,ni,style=-9,mark_size=8,leg='Zeros')
    strf='100'
    pos=6
  else
    strf='100'
    pos=3
  end
  plot2d(dr,di,style=-2,mark_size=8,leg='Poles');
  plot2d([mnx;mxx],[0;0],style=4);
  plot2d([0;0],[-my;my],style=4);

  xarc(-1,1,2,2,0,360*64)
  xtitle('transmission zeros and poles','real axis','imag. axis');
endfunction
