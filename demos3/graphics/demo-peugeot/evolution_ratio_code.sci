//driver('Rec');

function evolution_ration(x,y,rect)
  plot2d([],[],style=-3,rect=rect,strf='070');
  vals='n = '+string(x);
  //FIXME:drawaxis(x=x,y=rect(2),val=vals,dir='d',tics='v');
  //FIXME:drawaxis(x=rect(1),y=y,dir='l',tics='v');
  for i=1:size(y,'*');
    xpoly([min(x);max(x)],[y(i);y(i)],type='lines',color=3);
  end
  plot2d(x,y,style=-3,rect=rect,strf='000');
endfunction

x=[1,5,7,13];
y=x.^2;
evolution_ration(x,y,[0,0,15,max(y)]);
// bug nouveau graphique 
xtitle(['Evolution du ration';'en fonction ...'],'effectif','Ratio');

////xs2ps(0,'evol_ratio.ps');
//unix(SCI+'/bin/Blatexpr -p 1 1 evol_ratio.ps')  
