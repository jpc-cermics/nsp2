function [sd]=gr_menu(sd,flag,noframe)
  global('gr_objects');
  scsmode=%f
  alu=xget('alufunction')
  dash=['0        continue';
	'1        {2,5,2,5}';
	'2        {5,2,5,2}';
	'3        {5,3,2,3}';
	'4        {8,3,2,3}';
	'5        {11,3,2,3}'; 
	'6        {11,3,5,3}}'];

  if nargin<=1,flag=0;end;
  if nargin<=2,noframe=0;end;
  if nargin <=0 then
    cdef=[0 0 100 100];
    init=1
  else
     select type(sd,'string')
      case 'Mat' then 
       cdef=sd;init=1
      case 'List' then
       if sd(1)<>'sd' then
	 error('l''entree n''est pas une liste de type sd'),
       end
       cdef=sd(2);init=0
     else 
	error('incorrect input:'+...
	      '[xmin,ymin,xmax,ymax] ou list(''sd'',,,)')
     end
  end 
  
  xset('recording',0);
  seteventhandler('my_eventhandler');
  if type(gr_objects,'string')=='Mat' then gr_objects=list(); end 
  
  //now move the mouse over the graphic window/
  //seteventhandler('') //suppress the event handler
  xsetech(frect=cdef);
  xrect([0,100,100,100]);
  //plot2d(0,0,style=[1],strf=s_t,rect=cdef);
  // xsetech(frect=cdef);
  curwin=xget('window')
  //xset('clipgrf'); //xclip('clipgrf')
  // menus : we return proper values in menus XXXX 
  names = ['Edit','Settings','Objects'];
  Edit=['redraw','delete all','delete','copy','random','Exit']
  Settings=['dash style','pattern','thickness','mark','clip off','clip on']
  Objects=['rectangle','frectangle','circle','fcircle','polyline',...
	  'fpolyline','spline','arrow','points','caption']
  menus=tlist(['menus',names,'names'],Edit,Settings,Objects);
  Edit='menus(names(1))'+string(1:size(Edit,'*'))+')';
  Settings='menus(names(2))'+string(1:size(Settings,'*'))+')';
  Objects='menus(names(3))'+string(1:size(Objects,'*'))+')';
  
  for k=1:size(names,'*') ;
    delmenu(curwin,names(k));
    addmenu(curwin,names(k),menus(names(k)),list(2,'gr_'+names(k)));
    execstr(names(k)+'_'+string(curwin)+'='+names(k)+';');
  end
  unsetmenu(curwin,'File',7) //close
  unsetmenu(curwin,'3D Rot.')
  
  if init==0 then redraw(sd,s_t); else sd=list('sd',cdef); end,
  if flag==1; xclip();return ;end
  [menus]=resume(menus);
  resume(pixmap=%f);
  //xset('pixmap',1);
endfunction

//---------------------------------------
// Edit menu 
//---------------------------------------

function str=gr_Edit(ind,win)
  // Activated whith menu Edit 
  global('gr_objects');
  str=menus.Edit(ind);
  select str 
   case 'redraw' then gr_draw(win);
   case 'delete all' then gr_objects=list();
   case 'delete' then gr_delete();gr_draw(win);
   case 'copy'   then gr_copy();
   case 'random' then 
    for i=1:50
      gr_rect('define',[100*rand(1,2),10,10]);
      gr_poly('define',[100*rand(2,3)]);
    end
    gr_draw(win);
  end 
endfunction

//---------------------------------------
// Object menu 
//---------------------------------------

function str=gr_Objects(ind,win)
// Activated whith menu Objects 
  str=menus.Objects(ind);
  execstr('gr_create_'+str+'()');
endfunction

function str=gr_Settings(ind,win)
  str=menus.Settings(ind);
endfunction

function [sd1]=symbs(sd,del)
  sd1=[];
  if nargin<=0 then 
    c=getsymbol("Choose a mark");
    if c==[] then
      c=xget('mark')
    end
    n1=c(1);dime=c(2)
    sd1=list("symbs",c(1),c(2));
  else 
     n1=sd(2);dime=sd(3)
  end
  xset("mark",n1,dime);
endfunction

function [sd1]=dashs(sd,del)
  sd1=[];
  if nargin<=0 then 
    n1=x_choose(dash,"Choose a dash style");
    if n1==[] then 
      sd1=list()
    else
       sd1=list("dashs",n1);
    end
  else 
     n1=sd(2)
  end 
  xset("dashes",n1);
endfunction

function [sd1]=patts(sd,del)
  sd1=[];
  if nargin<=0 then 
    n1=getcolor('Choose a pattern ',0)
    if n1==[] then 
      sd1=list()
    else
       sd1=list("patts",n1);
    end
  else 
     n1=sd(2)
  end
  xset("pattern",n1);
endfunction

function [sd1]=Thick(sd,del)
  sd1=[];
  if nargin<=0 then 
    T=string(1:15)
    ll=list()
    t=xget('thickness')
    ll(1)=list('Thickness',t,T);
    [lrep,lres,n1]=x_choices('Choose a Thickness',ll);
    if n1==[] then 
      sd1=list()
    else
       sd1=list("thick",n1);
    end
  else 
     n1=sd(2)
  end
  xset("thickness",n1);
endfunction

//---------------------------------------
// Rectangles 
//---------------------------------------

function [sd1]=gr_rect(action,sd,pt,pt1)
  global('gr_objects');
  control_color=10;
  sd1=0;
  select action 
   case 'draw' then 
    // called for drawing the object sd 
    sd = gr_objects(sd);
    if sd('show') then
      if exists('%nsp') then 
	xrect(sd('data'),thickness=sd('thickness'),color=1,...
	      background=sd('color'));
	if sd('hilited') then 
	  rr= sd('data');
	  xfrect([rr(1:2)+[-1,1],2,2],color=control_color);
	  dr= rr(1:2)+ [rr(3),-rr(4)];
	  xfrect([dr(1:2)+[-1,1],2,2],color=control_color);
	end
	rr=sd('locks');
	cp=find(sd('locks status')<>0);
	for i=cp ; xrect([rr(i,1:2)+[-1,1],2,2],color=1);end 
      else 
	xrect(sd('data'),color=sd('color'),thickness=sd('thickness'));
	if sd('hilited') then 
	  xfrect([sd('data')(1:2)+[-1,1],2,2],color=10);
	end
      end
    end
   case 'translate' then 
    // translate sd with translation vector pt 
    gr_objects(sd)('data')=gr_objects(sd)('data') + [pt,0,0];
    gr_rect('locks',sd);
   case 'define' then 
    sd1= tlist(["rect","show","hilited","data","color","thickness","locks","locks status","pt"],...
               %t,%f,sd,30*rand(1),2,[],[],[0,0]);
    sd1('locks status')=0*ones(1,4); // 4 lock points 
    gr_objects($+1)=sd1;
    n=size(gr_objects,0);
    gr_rect('locks',n);
   case 'move' then  
    // used during copy this is to be changed 
    gr_rect('translate',sd,[5,5]);
    gr_rect('draw',sd)
   case 'inside' then 
    // check if pt is inside boundaries of the rectangle
    br=gr_objects(sd)('data');
    sd1 = br(1) < pt(1) & br(2) >= pt(2) & br(1)+br(3) > pt(1) & br(2)-br(4) <= pt(2);
   case 'inside control' then    
    // check if we are near a control point 
    // here the down-right point 
    d= gr_objects(sd)('data');
    d= max(abs(d(1)+d(3)-pt(1)),abs((d(2)-d(4)-pt(2))));
    if d < 2 then 
      sd1=[1,1]
      xinfo('control point '+string(1));
    else 
       sd1=[0]
    end
   case 'move draw' then 
    // called when we interactively move object 
    gr_rect('translate',sd,pt);
   case 'move point init' then 
    // nothing to do 
   case 'move point' then 
    // move a control point 
    xinfo('inside the move point')
    gr_objects(sd)('data')(3:4)=max(gr_objects(sd)('data')(3:4)+[pt(1),-pt(2)],0);
    gr_rect('locks',sd);
   case 'locks' then 
    // compute locks points 
    rr=gr_objects(sd)('data');
    sd1=[rr(1)+rr(3)/2,rr(2);
	 rr(1)+rr(3)/2,rr(2)-rr(4);
	 rr(1),rr(2)-rr(4)/2;
	 rr(1)+rr(3),rr(2)-rr(4)/2];
    gr_objects(sd)('locks')=sd1;
   case 'inside lock' then 
    // check if we are near a lock point 
    d= gr_objects(sd)('locks'); 
    d1= d - ones(4,1)*pt; 
    [d1]= max(abs(d1),'c');
    [d1,kd]=min(d1);
    if d1 < 5 then 
      sd1=[1,kd,d(kd,1:2)]
      xinfo('lock point '+string(kd));
    else 
       sd1=[0]
    end
   case 'locks update' then 
    // checks if locks point are to be updated 
    sd1=[]
    rr=gr_objects(sd)('locks');
    cp=gr_objects(sd)('locks status');
    for i=1:size(cp,'*') ;
      if cp(i) > 0 then 
	// update a lock last 
	sd1=[sd1,cp(i)];
	n= size(gr_objects(cp(i))('x'),'*');
	gr_objects(cp(i))('x')(n)= rr(i,1);
	gr_objects(cp(i))('y')(n)= rr(i,2);	
      elseif cp(i) < 0 then 
	 sd1=[sd1,-cp(i)];
	 // update a lock first 
	 gr_objects(-cp(i))('x')(1)= rr(i,1);
	 gr_objects(-cp(i))('y')(1)= rr(i,2);
      end
    end
   case 'unlock all' then 
    // check that locks are released 
    cp=gr_objects(sd)('locks status');
    for i=1:size(cp,'*') ;
      if cp(i) > 0 then 
	// polyline  lock last 
	gr_objects(cp(i))('lock last')= 0;
      elseif cp(i) < 0 then 
	 // polyline  lock first 
	gr_objects(-cp(i))('lock first')= 0;
      end
    end
    gr_objects(sd)('locks status')=0*cp;
   case 'params' then 
    colors=m2s(1:xget("lastpattern")+2,"%1.0f");
    lcols_bg=list('colors','Color',sd('color'),colors);
    [lrep,lres,rep]=x_choices('color settings',list(lcols_bg));
    if rep<>[] then
      sd('color')=rep;
    end
    sd1=sd;
  end
endfunction

function gr_create_rectangle()
// interactive acquisition of a rectangle 
  global('gr_objects');
  gr_unhilite();   
  gr_rect('define',[0,100,10,10]);
  n=size(gr_objects,0);
  gr_objects(n)('hilited')=%t;
  [rep]=gr_frame_move(n,[0,100],-5,'move draw',0)
  if rep== -100 then  return;end 
  gr_rect('draw',n);
  if pixmap then xset('wshow'),end
endfunction

// ------------------------------------------
// polyline 
// ------------------------------------------

function sd1 =gr_poly(action,sd,pt,pt1)
  global('gr_objects');
  control_color=10;
  sd1=0;
  select action 
   case 'draw' then 
    sd = gr_objects(sd);
    if sd('show') then
      xpoly(sd('x'),sd('y'),type='lines');
      if sd('hilited') then 
	// hilited part 
	rects=[sd('x')-1;sd('y')+1;2*ones(sd('x'));2*ones(sd('x'))];
	n=size(rects,'c');
	control_color=10;
	colors=control_color*ones(1,n);
	if sd('lock first')(1) then colors(1)=1;end 
	if sd('lock last')(1) then colors($)=1;end 
	xrects(rects,colors);
      end
    end
    
   case 'translate' then 
    // translate sd with translation vector pt 
    gr_objects(sd)('x')=gr_objects(sd)('x')+pt(1);
    gr_objects(sd)('y')=gr_objects(sd)('y')+pt(2);
   case 'define' then 
    sd1= tlist(["poly","show","hilited","x","y","color","thickness","lock first","lock last","locks status","pt"],...
               %t,%f,sd(1,:),sd(2,:),30*rand(1),2,0,0,0,[0,0]);
    gr_objects($+1)=sd1;
   case 'move' then 
    gr_poly('translate',sd,[5,5]);
    gr_poly('draw',sd)
   case 'inside' then 
    // is pointer near object 
    sd=gr_objects(sd);
    [pt,kmin,pmin,d]=gr_dist2polyline(sd('x'),sd('y'),pt);
    if d < 3 then sd1=%t 
    else 
       sd1=%f ;
    end
   case 'inside control' then
    // check if we are near a control point 
    sd=gr_objects(sd);
    [d,k]=min( (sd('x')-pt(1)).^2 + (sd('y')-pt(2)).^2 )
    if d < 2 then 
      sd1=[1,k]
      xinfo('control point '+string(k));
    else 
       sd1=[0]
    end
   case 'move draw' then 
    // translate then draw (forcing the show)
    // used when object is moved 
    gr_poly('translate',sd,pt);
   case 'move point init' then 
    gr_objects(sd)('pt')=  [gr_objects(sd)('x')(pt),gr_objects(sd)('y')(pt)];
   case 'move point' then 
    // move a control point 
    xinfo('inside the move point '+string(pt1))
    xinfo('moving control '+string(pt1));
    // force horizontal and vertical line 
    // when we are in the vicinity of it 
    n = size(gr_objects(sd)('x'),'*');
    // we keep in pt the current point position 
    // since magnetism can move us to a new position 
    ptc = gr_objects(sd)('pt');
    //ptc=[gr_objects(sd)('x')(pt1),gr_objects(sd)('y')(pt1)];
    ptnew = ptc+pt;
    gr_objects(sd)('pt')=ptnew;
    if pt1 >= 2 & pt1 < n then 
      // magnetism toward horizontal or vertival lines 
      ptb=[gr_objects(sd)('x')(pt1-1),gr_objects(sd)('y')(pt1-1)];
      ptn=[gr_objects(sd)('x')(pt1+1),gr_objects(sd)('y')(pt1+1)];
      ptmin=min(ptb,ptn);ptmax=max(ptb,ptn);
      pts=[ptmin;ptmax;ptmin(1),ptmax(2);ptmax(1),ptmin(2)]
      dd= abs(pts-ones(4,1)*ptnew);
      k=find(max(dd,'c') < 5);
      if k<>[] then 
	xinfo('found '+string(pts(k(1),1))+' '+string(pts(k(1),2)));
	ptnew= pts(k(1),:)
      end
    elseif pt1==1 then 
       // try to check if we are in the vivinity of 
       // a lock point lock points ptl=[lock-number,point]
       [k,ptl]=gr_lock(ptnew);
       if k<>0 then 
	 // we force the point to move to ptl(2:3) 
	 // the lock point near ptnew position 
	 ptnew=ptl(2:3);
	 rr = gr_objects(sd)('lock first');
	 if  rr(1) == 1 ; 
	   // we were already locked somewhere; unlock 
	   gr_objects(rr(2))('locks status')(rr(3))=0;// set unlock 
	 end
	 // lock at new point 
	 xinfo('trying to lock '+string(k)+' '+string(ptl(1)));
	 gr_objects(sd)('lock first')=[1,k,ptl(1)];
	 gr_objects(k)('locks status')(ptl(1))= - sd ;// set lock (<0)
	 
       else
	  // just test if unlock is necessary 
	  rr= gr_objects(sd)('lock first');
	  if  rr(1) == 1 ; 
	    xinfo('trying to unlock '+string(rr(2))+' '+string(rr(3)));
	    gr_objects(rr(2))('locks status')(rr(3))=0;// set unlock 
	    gr_objects(sd)('lock first')=0;
	  end
       end
    elseif pt1==n then 
       // try to check if we are in the vivinity of 
       // a lock point lock points ptl=[lock-number,point]
       [k,ptl]=gr_lock(ptnew);
       if k<>0 then 
	 // we force the point to move to ptl(2:3) 
	 // the lock point near ptnew position 
	 ptnew=ptl(2:3);
	 rr = gr_objects(sd)('lock last');
	 if  rr(1) == 1 ; 
	   // we were already locked somewhere; unlock 
	   gr_objects(rr(2))('locks status')(rr(3))=0;// set unlock 
	 end
	 // lock at new point 
	 xinfo('trying to lock '+string(k)+' '+string(ptl(1)));
	 gr_objects(sd)('lock last')=[1,k,ptl(1)];
	 gr_objects(k)('locks status')(ptl(1))=sd ;// set lock (>0)
       else
	  // just test if unlock is necessary 
	  rr= gr_objects(sd)('lock last');
	  if  rr(1) == 1 ; 
	    xinfo('trying to unlock '+string(rr(2))+' '+string(rr(3)));
	    gr_objects(rr(2))('locks status')(rr(3))=0;// set unlock 
	    gr_objects(sd)('lock last')=0;
	  end
       end
    end
    gr_objects(sd)('x')(pt1)=ptnew(1);
    gr_objects(sd)('y')(pt1)=ptnew(2);
   case 'locks' then 
    // compute locks points 
    sd1=[];
   case 'inside lock' then 
    // check if we are near a lock point 
    sd1=[0];
   case 'locks update' then 
    // nothing to update 
    sd1=[];
   case 'unlock all' then 
    // check that locks are released 
    rr=gr_objects(sd)('lock first');
    if  rr(1) == 1 ; 
      gr_objects(sd)('lock first')=0;
      gr_objects(rr(2))('locks status')(rr(3))=0;// set unlock 
    end
    rr=gr_objects(sd)('lock last');
    if  rr(1) == 1 ; 
      gr_objects(sd)('lock last')=0;
      gr_objects(rr(2))('locks status')(rr(3))=0;// set unlock 
    end
  end
endfunction

function gr_create_polyline()
// interactive acquisition of a polyline 
// 
  global('gr_objects');
  gr_unhilite();   
  hvfactor=5;// magnetism toward horizontal and vertical line 
  xinfo('Enter polyline, Right click to stop');
  dr=driver();  if dr=='Rec' then driver('X11'),end
  rep(3)=%inf;
  wstop = 0; 
  kstop = 2;
  count = 2;
  ok=%t;
  // A améliorer XXX 
  [i,x,y]=xclick();
  gr_poly('define',[x,x;y,y]);
  n = size(gr_objects,0);
  gr_objects(n)('hilited')=%t;
  xset('alufunction',6); // change alu mode 
  if i==2 then ok=%f ; wstop=1; end 
  while wstop==0 , //move loop
    // draw block shape
    // 
    gr_poly('draw',n);
    if pixmap then xset('wshow'),end
    // get new position
    if exists('%nsp') then 
      rep=xgetmouse(clearq=%f,getmotion=%t,getrelease=%f);
    else 
       rep=xgetmouse(0,[%t,%t])
    end
    xinfo('rep='+string(rep(3)));
    if rep(3)== -100 then
      rep =rep(3);
      return; 
    end ;
    // clear block shape
    gr_poly('draw',n);
    wstop= size(find(rep(3)== kstop),'*');
    if rep(3) == 0 then   count =count +1;end 
    if rep(3) == 0 | rep(3) == -1 then 
      // are we near a lock point 
      [k,ptl]=gr_lock(rep(1:2));
      if k<>0 then 
	gr_objects(n)('x')(count)=ptl(2);
	gr_objects(n)('y')(count)=ptl(3);
	gr_objects(n)('lock last')=[1,k,ptl(1)];
	if rep(3)==0;wstop=1;end
      else 
	 // try to keep horizontal and vertical lines 
	 if abs(gr_objects(n)('x')(count-1)- rep(1)) < hvfactor then rep(1)=gr_objects(n)('x')(count-1);end 
	 if abs(gr_objects(n)('y')(count-1)- rep(2)) < hvfactor then rep(2)=gr_objects(n)('y')(count-1);end 
	 gr_objects(n)('x')(count)=rep(1);
	 gr_objects(n)('y')(count)=rep(2);
	 gr_objects(n)('lock last')=[0];
      end
    end
  end
  // update and draw block
  xset('alufunction',3);
  driver(dr);
  if ~ok then return ;end
  // check if the polyline is locked at some rectangles lock point 
  [k,ptl]=gr_lock([gr_objects(n)('x')(1),gr_objects(n)('y')(1)]);
  if k<>0 then 
    gr_objects(n)('x')(1)=ptl(2);
    gr_objects(n)('y')(1)=ptl(3);
    gr_objects(n)('lock first')=[1,k,ptl(1)];
    gr_objects(k)('locks status')(ptl(1))= - n;// set lock (<0)
  end 
  //Attention ici $ est mal evalue XXXX
  //[k,ptl]=gr_lock([poly('x')($),poly('y')($)]);
  np=size(gr_objects(n)('x'),'*');
  [k,ptl]=gr_lock([gr_objects(n)('x')(np),gr_objects(n)('y')(np)]);
  if k<>0 then 
    gr_objects(n)('x')(np)=ptl(2);
    gr_objects(n)('y')(np)=ptl(3);
    gr_objects(n)('lock last')=[1,k,ptl(1)];
    gr_objects(k)('locks status')(ptl(1))=n;// set lock (>0)
  end 
  gr_poly('draw',n);
  if pixmap then xset('wshow'),end
endfunction


//-----------------------------------
// filled rectangle 
//-----------------------------------

function sd1=frect(sd,del)
  sd1=[];
  if nargin<=0 then // get
    [x1,y1,x2,y2,but]=xgetm(d_xrect) 
    if but==2 then sd1=list();return,end
    sd1=list("frect",x1,x2,y1,y2);
    d_xfrect(x1,y1,x2,y2);
  elseif nargin==1 then //draw
     x1=sd(2);x2=sd(3),y1=sd(4),y2=sd(5)
     d_xfrect(x1,y1,x2,y2);
  elseif del=='del' then //erase    
     x1=sd(2);x2=sd(3),y1=sd(4),y2=sd(5)
     d_xfrect(x1,y1,x2,y2);
  elseif del=='mov' then //move      
     x1=sd(2);x2=sd(3),y1=sd(4),y2=sd(5)
     x0=xx(1);y0=xx(2);
     [xo,yo]=move_object('d_xfrect(x1-(x0-xo),y1-(y0-yo),x2-(x0-xo),y2-(y0-yo))',x0,y0);
     sd(2)=sd(2)-(x0-xo)
     sd(3)=sd(3)-(y0-yo)
     sd(4)=sd(4)-(x0-xo)
     sd(5)=sd(5)-(y0-yo)
  end
endfunction

// circle 

function sd1=cerc(sd,del)
  sd1=[];
  if nargin<=0 then // get
    [c1,c2,x1,x2,but]=xgetm(d_circle);
    if but==2 then sd1=list();return,end
    x=[x1;x2],c=[c1;c2];r=norm(x-c,2);
    sd1=list("cercle",c,r);
    d_circle(c,r);
  elseif nargin==1 then //draw
     c=sd(2);r=sd(3);
     d_circle(c,r);
  elseif del=='del' then //erase      
     c=sd(2);r=sd(3);
     d_circle(c,r);
  elseif del=='mov' then //move        
     c=sd(2);r=sd(3)
     x0=xx(1);y0=xx(2);
     [xo,yo]=move_object('d_circle(c-[x0-xo;y0-yo],r)',x0,y0);
     sd(2)=sd(2)-[x0-xo;y0-yo]
  end;
endfunction

// filled circle 

function sd1=fcerc(sd,del)
  sd1=[];
  if nargin<=0 then // get
    [c1,c2,x1,x2,but]=xgetm(d_circle);
    if but==2 then sd1=list();return,end
    x=[x1;x2],c=[c1;c2];r=norm(x-c,2);
    sd1=list("fcercle",c,r);
    d_fcircle(c,r);
  elseif nargin==1 then //draw
     c=sd(2);r=sd(3)
     d_fcircle(c,r);
  elseif del=='del' then //erase   
     c=sd(2);r=sd(3)
     d_fcircle(c,r);
  elseif del=='mov' then //move      
     c=sd(2);r=sd(3)
     x0=xx(1);y0=xx(2);
     [xo,yo]=move_object('d_fcircle(c-[x0-xo;y0-yo],r)',x0,y0);
     sd(2)=sd(2)-[x0-xo;y0-yo]
  end;
endfunction

// arrow 

function [sd1]=fleche(sd,del)
  sd1=[]
  if nargin<=0 then // get
    [oi1,oi2,of1,of2,but]=xgetm(d_arrow);
    if but==2 then sd1=list();return,end
    o1=[oi1;of1],o2=[oi2;of2];
    [r1,r2]=xgetech()
    sz=1/(40*min(abs(r2(3)-r2(1)),abs(r2(4)-r2(2))))
    sd1=list("fleche",o1,o2,sz);
    d_arrow(o1,o2,sz);
  elseif nargin==1 then //draw
     o1=sd(2),o2=sd(3),
     sz=-1
     if size(sd)>=4 then sz=sd(4),end
     d_arrow(o1,o2,sz);
  elseif del=='del' then //erase 
     o1=sd(2),o2=sd(3),
     sz=-1
     if size(sd)>=4 then sz=sd(4),end
     d_arrow(o1,o2,sz);
  elseif del=='mov' then //move
     o1=sd(2),o2=sd(3),
     sz=-1
     if size(sd)>=4 then sz=sd(4),end
     x0=xx(1);y0=xx(2);
     [xo,yo]=move_object('d_arrow(o1-(x0-xo),o2-(y0-yo),sz)',x0,y0);
     sd(2)=sd(2)-(x0-xo)
     sd(3)=sd(3)-(y0-yo)
  end
endfunction

// Text 
//-----

function [sd1]=comment(sd,del)
  sd1=[];
  if nargin<=0 then // get
    [i,z1,z2]=xclick(0);z=[z1;z2];
    com=x_dialog("Enter string"," ");
    if com<>[] then  
      sd1=list("comm",z,com),
      xstring(z(1),z(2),com,0,0);
    end
  elseif nargin==1 then //draw
     z=sd(2);com=sd(3);
     xstring(z(1),z(2),com,0,0);
  elseif del=='del' then //erase 
     z=sd(2);com=sd(3);
     xstring(z(1),z(2),com,0,0);
  elseif del=='mov' then //move  
     z=sd(2);com=sd(3);
     [xo,yo]=move_object('xstring(xo,yo,com,0,0)',z(1),z(2));
     sd1=sd;sd1(2)(1)=xo;sd1(2)(2)=yo;
  end;
endfunction

// ? 

function [sd1]=ligne(sd,del)
// polyline 
  sd1=[];
  if nargin<=0 then // get
    z=xgetpoly(d_seg);
    if z==[], return;end;
    sd1=list("ligne",z);
    xpoly(z(1,:)',z(2,:)',type="lines")
  elseif nargin==1 then //draw
     z=sd(2);
     xpoly(z(1,:)',z(2,:)',type="lines")
  elseif del=='del' then //erase
     z=sd(2);
     xpoly(z(1,:)',z(2,:)',type="lines")
  elseif del=='mov' then //move
     z=sd(2);
     x0=xx(1);y0=xx(2);
     [xo,yo]=move_object('xpoly(z(1,:)''-(x0-xo),z(2,:)''-(y0-yo),type=""lines"")',x0,y0);
     sd(2)=[z(1,:)-(x0-xo);z(2,:)-(y0-yo)]
  end;
endfunction

function [sd1]=fligne(sd,del)
// filled polyline 
  sd1=[];
  if nargin<=0 then // get
    z=xgetpoly(d_seg);
    if z==[], return;end;
    sd1=list("fligne",z);
    xfpoly(z(1,:),z(2,:),1);
  elseif nargin==1 then //draw
     z=sd(2);
     xfpoly(z(1,:),z(2,:),1);
  elseif del=='del' then //erase
     z=sd(2);
     xfpoly(z(1,:),z(2,:),1)
  elseif del=='mov' then //move
     z=sd(2);
     x0=xx(1);y0=xx(2);
     [xo,yo]=move_object('xfpoly(z(1,:)-(x0-xo),z(2,:)-(y0-yo),1)',x0,y0);
     sd(2)=[z(1,:)-(x0-xo);z(2,:)-(y0-yo)]
  end;
endfunction

function [sd1]=curve(sd,del)
// smoothed curve 
  sd1=[];
  if nargin<=0 then ,//get
    z=xgetpoly(d_seg);
    if z==[], return;end
    mm=clearmode();xpoly(z(1,:)',z(2,:)',type="lines");modeback(mm)
    [x1,k1]=sort(z(1,:));y1=z(2,k1);z=[x1;y1];
    [n1,n2]=size(z);z=smooth(z(:,n2:-1:1));
    sd1=list("ligne",z);
  else
     z=sd(2);
  end;
  xpoly(z(1,:)',z(2,:)',type="lines");
endfunction

function [sd1]=points(sd,del)
// polymark 
  sd1=[];
  if nargin<=0 then //get
    z=xgetpoly(d_point);
    if z==[], return;end;
    sd1=list("point",z);
    xpoly(z(1,:)',z(2,:)',type="marks");
  elseif nargin==1 then //draw
     z=sd(2);
     xpoly(z(1,:)',z(2,:)',type="marks");
  elseif del=='del' then //erase  
     z=sd(2);
     xpoly(z(1,:)',z(2,:)',type="marks");
  elseif del=='mov' then //move  
     z=sd(2);
     x0=xx(1);y0=xx(2);
     [xo,yo]=move_object('xfpoly(z(1,:)''-(x0-xo),z(2,:)''-(y0-yo),""marks"")',x0,y0);
     sd(2)=[z(1,:)-(x0-xo);z(2,:)-(y0-yo)]
  end;
endfunction

function [sd1]=grclipoff(sd,del)
  sd1=[];
  if nargin<=0 then ,
    sd1=list("clipoff")
  end;
  xclip();
endfunction

function [sd1]=grclipon(sd,del)
  sd1=[];
  if nargin<=0 then ,
    sd1=list("clipon")
  end;
  xclip('clipgrf');
endfunction

function my_eventhandler(win,x,y,ibut)
  global('gr_objects');
  global('count');
  //if count == 1 then 
  //  printf("event handler aborted =%d\n",count)
  //  return
  //end
  count = 1;
  if ibut == -100 then 
    printf('window killed ')
  elseif ibut==-1 then 
    [xc,yc]=xchange(x,y,'i2f')
    xinfo('Mouse position is ('+string(xc)+','+string(yc)+')')
  elseif ibut==0 then 
     [xc,yc]=xchange(x,y,'i2f')
     k = gr_find(xc,yc);
     if k<>0 then 
       rep(3)=-1
       o=gr_objects(k);
       // are we moving the object or a control point 
       execstr('ic=gr_'+o.type+'(''inside control'',k,[xc,yc]);');
       // hide the moving object and its locked objects 
       gr_objects(k)('show')=%f; 
       execstr('lcks=gr_'+o.type+'(''locks update'',k);');
       for i=lcks 
	 gr_objects(i)('show')=%f; 
       end
       gr_unhilite(draw=%f);
       gr_objects(k)('hilited')=%t;
       // global draw to hide current object 
       gr_draw(win);
       gr_objects(k)('show')=%t;
       for i=lcks 
	 gr_objects(i)('show')=%t;
       end
       // interactive move 
       if ic(1)==0 then 
	 // we are moving the object 
	 [rep]=gr_frame_move(k,[xc,yc],-5,'move draw',0)
	 if rep== -100 then  
	   count= 0; 
	   return;
	 end 
       else 
       	 // we are moving a control point of the object 
	 execstr('gr_'+o.type+'(''move point init'',k,ic(2));');
	 [rep]=gr_frame_move(k,[xc,yc],-5,'move point',ic(2))
	 if rep== -100 then  
	   count=0; 
	   return;
	 end 
       end
       gr_draw(win);
     else 
	xinfo('Click in empty region');
     end
  elseif ibut==2
    [xc,yc]=xchange(x,y,'i2f')
    k = gr_find(xc,yc);
    if k<>0 then 
      rep(3)=-1
      obj=gr_objects(k);
      execstr('obj1=gr_'+obj.type+'(''params'',obj,0);');
      gr_objects(k)=obj1;
      // should check here if redraw is needed 
      if ~obj1.equal[obj] then 
	gr_draw(win);
      end
    end
  elseif ibut==100 
    gr_delete();
    gr_draw(win);
  elseif ibut==99
    gr_copy();
    gr_draw(win);
  else
    xinfo('Mouse action: ['+string(ibut)+']');
  end
  count=0;
endfunction

function [rep]=gr_frame_move(ko,pt,kstop,action,pt1)
// move object frame in Xor mode 
  global('gr_objects');
  dr=driver(); 
  if dr=='Rec' then driver('X11'),end
  xset('alufunction',6);
  rep=[0,0,%inf];
  wstop = 0; 
  lcks=[];
  otype = gr_objects(ko).type; 
  of = 'gr_'+otype;
  while wstop==0 , //move loop
    // draw block shape
    execstr(of+'(''draw'',ko);');
    if size(lcks,'*')<>0 then 
      // draw connected links 
      for lk= lcks 
	lo = gr_objects(lk);
	execstr('gr_'+lo.type+'(''draw'',lk);');      
      end
    end
    if pixmap then xset('wshow'),end
    // get new position
    if exists('%nsp') then 
      rep=xgetmouse(clearq=%f,getmotion=%t,getrelease=%t);
    else 
       rep=xgetmouse(0,[%t,%t])
    end
    if rep(3)== -100 then 
      rep =rep(3);
      xset('alufunction',3);
      driver(dr);
      return; 
    end ;
    wstop= size(find(rep(3)== kstop),'*');
    // clear block shape
    execstr(of+'(''draw'',ko);');
    if size(lcks,'*')<>0 then 
      // draw connected links 
      for lk= lcks 
	lo = gr_objects(lk);
	execstr('gr_'+lo.type+'(''draw'',lk);');      
      end
    end
    // move object or point inside object 
    execstr(of+'(action,ko,rep(1:2)- pt);');
    // update associated lock; 
    execstr('lcks='+of+'(''locks update'',ko);');
    pt=rep(1:2);
  end
  // update and draw block
  xset('alufunction',3);
  driver(dr);
  rep=rep(3);
endfunction

//---------------------------------
// check if pt is in a lock point 
//---------------------------------

function [k,rep]=gr_lock(pt) 
  global('gr_objects');
  for k=1:size(gr_objects)
    o=gr_objects(k);
    execstr('rep=gr_'+o.type+'(''inside lock'',k,pt);');
    if rep(1)==1 then
      rep=rep(2:4);
      return ;  
    end
  end
  k=0;rep=0;
endfunction


//--------------------------------------
// find object k for which [x,y] is inside 
//--------------------------------------

function k=gr_find(x,y)
  global('gr_objects');
  for k=1:size(gr_objects)
    o=gr_objects(k);
    execstr('ok=gr_'+o.type+'(''inside'',k,[x,y]);');
    if ok then return ; end 
  end
  k=0;
endfunction

//--------------------------------------
// draw all objects 
//--------------------------------------

function gr_draw(win)
  global('gr_objects');
  xclear(win,%f);
  xtape('replay',win);
  fr=[0,0,100,100];
  xsetech(frect=fr)
  xrect([0,100,100,100]);
  //pause
  for k=1:size(gr_objects)
    o=gr_objects(k);
    execstr('gr_'+o.type+'(''draw'',k);');
  end
  if pixmap then xset('wshow'); end 
endfunction 

//--------------------------------------
// unhilite objects and redraw
//--------------------------------------

function gr_unhilite(win=-1,draw=%t)
  global('gr_objects');
  ok=%f;
  if win == -1 then win=xget('window');end 
  for k=1:size(gr_objects)
    o=gr_objects(k);
    if o('hilited') then ok=%t;end 
    gr_objects(k)('hilited')=%f;
  end
  if ok & draw then gr_draw(win);end 
endfunction 

function gr_delete()
  // delete hilited objects 
  global('gr_objects');
  g_rep=%f
  for k=size(gr_objects):-1:1
    o=gr_objects(k);
    if o('hilited') then 
      execstr('rep=gr_'+o.type+'(''unlock all'',k);');
      gr_objects(k)=null();
      // we must update all the numbers contained in lock 
      for j=1:size(gr_objects)
	lkcs=gr_objects(j)('locks status')
	for i=1:size(lkcs,'*')
	  if lkcs(i) >= k then 
	    gr_objects(j)('locks status')(i)= lkcs(i)-1;
	  end
	end
      end
      g_rep=%t ; 
    end 
  end
  if g_rep==%f then 
    xinfo('No object selected fo deletion');
  end
endfunction 

function gr_copy()
  // copy  hilited objects 
  global('gr_objects');
  g_rep=%f
  k1=size(gr_objects):-1:1;
  for k=k1;
    o=gr_objects(k);
    if o('hilited') then 
      gr_objects($+1)=o;
      n=size(gr_objects,0);
      gr_objects(n)('hilited')=%f;
      execstr('gr_'+o.type+'(''move'',n);');
      g_rep=%t ; 
    end 
  end
  if g_rep==%f then 
    xinfo('No object selected fo copy');
  end
endfunction 

function [pt,kmin,pmin,dmin]=gr_dist2polyline(xp,yp,pt)
// utility function 
// distance from a point to a polyline 
// the point is on the segment [kmin,kmin+1] (note that 
// kmin is < size(xp,'*'))
// and its projection is at point 
// pt = [ xp(kmin)+ pmin*(xp(kmin+1)-xp(kmin)) ;
//        yp(kmin)+ pmin*(yp(kmin+1)-yp(kmin)) 
// the distance is dmin 
// Copyright ENPC
  n=size(xp,'*');
  ux= xp(2:n)-xp(1:n-1);
  uy= yp(2:n)-yp(1:n-1);
  wx= pt(1) - xp(1:n-1);
  wy= pt(2) - yp(1:n-1);
  un= ux.*ux + uy.*uy
  // XXXX %eps 
  eps= 1.e-10;
  un=max(un,100*eps); // to avoid pb with empty segments 
  p = max(min((ux.*wx+ uy.*wy)./un,1 ),0);
  // the projection of pt on each segment 
  gx= wx -  p .* ux;
  gy= wy -  p .* uy;
  [d2min,kmin] = min(gx.*gx + gy.*gy );
  dmin=sqrt(d2min);
  pmin= p(kmin);
  pt = [ xp(kmin)+ pmin*(xp(kmin+1)-xp(kmin));yp(kmin)+ pmin*(yp(kmin+1)-yp(kmin))];
endfunction


function [gr_options,edited]=gr_do_options(gr_options)
// Copyright ENPC/jpc  options for gr_menus 
// 
  colors=m2s(1:xget("lastpattern")+2,"%1.0f");
  fontsSiz=['08','10','12','14','18','24'];
  fontsIds=[ 'Courrier','Symbol','Times','Times Italic','Times Bold',
	     'Times B. It.'];
  marksIds=['.','+','x','*','diamond fill.','diamond','triangle up',
	    'triangle down','trefle','circle'];
  DashesIds=['Solid','-2-  -2-','-5-  -5-','-5-  -2-','-8-  -2-',
	     '-11- -2-','-11- -5-'];
  edited=%f
  //  gr_options=hcreate(color=4,background=xget('white'),foreground=6,font=2,font_size=1);
  l_col=list('colors','color',gr_options.color,colors);
  l_bg=list('colors','Background',gr_options.background,colors);
  l_fg=list('colors','Foreground',gr_options.foreground,colors);
  l_fid=list('combo','fontId',gr_options.font+1,fontsIds);
  l_fiz=list('combo','fontsize',gr_options.font_size,fontsSiz);
  Lc = list(l_col,l_bg,l_fg,l_fid,l_fiz);
  [lrep,lres,rep]=x_choices('GrMenu options',Lc,%t);
  if rep<>[] then
    gr_options=hcreate(color=rep(1),...
		       background=rep(2),...
		       foreground=rep(3),...
		       font=rep(4),...
		       font_size=rep(5));
    edited=%t;
  end
endfunction




