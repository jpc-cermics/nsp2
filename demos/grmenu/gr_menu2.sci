// test program 
// a new scicos editor 
// this works with objects of src/gobjects 

function w=create_midle_menu (win,xc,yc)
// midle button menu construction 
  tearoff=%t;
  menu = gtkmenu_new ();
  if tearoff then 
    menuitem = gtktearoffmenuitem_new ();
    menu.append[  menuitem]
    menuitem.show[];
  end
  menuitem = gtkmenuitem_new(label="delete");
  menuitem.connect["activate",midle_menuitem_response,list(1,win)];
  menu.append[menuitem]
  menuitem.show[];
  //
  menuitem = gtkmenuitem_new(label="add control point");
  menuitem.connect["activate",midle_menuitem_response,list(2,xc,yc,win)];
  menu.append[menuitem]
  menuitem.show[];
  //
  menuitem = gtkmenuitem_new(label="remove control point");
  menuitem.connect["activate",midle_menuitem_response,list(3,xc,yc,win)];
  menu.append[menuitem]
  menuitem.show[];
  //  
  w=menu;
endfunction 

function midle_menuitem_response(w,args) 
// midle button menu activation 
// the midle menu should be a by object menu 
  global('GF');
  printf("Menu item [%d] activated for win=%d\n",args(1),args($));
  win='win'+string(args($))
  select args(1) 
   case 1 then  GF(win).delete_hilited[] ; GF(win).draw[];
   case 2 then  
    GF(win).hilite_near_pt[[args(2),args(3)]];
    GF(win).select_link_and_add_control[[args(2),args(3)]];
   case 3 then  
    GF(win).hilite_near_pt[[args(2),args(3)]];
    GF(win).select_link_and_remove_control[[args(2),args(3)]];
  end
  GF(win).draw[]
endfunction

function w=create_right_menu (win)
// midle button menu construction 
// midle button is for the current selected object 
// if more than one object is selected this menu is deactivated.
  tearoff=%t;
  menu = gtkmenu_new ();
  if tearoff then 
    menuitem = gtktearoffmenuitem_new ();
    menu.append[  menuitem]
    menuitem.show[];
  end
  menuitem = gtkmenuitem_new(label="new link");
  menuitem.connect["activate",menuitem_response,list(1,win)];
  menu.append[menuitem]
  menuitem.show[];
  
  menuitem = gtkmenuitem_new(label="new block");
  menuitem.connect["activate",menuitem_response,list(2,win)];
  menu.append[menuitem]
  menuitem.show[];
  
  menuitem = gtkmenuitem_new(label="new rect");
  menuitem.connect["activate",menuitem_response,list(3,win)];
  menu.append[menuitem]
  menuitem.show[];
  w=menu;
endfunction 

function menuitem_response(w,args) 
// midle button menu activation 
  global('GF');
  printf("Menu item [%d] activated \n",args(1));
  win='win'+string(args($));
  select args(1) 
   case 1 then  GF(win).new_link[] 
   case 2 then  GF(win).new_block[] 
   case 3 then  GF(win).new_rect[] 
  end
  GF(win).draw[]
endfunction


function w=create_menu (depth, length, tearoff)
// provisoire 
  if depth < 1 ;  w=[] ;return; end 
  menu = gtkmenu_new ();
  if tearoff then 
    menuitem = gtktearoffmenuitem_new ();
    menu.append[  menuitem]
    menuitem.show[];
  end
  menuitem = gtkimagemenuitem_new(stock_id="gtk-open");
  menu.append[menuitem]
  menuitem.show[];
    
  for i = 1:length 
    buf = sprintf("radio menu item %2d - %d", depth,i);
    if i==1 then 
      menuitem = gtkradiomenuitem_new(label=buf);
      group = menuitem;
    else
      menuitem = gtkradiomenuitem_new(group=group,label=buf);
    end
    // callback 
    menuitem.connect["activate",menuitem_response,list(buf)];
    menu.append[menuitem]
    menuitem.show[];
    if i == 3 then menuitem.set_sensitive[%f]; end 
    if i == 4 then menuitem.set_inconsistent[ %f]; end 
    if i < 3 then 
      if depth > 1 then 
	menuitem.set_submenu[create_menu(depth - 1,5,%t)];
      end
    end
  end
  w=menu;
endfunction 


function my_eventhandler(win,x,y,ibut)
  global('gr_objects');
  global('GF');
  global('count');
  winid= 'win'+string(win);
  if ~GF.iskey[winid];then
    GF(winid)=%types.GFrame.new[[0,0,100,100],[0,0,100,100],win];
    GF(winid).draw[];// this will fix the initial scale 
  end
    
  if count == 1 then 
    printf("event handler aborted =%d\n",count)
    return
  end
  count = 1;
  if ibut == -100 then 
    printf('window killed ')
  elseif ibut==-1 then 
    [xc,yc]=xchange(x,y,'i2f')
    xinfo('Mouse position is ('+string(xc)+','+string(yc)+')')
  elseif ibut==0 then 
    // left press 
    [xc,yc]=xchange(x,y,'i2f')
    GF(winid).select_and_move[[xc,yc]];
  elseif ibut==1 then 
    // midle press 
    [xc,yc]=xchange(x,y,'i2f')
    popup_m=create_midle_menu (win,xc,yc)
    popup_m.popup[button=1,activate_time=0]; //event.time]; 
  elseif ibut==2 then 
    // right press
    [xc,yc]=xchange(x,y,'i2f')
    [xc,yc]=xchange(x,y,'i2f')
    popup_m=create_right_menu (win)
    popup_m.popup[button=2,activate_time=0]; //event.time]; 
    //popup_menu=create_menu(2,8,%f); 
    //popup_menu.popup[button=3,activate_time=0]; //event.time]; 
  elseif ibut==3 then 
    // a double click 
    x_message('double click');
  elseif ibut==100 
    x_message('100');
    gr_delete();
    gr_draw(win);
  elseif ibut==99
    x_message('99');
    gr_copy();
    gr_draw(win);
  else
    xinfo('Mouse action: ['+string(ibut)+']');
    // test a popup menu 
    //
  end
  count=0;
endfunction

function draw_vanne(rect)
// test function for block drawing 
  orig=[rect(1),rect(2)-rect(4)];
  sz=[rect(3),rect(4)];
  xfpolys(orig(1)+[0;5;7;3;5;10;10;0;0]*sz(1)/10,...
	  orig(2)+[4;2;7;7;2;0;4;0;4]*sz(2)/10,15);
  xfarcs([orig(1)+3*sz(1)/10;orig(2)+sz(2);4*sz(1)/10;6*sz(2)/10;0;180*64],...
	 15)
  xarcs([orig(1)+3*sz(1)/10;orig(2)+sz(2);4*sz(1)/10;6*sz(2)/10;0;180*64],...
	1);
endfunction;

function draw_plot2d(rect)
// test function for block drawing 
  print(rect)
  [wrect,frect,vv,arect]=xgetech();
  wx= (rect(1)-frect(1))/(frect(3)-frect(1));
  wy= abs(rect(2)-frect(4))/(frect(4)-frect(2));
  ww= rect(3)/(frect(3)-frect(1));
  wh= rect(4)/(frect(4)-frect(2));
  xsetech(wrect=[wx,wy,ww,wh],arect=ones(1,4)/8);
  plot2d();
  xsetech(wrect=wrect,frect=frect,arect=arect);
endfunction;


global('GF');
//R=%types.Block.new[[10,20,20,20],color=6,background=7];
GF=hcreate(6);
//GF('win0')=%types.GFrame.new[[0,0,100,100],[0,0,100,100]];
//GF('win0').draw[];

xinit(name='My diagram',dim=[1000,1000],popup_dim=[600,400])
xset('recording',0)
xsetech(arect=[0,0,0,0]);

pause
seteventhandler('my_eventhandler');

xinit(name='My second diagram',dim=[1000,1000],popup_dim=[600,400])
xset('recording',0)
xsetech(arect=[0,0,0,0]);
seteventhandler('my_eventhandler');




