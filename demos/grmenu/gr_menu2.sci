// test program 
// a new scicos editor 
// this works with objects of src/gobjects 

function w=create_midle_menu (xc,yc)
// midle button menu construction 
  tearoff=%t;
  menu = gtkmenu_new ();
  if tearoff then 
    menuitem = gtktearoffmenuitem_new ();
    menu.append[  menuitem]
    menuitem.show[];
  end
  menuitem = gtkmenuitem_new(label="delete");
  menuitem.connect["activate",midle_menuitem_response,list(1)];
  menu.append[menuitem]
  menuitem.show[];
  //
  menuitem = gtkmenuitem_new(label="add control point");
  menuitem.connect["activate",midle_menuitem_response,list(2,xc,yc)];
  menu.append[menuitem]
  menuitem.show[];
  //
  menuitem = gtkmenuitem_new(label="remove control point");
  menuitem.connect["activate",midle_menuitem_response,list(3,xc,yc)];
  menu.append[menuitem]
  menuitem.show[];
  //  
  w=menu;
endfunction 

function midle_menuitem_response(w,args) 
// midle button menu activation 
// the midle menu should be a by object menu 
  global('GF');
  printf("Menu item [%d] activated \n",args(1));
  select args(1) 
   case 1 then  GF.delete_hilited[] ; GF.draw[];
   case 2 then  
    GF.hilite_near_pt[[args(2),args(3)]];
    GF.select_link_and_add_control[[args(2),args(3)]];
   case 3 then  
    GF.hilite_near_pt[[args(2),args(3)]];
    GF.select_link_and_remove_control[[args(2),args(3)]];
  end
  GF.draw[]
endfunction

function w=create_right_menu ()
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
  menuitem.connect["activate",menuitem_response,list(1)];
  menu.append[menuitem]
  menuitem.show[];
  
  menuitem = gtkmenuitem_new(label="new block");
  menuitem.connect["activate",menuitem_response,list(2)];
  menu.append[menuitem]
  menuitem.show[];
  
  menuitem = gtkmenuitem_new(label="new rect");
  menuitem.connect["activate",menuitem_response,list(3)];
  menu.append[menuitem]
  menuitem.show[];
  w=menu;
endfunction 

function menuitem_response(w,args) 
// midle button menu activation 
  global('GF');
  printf("Menu item [%d] activated \n",args(1));
  select args(1) 
   case 1 then  GF.new_link[] 
   case 2 then  GF.new_block[] 
   case 3 then  GF.new_rect[] 
  end
  GF.draw[]
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

global('GF');
a=rand(6,6)*20;
driver('X11');
xset('pixmap',1);
xset('window',0)
xset('wdim',800,800)
xset('wpdim',400,400)

R=%types.Block.new[[10,20,20,20],color=6,background=7];
GF=%types.GFrame.new[[0,0,100,100],[0,0,100,100],R];
GF.draw[];
//GF.new_block[]
//GF.new_block[]
//GF.new_block[]
//GF.new_link[]
//GF.new_connector[]
//GF.draw[];

function my_eventhandler(win,x,y,ibut)
  global('gr_objects');
  global('GF');
  global('count');
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
    GF.select_and_move[[xc,yc]];
  elseif ibut==1 then 
    // midle press 
    [xc,yc]=xchange(x,y,'i2f')
    popup_m=create_midle_menu (xc,yc)
    popup_m.popup[button=1,activate_time=0]; //event.time]; 
  elseif ibut==2 then 
    // right press
    [xc,yc]=xchange(x,y,'i2f')
    [xc,yc]=xchange(x,y,'i2f')
    popup_m=create_right_menu ()
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

seteventhandler('my_eventhandler');



