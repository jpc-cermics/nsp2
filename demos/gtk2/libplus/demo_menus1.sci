//
// Menu demo
// 

function menuitem_response(w,args) 
  printf("Menu item [%s] activated \n",args(1));
endfunction

function w=create_menu (depth, length, tearoff)
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

function y=button_popup_pressed(widget,event,args) 
  menu=args(1); 
  if event.type == GDK.BUTTON_PRESS then 
    printf("button=%d time = %d\n",event.button,event.time);
    menu.popup[button=event.button,activate_time=0]; //event.time]; 
    y=%t 
  else
    y=%f
  end
endfunction

function demo_menus1() 
  window = gtkwindow_new();// (GTK.WINDOW_TOPLEVEL);
  //window.set_screen[  screen]
  //window.connect["destroy",hide];
  //window.connect["delete-event",hide]; 
      
  accel_group = gtkaccelgroup_new ();
  window.add_accel_group[  accel_group]

  window.set_title[  "menus"]
  window.set_border_width[  0]
        
  box1 = gtkvbox_new(homogeneous=%f,spacing=0);
  window.add[box1]
  box1.show[];
      
  menubar = gtkmenubar_new ();
  box1.pack_start[ menubar,expand=%f,fill=%t,padding=0]
  menubar.show[];
      
  menu = create_menu(2,50,%t);
      
  menuitem = gtkmenuitem_new(label="test\nline2");
  menuitem.set_submenu[menu];
  menubar.append[  menuitem]
  menuitem.show[];
      
  menuitem = gtkmenuitem_new(label="foo");
  menuitem.set_submenu[create_menu ( 3, 5, %t)];
  menubar.append[  menuitem]
  menuitem.show[];

  menuitem = gtkimagemenuitem_new(stock_id="gtk-help");
  menuitem.set_submenu[ create_menu (4, 5, %t)];
  menuitem.set_right_justified[%t];
  menubar.append[  menuitem]
  menuitem.show[];
      
  menubar = gtkmenubar_new ();
  box1.pack_start[ menubar,expand=%f,fill=%t,padding=0]
  menubar.show[];

  menuitem = gtkmenuitem_new(label="Second menu bar");
  menuitem.set_submenu[create_menu (2,10,%t)];
  menubar.append[menuitem]
  menuitem.show[];
  
  box2 = gtkvbox_new(homogeneous=%f,spacing=10);
  box2.set_border_width[  10]
  box1.pack_start[ box2,expand=%t,fill=%t,padding=0]
  box2.show[];
  
  menu = create_menu ( 1, 5, %f);
  menu.set_accel_group[ accel_group];
  
  menuitem = gtkimagemenuitem_new(stock_id="gtk-new",accel_group= accel_group);
  menu.append[  menuitem]
  menuitem.show[];
  
  menuitem = gtkcheckmenuitem_new(label="Accelerate Me");
  menu.append[  menuitem]
  menuitem.show[];
  //menuitem.add_accelerator[ "activate", accel_group, GDK_F1, 0,  GTK.ACCEL_VISIBLE]
  menuitem = gtkcheckmenuitem_new(label="Accelerator Locked");
  menu.append[  menuitem]
  menuitem.show[];
  //menuitem.add_accelerator[ "activate", accel_group, GDK_F2, 0, ior(GTK.ACCEL_VISIBLE,GTK.ACCEL_LOCKED)]
  menuitem = gtkcheckmenuitem_new(label="Accelerators Frozen");
  menu.append[  menuitem]
  menuitem.show[];
  //menuitem.add_accelerator[ "activate", accel_group, GDK_F2, 0, GTK.ACCEL_VISIBLE]
  //menuitem.add_accelerator[  "activate", accel_group, GDK_F3, 0, GTK.ACCEL_VISIBLE]
      
  optionmenu = gtkoptionmenu_new ();
  optionmenu.set_menu[menu]
  optionmenu.set_history[3]
  box2.pack_start[ optionmenu,expand=%t,fill=%t,padding=0]
  optionmenu.show[];
  
  separator = gtkhseparator_new ();
  box1.pack_start[ separator,expand=%f,fill=%t,padding=0]
  separator.show[];

  box2 = gtkvbox_new(homogeneous=%f,spacing=10);
  box2.set_border_width[  10]
  box1.pack_start[ box2,expand=%f,fill=%t,padding=0]
  box2.show[];

  // test a popup menu 
  popup_menu=create_menu(1,5,%f); 
    
  button = gtkbutton_new(label="popup"); 
  button.connect["event", button_popup_pressed ,list(popup_menu)];
  box2.pack_start[button,expand=%t,fill=%t,padding=0]
  button.show[];
  
  button = gtkbutton_new(label="close"); 
  button.connect["clicked", button_destroy_win,list(window)];
  box2.pack_start[ button,expand=%t,fill=%t,padding=0]
  button.set_flags[GTK.CAN_DEFAULT];
  button.grab_default[];
  button.show[];

  window.show[];
endfunction 

