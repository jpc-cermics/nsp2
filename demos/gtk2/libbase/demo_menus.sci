// Menus OK 
//---------------------------------------------

function [menu]=demo_menu(depth)
  if depth < 1 then return; end;
  // menu = GtkMenu()
  menu = gtkmenu_new()
  i=1; 
  menuitem= gtkradiomenuitem_new(label=sprintf("item %2d - %d",depth, i))  
  group = menuitem;
  menu.append[menuitem];
  for i = 2:4 
    menuitem= gtkradiomenuitem_new(group=group,label=sprintf("item %2d - %d",depth, i))  
    menu.append[menuitem];
    menuitem.show[]
    if depth > 1 then 
      submenu = demo_menu(depth - 1)
      menuitem.set_submenu[submenu];
    end
  end
endfunction 

function demo_menus()
  win = gtkwindow_new()
  win.connect["delete_event", hide];
  win.set_title["menus"];
  box1 = gtkvbox_new(homogeneous=%f,spacing=0)
  win.add[box1]
  box1.show[]
  menubar = gtkmenubar_new()
  box1.pack_start[menubar,expand= %f,fill=%t,padding=0]
  menubar.show[]
  menuitem = gtkmenuitem_new("test\nline2")
  menuitem.set_submenu[demo_menu(2)];
  menubar.append[menuitem];
  menuitem.show[]
  
  menuitem = gtkmenuitem_new("foo");
  menuitem.set_submenu[demo_menu(2)];
  menubar.append[menuitem];
  menuitem.show[]
  
  menuitem = gtkmenuitem_new("bar");
  menuitem.set_submenu[demo_menu(2)];
  menuitem.set_right_justified[%t];
  menubar.append[menuitem];
  menuitem.show[]

  box2 = gtkvbox_new(homogeneous=%f,spacing=10)
  box2.set_border_width[10]
  box1.pack_start[box2]
  box2.show[]
  
  optionmenu = gtkoptionmenu_new()
  optionmenu.set_menu[demo_menu(1)];
  box2.pack_start[optionmenu]
  optionmenu.show[]
  separator = gtkhseparator_new()
  box1.pack_start[separator,padding= 0]
  separator.show[]
  box2 = gtkvbox_new(homogeneous=%f,spacing=10)
  box2.set_border_width[10]
  box1.pack_start[box2,expand= %f,fill=%t,padding=0]
  box2.show[]
  button = gtkbutton_new(label="close")
  button.connect["clicked", win_hide,list(win)];
  box2.pack_start[button]
  button.set_flags[GTK.CAN_DEFAULT]
  button.grab_default[]
  button.show[]
  win.show[]
  // gtk_main()
endfunction
