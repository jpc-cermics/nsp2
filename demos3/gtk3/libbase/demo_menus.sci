// Menus
//
// There are several widgets involved in displaying menus. The
// GtkMenuBar widget is a menu bar, which normally appears horizontally
// at the top of an application, but can also be layed out vertically.
// The GtkMenu widget is the actual menu that pops up. Both GtkMenuBar
// and GtkMenu are subclasses of GtkMenuShell; a GtkMenuShell contains
// menu items (GtkMenuItem). Each menu item contains text and/or images
// and can be selected by the user.
//
// There are several kinds of menu item, including plain GtkMenuItem,
// GtkCheckMenuItem which can be checked/unchecked, GtkRadioMenuItem
// which is a check menu item that's in a mutually exclusive group,
// GtkSeparatorMenuItem which is a separator bar, GtkTearoffMenuItem
// which allows a GtkMenu to be torn off, and GtkImageMenuItem which
// can place a GtkImage or other widget next to the menu text.
//
// A GtkMenuItem can have a submenu, which is simply a GtkMenu to pop
// up when the menu item is selected. Typically, all menu items in a menu bar
// have submenus.

function demo_menus()

  function [menu]=demo_menu(depth)
    if depth < 1 then return; end;
    // menu = GtkMenu()
    menu = gtk_menu_new()
    i=1;
    menuitem= gtk_radio_menu_item_new(label=sprintf("item %2d - %d",depth, i))
    group = menuitem;
    menu.append[menuitem];
    for i = 2:4
      menuitem= gtk_radio_menu_item_new(group=group,label=sprintf("item %2d - %d",depth, i))
      menu.append[menuitem];
      menuitem.show[]
      if depth > 1 then
	submenu = demo_menu(depth - 1)
	menuitem.set_submenu[submenu];
      end
    end
  endfunction
  
  win = gtk_window_new()
  win.connect["delete_event", demo_delete];
  win.set_title["menus"];
  box1 = gtk_box_new("vertical",spacing=0)
  win.add[box1]
  box1.show[]
  menubar = gtk_menu_bar_new()
  box1.pack_start[menubar,expand= %f,fill=%t,padding=0]
  menubar.show[]
  menuitem = gtk_menu_item_new("test\nline2")
  menuitem.set_submenu[demo_menu(2)];
  menubar.append[menuitem];
  menuitem.show[]

  menuitem = gtk_menu_item_new("foo");
  menuitem.set_submenu[demo_menu(2)];
  menubar.append[menuitem];
  menuitem.show[]

  menuitem = gtk_menu_item_new("bar");
  menuitem.set_submenu[demo_menu(2)];
  // menuitem.set_right_justified[%t];
  menubar.append[menuitem];
  menuitem.show[]

  box2 = gtk_box_new("vertical",spacing=10)
  box2.set_border_width[10]
  box1.pack_start[box2]
  box2.show[]
  
  separator = gtk_separator_new("horizontal")
  box1.pack_start[separator,padding= 0]
  separator.show[]
  box2 = gtk_box_new("vertical",spacing=10)
  box2.set_border_width[10]
  box1.pack_start[box2,expand= %f,fill=%t,padding=0]
  box2.show[]
  button = gtk_button_new(label="close")
  button.connect["clicked", button_destroy_win,list(win)];
  box2.pack_start[button]
  button.set_can_default[%t]
  button.grab_default[]
  button.show[]
  win.show[]
  // gtk_main()
endfunction
