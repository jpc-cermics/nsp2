// Display & Screen test 

function screen_display_check (widget,args)
  data= args(1).get_data['dpy_data'];
  // data = list( combo_dpy.entry,  radio_dpy, window.get_toplevel[], valid_display_list) 
  display = widget.get_display[];
  current_screen = widget.get_screen[];
  new_screen = [];
  if  data(2).get_active[] then 
    display_name = data(1).get_text[];
    display = gdk_display_open(display_name);
    if is(display,%types.None) then 
      dialog = gtkmessagedialog_new(parent=widget.get_toplevel[], ...
				    flags=GTK.DIALOG_DESTROY_WITH_PARENT,...
				    type=GTK.MESSAGE_ERROR,...
				    buttons=GTK.BUTTONS_OK,...
				    message="The display :\n"+display_name+"\ncannot be opened");
      dialog.set_screen[  current_screen]
      dialog.show[];
      dialog.connect["response",hide]// destroy 
      else
	I=find(display_name == data(4))
	if I<>[] then data(5)=[data(4);display_name];end 
	new_screen = display.get_default_screen[];
    end 
  else
    number_of_screens = display.get_n_screens[];
    screen_num = current_screen.get_number[];
    if ((screen_num +1) < number_of_screens)
      new_screen = display.get_screen[ screen_num + 1];
    else
      new_screen = display.get_screen[ 0];
    end 
  end 
  if is(new_screen,%types.GdkScreen)
    // changing screen of window tolevel 
    data(3).set_screen[ new_screen];
  end 
endfunction 

function demo_display_screen () 
  opts=hcreate(type= GTK.WINDOW_TOPLEVEL,title="Screen or Display selection",...
	       border_width= 10);
  window = gtkwidget_new(%types.GtkWindow,opts);
  screen = window.get_screen[];
  display = screen.get_display[]; 

  //  window.connect[  "destroy", gtk_widget_destroy, NULL]

  vbox = gtkvbox_new(homogeneous=%f,spacing=3);
  window.add[  vbox]
  
  frame = gtkframe_new(label="Select screen or display");
  vbox.add[  frame]
  
  table = gtktable_new(rows=2,columns=2,homogeneous=%t);
  table.set_row_spacings[  3]
  table.set_col_spacings[  3]

  frame.add[  table]

  radio_dpy = gtkradiobutton_new (label="move to another X display");
  
  ns= display.get_n_screens[]; 
  
  if ns > 1 
    radio_scr = gtkradiobutton_new(group=radio_dpy,label= "move to next screen");
  else
    radio_scr = gtkradiobutton_new(group=radio_dpy,label= "only one screen on the current display");
    radio_scr.set_sensitive[%f]
  end 

  combo_dpy = gtkcombo_new ();

  valid_display_list = "diabolo:0.0"; 
    
  combo_dpy.set_popdown_strings[ valid_display_list];
    
  combo_dpy.entry.set_text["<hostname>:<X Server Num>.<Screen Num>"];

  table.attach_defaults[  radio_dpy, 0, 1, 0, 1]
  table.attach_defaults[  radio_scr, 0, 1, 1, 2]
  table.attach_defaults[  combo_dpy, 1, 2, 0, 1]

  bbox = gtkhbuttonbox_new ();
  applyb = gtkbutton_new(stock = "gtk-apply");
  cancelb = gtkbutton_new(stock = "gtk-cancel");
  
  vbox.add[  bbox]

  bbox.add[  applyb]
  bbox.add[  cancelb]

  scr_dpy_data = list( combo_dpy.entry,  radio_dpy, window.get_toplevel[], valid_display_list) 
  window.set_data[ dpy_data = scr_dpy_data]; 
  // cancelb.connect["clicked", screen_display_destroy_diag,list(window)]
  applyb.connect["clicked", screen_display_check, list(window) ]
  window.show_all[];
endfunction 

