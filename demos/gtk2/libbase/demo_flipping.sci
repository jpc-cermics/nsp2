
// flipping 

function flipping_toggled_cb (widget, data)
  if  widget.get_active[] then 
     gtk_widget_set_default_direction[GTK.TEXT_DIR_RTL];
  else 
     gtk_widget_set_default_direction[GTK.TEXT_DIR_LTR]
  end 
endfunction 


function set_direction_recurse (widget, data)
  widget.set_direction[data(1)]
  if is(widget,%types.GtkContainer) then   
    widget.foreach[ set_direction_recurse,data]
  end 
endfunction 

function frame= create_forward_back (title, text_dir)
  frame = gtkframe_new(label=title);
  bbox = gtkhbuttonbox_new ();
  back_button = gtkbutton_new(stock="gtk-go-back");
  forward_button = gtkbutton_new(stock="gtk-go-forward");
  bbox.set_border_width[  5]
  frame.add[  bbox]
  bbox.add[  back_button]
  bbox.add[  forward_button]
  set_direction_recurse(frame,list(text_dir));
endfunction 

function demo_flipping ()
  window = gtkdialog_new ();
  //     window.connect[  "destroy",hide]
  window.set_title[  "Bidirectional Flipping"]

  check_button = gtkcheckbutton_new(label="Right-to-left global direction");
  window.vbox.pack_start[ check_button,expand=%t,fill=%t,padding=0];
  window.vbox.pack_start[ create_forward_back ("Default", GTK.TEXT_DIR_NONE)]; 
  window.vbox.pack_start[ create_forward_back ("Left-to-Right", GTK.TEXT_DIR_LTR)];
  window.vbox.pack_start[ create_forward_back ("Right-to-Left", GTK.TEXT_DIR_RTL)];

  if gtk_widget_get_default_direction () == GTK.TEXT_DIR_RTL then 
    check_button.set_active[  %t]
  end
  check_button.connect[  "toggled",	flipping_toggled_cb, list(%f)]
  check_button.set_border_width[  10]
  button = gtkbutton_new(label="Close");
  button.connect["clicked", button_destroy_win,list(window)];
  window.action_area.pack_start[button];
  window.show_all[];
endfunction 

