// GtkScrolledWindow OK 
//----------------------------------------------------

function demo_scrolled_windows()
  win = gtkdialog_new()
  win.connect["delete_event", hide];
  win.set_title["dialog"];
  scrolled_window= gtkscrolledwindow_new()
  scrolled_window.set_border_width[10]
  scrolled_window.set_policy[GTK.POLICY_AUTOMATIC, GTK.POLICY_AUTOMATIC]
  win.vbox.pack_start[scrolled_window]
  scrolled_window.show[]
  table = gtktable_new(rows=20,columns=20,homogeneous=%t)
  table.set_row_spacings[10]
  table.set_col_spacings[10]
  scrolled_window.add_with_viewport[table];
  table.show[]
  for i = 0:19
    for j = 0:19
      button = gtktogglebutton_new(label=sprintf("button (%d,%d)",i,j));
      xoptions=ior(GTK.EXPAND,GTK.FILL)
      yoptions=ior(GTK.EXPAND,GTK.FILL)
      xpadding=0, ypadding=0
      table.attach[button,i,i+1,j,j+1,xoptions=xoptions,yoptions=yoptions, xpadding=0,ypadding=0];
      button.show[]
    end
  end 
  button = gtkbutton_new(label="close")
  button.connect["clicked", win_hide,list(win)];
  win.action_area.pack_start[button]
  button.set_flags[GTK.CAN_DEFAULT]
  button.grab_default[]
  button.show[]
  win.show[]
  //gtk_main()
endfunction

