
// GtkChekButton OK 
//-----------------------------------------------

function []=demo_check_buttons()
  win = gtkwindow_new()
  win.connect["delete_event",hide];
  win.set_name["check buttons"];
  box1 = gtkvbox_new(homogeneous=%f,spacing=0)
  win.add[box1]
  box1.show[]
  box2 = gtkvbox_new(homogeneous=%f,spacing=10)
  box2.set_border_width[10]
  box1.pack_start[box2]
  box2.show[]
  for i = 0:3 
    button = gtkcheckbutton_new(label="button" +  m2s(i,expand="%0.f"))
    box2.pack_start[button]
    button.show[]
  end
  separator = gtkhseparator_new()
  box1.pack_start[separator,expand=%f,fill=%t,padding=0]
  separator.show[]
  box2 = gtkvbox_new(homogeneous=%f,spacing=10)
  box2.set_border_width[10]
  box1.pack_start[box2,expand=%f,fill=%t,padding=0]
  box2.show[]
  button = gtkbutton_new(label="close")
  button.connect["clicked", win_hide,list(win)];
  box2.pack_start[button]
  button.set_flags[GTK.CAN_DEFAULT]
  button.grab_default[]
  button.show[]
  win.show[]
  //gtk_main()
endfunction
