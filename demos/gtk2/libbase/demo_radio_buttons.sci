// GtkRadioButton OK 
//-----------------------------------------------

function demo_radio_buttons()
  //if not wins.has_key("radio_buttons"):
  win = gtkwindow_new()
  win.connect["delete_event",fill= hide];
  win.set_title["radio buttons"];
  box1 = gtkvbox_new(homogeneous=%f,spacing=0)
  win.add[box1]
  box1.show[]
  box2 = gtkvbox_new(homogeneous=%f,spacing=0)
  box2.set_border_width[10]
  box1.pack_start[box2]
  box2.show[]
  button1=gtkradiobutton_new(label="button1")
  box2.pack_start[button1]
  button1.show[]
  for i =2:3
    button=gtkradiobutton_new(group=button1,label= "button" + m2s(i,"%0.f"))
    box2.pack_start[button]
    button.show[]
  end 
  separator = gtkhseparator_new()
  box1.pack_start[separator,expand=%f,fill= %f,padding=0]
  separator.show[]
  box2 = gtkvbox_new(homogeneous=%f,spacing=10)
  box2.set_border_width[10]
  box1.pack_start[box2,expand=%f,fill=%t,padding=0]
  box2.show[]
  button = gtkbutton_new(label="Close")
  button.connect["clicked",win_hide,list(win)];
  box2.pack_start[button]
  button.set_flags[GTK.CAN_DEFAULT]
  button.grab_default[]
  button.show[]
  win.show[]
  //gtk_main()
endfunction
