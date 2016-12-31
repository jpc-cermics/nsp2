// Reparent 

function demo_reparent()
  win = gtk_window_new()
  win.connect["delete_event", demo_delete];
  win.set_title["buttons"];
  box1 = gtk_box_new("vertical",spacing=0)
  win.add[box1]
  box1.show[]
  box2 = gtk_box_new("horizontal",spacing=5)
  box2.set_border_width[10]
  box1.pack_start[box2]
  box2.show[]
  label = gtk_label_new(str="Hello World")
  frame = gtk_frame_new(label="Frame 1")
  box2.pack_start[frame]
  frame.show[]
  box3 = gtk_box_new("vertical",spacing=5)
  box3.set_border_width[5]
  frame.add[box3]
  box3.show[]
  function reparent_label(button,args)
    args(1).reparent[args(2)]
  endfunction 
  button = gtk_button_new(label="switch")
  button.connect["clicked", reparent_label,list(label,box3)];
  box3.pack_start[button,expand=%f,fill=%t,padding=0]
  button.show[]
  box3.pack_start[label,expand=%f,fill=%t,padding=0]
  label.show[]
  frame = gtk_frame_new(label="Frame 2")
  box2.pack_start[frame]
  frame.show[]
  box4 = gtk_box_new("vertical",spacing=5)
  box4.set_border_width[5]
  frame.add[box4]
  box4.show[]
  button = gtk_button_new(label="switch")
  button.connect["clicked", reparent_label,list(label,box4)];
  box4.pack_start[button,expand=%f,fill=%t,padding=0]
  button.show[]
  separator = gtk_separator_new("horizontal")
  box1.pack_start[separator,expand=%f,fill=%t,padding=0]
  separator.show[]
  box2 = gtk_box_new("vertical",spacing=10)
  box2.set_border_width[10]
  box1.pack_start[box2,expand= %f,fill=%t,padding=0]
  box2.show[]
  button = gtk_button_new(label="close")
  button.connect["clicked",button_destroy_win,list(win)];
  box2.pack_start[button]
  //button.set_flags[GTK.CAN_DEFAULT]
  button.set_can_default[%t]
  button.grab_default[]
  button.show[]
  win.show[]
  //gtk_main()
endfunction 
