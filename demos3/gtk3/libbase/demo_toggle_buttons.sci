// GtkToggleButton 

function []=demo_toggle_buttons()
  win = gtk_window_new()
  win.connect["delete_event",demo_delete];
  win.set_title["toggle butttons"];
  win.set_border_width[0]
  box1 = gtk_box_new("vertical",spacing=0)
  win.add[box1]
  box1.show[]
  box2 = gtk_box_new("vertical",spacing=10)
  box2.set_border_width[10]
  box1.pack_start[box2]
  box2.show[]
  for i = 0:3 
    button = gtk_toggle_button_new(label="button" + m2s(i,fill="%0.f"))
    box2.pack_start[button]
    button.show[]
  end
  separator = gtk_separator_new("horizontal")
  box1.pack_start[separator,expand=%f,fill=%t,padding=0]
  separator.show[]
  box2 = gtk_box_new("vertical",spacing=10)
  box2.set_border_width[10]
  box1.pack_start[box2,expand=%f,fill=%t,padding=0]
  box2.show[]
  button = gtk_button_new(label="close")
  button.connect["clicked", button_destroy_win,list(win)];
  box2.pack_start[button]
  //button.set_flags[GTK.CAN_DEFAULT]
  button.set_can_default[%t]
  button.grab_default[] 
  button.show[]
  win.show[]
  //gtk_main()
endfunction
