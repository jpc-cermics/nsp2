// Tooltips.

function demo_tooltips()
// demo of set_tooltip_text[] and set_tooltip_markup[]
  win = gtkwindow_new()
  win.connect["delete_event", demo_delete];
  win.set_title["tooltips"];
  box1 = gtkbox_new("vertical",spacing=0)
  win.add[box1]
  box1.show[]
  box2 = gtkbox_new("vertical",spacing=10)
  box2.set_border_width[10]
  box1.pack_start[box2]
  box2.show[]
  button = gtktogglebutton_new(label="button1")
  box2.pack_start[button]
  button.show[],
  button.set_tooltip_text["This is a tooltip text for button 1"];
  button = gtktogglebutton_new(label="button2")
  box2.pack_start[button]
  button.show[]
  button.set_tooltip_markup["This is a <span foreground=''blue''>tooltip markup</span>"];
  button = gtktogglebutton_new(label="button3");
  box2.pack_start[button]
  button.show[]
  button.set_tooltip_text["This is button 3.\nThis is also a long\ntooltip on several lines\n"];
  separator = gtkseparator_new("horizontal")
  box1.pack_start[separator,expand= %f,fill=%t,padding=0]
  separator.show[]
  box2 = gtkbox_new("vertical",spacing=10)
  box2.set_border_width[10]
  box1.pack_start[box2,expand= %f,fill=%t,padding=0]
  box2.show[]
  button = gtkbutton_new(label="close")
  button.connect["clicked", button_destroy_win,list(win)];
  box2.pack_start[button]
  //button.set_flags[GTK.CAN_DEFAULT]
  button.grab_default[]
  button.show[]
  button.set_tooltip_text["Push this button to close window"];
  win.show[]
endfunction
