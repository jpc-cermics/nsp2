// Tooltips.
// 

function demo_tooltips()
  win = gtkwindow_new()
  win.connect["delete_event", hide];
  win.set_title["tooltips"];
  tooltips = gtktooltips_new()
  box1 = gtkvbox_new(homogeneous=%f,spacing=0)
  win.add[box1]
  box1.show[]
  box2 = gtkvbox_new(homogeneous=%f,spacing=10)
  box2.set_border_width[10]
  box1.pack_start[box2]
  box2.show[]
  button = gtktogglebutton_new(label="button1")
  box2.pack_start[button]
  button.show[]
  tooltips.set_tip[ button ,"This is button 1", tip_private="button 1"];
  button = gtktogglebutton_new(label="button2")
  box2.pack_start[button]
  button.show[]
  tooltips.set_tip[ button ,"This is button 2", tip_private="button 2"];
  button = gtktogglebutton_new(label="button3");
  box2.pack_start[button]
  button.show[]
  title= "This is button 3.  This is also a really long tooltip which probably won''t fit on a single line and will therefore need to be wrapped.  Hopefully the wrapping will work correctly."
  tooltips.set_tip[button,title,tip_private= "long"];
  separator = gtkhseparator_new()
  box1.pack_start[separator,expand= %f,fill=%t,padding=0]
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
  tooltips.set_tip[ button , "Push this button to close window",tip_private="push"];
  tooltips.enable[];
  win.show[]
endfunction 

