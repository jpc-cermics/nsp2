
// GtkStatusBar OK 
//-----------------------------------------------

function []=demo_statusbar()
  win = gtkdialog_new()
  win.connect[ "delete_event", demo_delete];
  win.set_title["Status Bar Test"];	

  statusbar = gtkstatusbar_new()
  win.vbox.add[statusbar];
  statusbar.show[]
  c = statusbar.get_context_id["test"];
  statusbar.push[c,"First message"]
  statusbar.set_data[statusbar_data=0];
  
  function []=pop_statusbar_test(but,args)
    args(1).pop[args(2)];
  endfunction 

  function []=push_statusbar_test(but,args) 
    count= args(1).get_data['statusbar_data'];
    args(1).set_data[statusbar_data=count+1];
    args(1).push[args(2),"Message "+m2s(count,"%5.0f")];
  endfunction 
  
  hbox = win.action_area; // .pack_start[vbox]; // XXXX

  button = gtkbutton_new(label="Pop")
  button.connect["clicked", pop_statusbar_test,list(statusbar,c)];
  hbox.add[button]
  button.show[]
  button = gtkbutton_new(label="Push")
  button.connect["clicked", push_statusbar_test,list(statusbar,c)];
  hbox.add[button]
  button.show[]	
  button = gtkbutton_new(label="Close")
  button.connect["clicked",button_destroy_win,list(win)];
  hbox.add[button]
  button.set_flags[GTK.CAN_DEFAULT]
  button.grab_default[]
  button.show[]	
  win.show[]
  //gtk_main()
endfunction 
