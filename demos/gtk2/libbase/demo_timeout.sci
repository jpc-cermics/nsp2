// timeout work properly with gtk_main() 
// but when we are at the nsp prompt 
// timeout are not properly activated 
// (i.e they are losing activation time) 

function demo_timeout(_button)
  win = gtkdialog_new()
  win.connect["delete_event", hide];
  win.set_title["Timeout Test"];
  label = gtklabel_new(str="count: 0")
  label.set_padding[10,10]
  win.vbox.pack_start[label];
  label.show[]
  
  // function to be executed when timeout 
  function [y]=timeout_test(args)
    y=1
    count=args(1).get_data['timeout_count'];
    count = count + 1;
    args(1).set_data[timeout_count=count];
    //label.set_text("count: " + str(count[0]))
    args(2).set_text["count: " + m2s(count,"%5.0f")];
  endfunction 

  // starts the timeout handler 
  function [y]=start_timeout_test(_button,args)
    y=1
    if args(1).get_data['timeout_id'] == 0 then 
      id = gtk_timeout_add(100,args(1).get_data['timeout_f'],args)
      args(1).set_data[timeout_id=id ]
    end 
  endfunction
  
  // stops  the timeout handler 
  function [y]=stop_timeout_test(_button,args) 
    y=1
    id = args(1).get_data['timeout_id'];
    if id <> 0 then 
      gtk_timeout_remove(id);
      args(1).set_data[timeout_id=0];
    end
  endfunction

  // attach data to the main windget 
  // since they must be shared between handlers 
  // and exists when returning from this function. 
  win.set_data[timeout_f = timeout_test];
  win.set_data[timeout_id = 0];
  win.set_data[timeout_count = 0];
  
  button = gtkbutton_new(label="close")
  button.connect["clicked",win_hide,list(win)];
  win.action_area.pack_start[button]
  button.set_flags[GTK.CAN_DEFAULT]
  button.grab_default[]
  button.show[]
  button = gtkbutton_new(label="start")
  button.connect["clicked", start_timeout_test,list(win,label)];
  win.action_area.pack_start[button]
  button.show[]
  button = gtkbutton_new(label="stop")
  button.connect["clicked", stop_timeout_test,list(win)];
  win.action_area.pack_start[button];
  button.show[]
  win.show[]
endfunction
