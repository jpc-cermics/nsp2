function [y]=demo_idle(_button)
  y=1
  //if not wins.has_key("idle_test"):
  //	win = GtkDialog()
  win = gtkdialog_new()
  //wins["idle_test"] = win
  win.connect["delete_event",hide];
  //		win.set_title("Idle Test")
  win.set_title["Idle Test"];
  //		label = GtkLabel("count: 0")
  label = gtklabel_new(str="count: 0")
  // label.set_padding(10, 10)
  label.set_padding[10,10];
  //		win.vbox.pack_start(label)
  win.vbox.pack_start[label]
  //		label.show()
  label.show[]

  // function to be executed when idle 
  function [y]=idle_test(args)
    y=1
    count=args(1).get_data['idle_count'];
    count = count + 1;
    args(1).set_data[idle_count=count];
    //label.set_text("count: " + str(count[0]))
    args(2).set_text["count: " + m2s(count,"%5.0f")];
  endfunction 

  // starts the idle handler 
  function [y]=start_idle_test(_button,args)
    y=1
    if args(1).get_data['idle_id'] == 0 then 
      id = gtk_idle_add(200,args(1).get_data['idle_f'],args)
      args(1).set_data[idle_id=id ]
    end 
  endfunction
  
  // stops  the idle handler 
  function [y]=stop_idle_test(_button,args) 
    y=1
    id = args(1).get_data['idle_id'];
    if id <> 0 then 
      gtk_idle_remove(id);
      args(1).set_data[idle_id=0];
    end
  endfunction

  // attach data to the main windget 
  // since they must be shared between handlers 
  // and exists when returning from this function. 
  win.set_data[idle_f = idle_test];
  win.set_data[idle_id = 0];
  win.set_data[idle_count = 0];
  
  button = gtkbutton_new(label="close")
  button.connect["clicked",win_hide,list(win)];
  win.action_area.pack_start[button]
  button.set_flags[GTK.CAN_DEFAULT]
  button.grab_default[]
  button.show[]
  button = gtkbutton_new(label="start")
  button.connect["clicked", start_idle_test,list(win,label)];
  win.action_area.pack_start[button]
  button.show[]
  button = gtkbutton_new(label="stop")
  button.connect["clicked", stop_idle_test,list(win)];
  win.action_area.pack_start[button];
  button.show[]
  win.show[]
  // gtk_main()
endfunction
