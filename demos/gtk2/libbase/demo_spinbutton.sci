
//----------------------------------------------------------------------------
// GtkSpinButton demo 
// version ou les handler n'utilise pas de var globales 
//----------------------------------------------------------------------------

function []=toggle_snap (widget, spin)
  if widget.get_active[] then 
    printf("dans toggle snap avec active = %%t \n",6);
  else
    printf("dans toggle snap avec active = %%f \n",7);
  end
  spin(1).set_snap_to_ticks[ widget.get_active[]];
endfunction

function []=toggle_numeric (widget, spin)
  spin(1).set_numeric[widget.get_active[]];
endfunction

function []=change_digits(widget,spin)
  spin(1).set_digits[ spin(2).get_value_as_int[]];
endfunction

function []=get_value(widget,data) 
  printf("dans get_value \n",6);
  label = widget.get_data['user_data'];
  if data(1) == 1 
    buf=sprintf( "%d", data(2).get_value_as_int[]);
  else 
    digits = data(2).get_digits[];
    buf=sprintf ( "%0.*f",digits,  data(2).get_value[]);
  end 
  label.set_text[buf];
endfunction 

function []=do_quit(win,L)
  // the first argument is win or button 
  // depending on the way do_quit is called 
  // printf("dans do_quit \n",6);
  window=L(1);
  // printf('quit spins');
  window.hide[];
  window.destroy[];
  //gtk_main_quit()
endfunction 

function []=do_delete(win,L)
  // the first argument is win or button 
  win.destroy[];
  //gtk_main_quit()
endfunction 

function []=demo_spinbutton()
  window = gtkwindow_new()
  window.connect[ "delete_event", do_delete,list(window)];
  window.connect[ "destroy", do_quit,list(window)];
  window.set_title[ "GtkSpinButton"];

  main_vbox = gtkvbox_new (homogeneous=%f,spacing= 5);
  main_vbox.set_border_width[ 10];
  window.add[ main_vbox];
  frame = gtkframe_new(label="Not accelerated");
  main_vbox.pack_start[ frame];
      
  vbox = gtkvbox_new (homogeneous=%f,spacing= 0);
  vbox.set_border_width[ 5];
  frame.add[ vbox];
      
  // Day, month, year spinners
    
  hbox = gtkhbox_new (homogeneous=%f,spacing= 0);
  vbox.pack_start[ hbox, expand=%t,fill= %t,padding= 5];
  
  vbox2 = gtkvbox_new (homogeneous=%f,spacing= 0);
  hbox.pack_start[ vbox2, expand=%t,fill= %t,padding= 5];
  
  label = gtklabel_new (str="Day :");
  label.set_alignment[ 0, 0.5];
  vbox2.pack_start[ label, expand=%f,fill= %t,padding=0]; 
      
  adj =  gtkadjustment_new (value=1.0,lower= 1.0,upper= 31.0,step_incr= 1.0,page_incr= 5.0, page_size=0.0);
  spinner = gtkspinbutton_new (adjustment=adj,climb_rate= 0,digits= 0);
  spinner.set_wrap[%t];

  // spinner.set_shadow_type[GTK.SHADOW_OUT];
  vbox2.pack_start[ spinner,expand=%f,fill= %t,padding=0]; 
  
  vbox2 = gtkvbox_new (homogeneous=%f,spacing= 0);
  hbox.pack_start[ vbox2,expand=%f,fill= %t,padding= 5];
  
  label = gtklabel_new (str="Month :");
  label.set_alignment[ 0, 0.5];
  vbox2.pack_start[ label, expand=%f,fill= %t,padding=0]; 
      
  adj =  gtkadjustment_new (value=1.0,lower= 1.0,upper= 12.0,step_incr= 1.0,page_incr=  5.0, page_size=0.0);
  spinner = gtkspinbutton_new (adjustment=adj,climb_rate= 0,digits= 0);
  spinner.set_wrap[%t];
  // spinner.set_shadow_type[GTK.SHADOW_ETCHED_IN];
  vbox2.pack_start[ spinner,  expand=%f,fill= %t,padding=0]; 
  
  vbox2 = gtkvbox_new (homogeneous=%f,spacing= 0);
  hbox.pack_start[ vbox2, expand=%t,fill= %t,padding=5];

  label = gtklabel_new (str="Year :");
  label.set_alignment[ 0, 0.5];
  vbox2.pack_start[ label, expand=%f,fill= %t,padding=0];  

  adj =  gtkadjustment_new (value=1998.0,lower= 0.0,upper= 2100.0,step_incr=  1.0,page_incr= 100.0, page_size=0.0);
  spinner = gtkspinbutton_new (adjustment=adj,climb_rate= 0,digits= 0);
  spinner.set_wrap[%t];
  //spinner.set_shadow_type[GTK.SHADOW_IN];
  
  //spinner.set_size_request [ 55, 0];
  vbox2.pack_start[ spinner,  expand=%f,fill= %t,padding=0]; 

  frame = gtkframe_new(label="Accelerated");
  main_vbox.pack_start[ frame,  expand=%t,fill= %t,padding=0]; 
  
  vbox = gtkvbox_new (homogeneous=%f,spacing= 0);
  vbox.set_border_width[ 5];
  frame.add[ vbox];
  
  hbox = gtkhbox_new (homogeneous=%f,spacing= 0);
  vbox.pack_start[ hbox,  expand=%f,fill= %t,padding=5]; 
  
  vbox2 = gtkvbox_new (homogeneous=%f,spacing= 0);
  hbox.pack_start[ vbox2, expand=%t,fill= %t,padding= 5];
  
  label = gtklabel_new (str="Value :");
  label.set_alignment [ 0, 0.5];
  vbox2.pack_start[ label, expand=%f,fill= %t,padding=0]; 

  adj =  gtkadjustment_new (value=0.0,lower= -10000.0,upper= 10000.0,step_incr= 0.5,page_incr= 100.0, page_size=0.0);
  spinner1 = gtkspinbutton_new (adjustment=adj,climb_rate= 1,digits= 2);
  spinner1.set_wrap[%t];
  //spinner1.set_size_request [ 100, 0];
  vbox2.pack_start[ spinner1,  expand=%f,fill= %t,padding=0]; 
  
  vbox2 = gtkvbox_new (homogeneous=%f,spacing= 0);
  hbox.pack_start[ vbox2,   expand=%t, fill= %t,padding=5]; 

  label = gtklabel_new (str="Digits :");
  label.set_alignment[ 0, 0.5];
  vbox2.pack_start[ label, expand=%f,fill= %t,padding=0]; 

  adj =  gtkadjustment_new (value=2,lower= 1,upper= 5,step_incr= 1,page_incr= 1, page_size=0);
  spinner2 = gtkspinbutton_new (adjustment=adj,climb_rate= 0,digits= 0);
  spinner2.set_wrap[%t];
  adj.connect[ "value_changed", change_digits,list(spinner1,spinner2)];
  vbox2.pack_start[ spinner2,  expand=%f,fill= %t,padding=0]; 

  hbox = gtkhbox_new (homogeneous=%f,spacing= 0);
  vbox.pack_start[ hbox,  expand=%f,fill= %t,padding=5]; 

  button = gtkcheckbutton_new(label="Snap to 0.5-ticks");
  button.connect[ "clicked",toggle_snap,list(spinner1)];
  vbox.pack_start[ button,   expand=%t,fill= %t,padding=0]; 
  button.set_active [ %t];

  button = gtkcheckbutton_new(label="Numeric only input mode");
  button.connect[ "clicked",toggle_numeric,list(spinner1)];
  vbox.pack_start[ button,  expand=%t,fill= %t,padding=0]; 
  button.set_active [ %t];

  val_label = gtklabel_new (str="");

  hbox = gtkhbox_new (homogeneous=%f,spacing= 0);
  vbox.pack_start[ hbox,   expand=%f,fill= %t,padding=5];
  
  button = gtkbutton_new(label="Value as Int");
  //    	gtk_object_set_user_data (button, val_label);
  button.set_data[user_data= val_label];
  button.connect[ "clicked",get_value,list(1,spinner1)];
  hbox.pack_start[ button,   expand=%t,fill= %t,padding=5];

  button = gtkbutton_new(label="Value as Float");
  button.set_data[user_data= val_label];
  button.connect[ "clicked",get_value,list(2,spinner1)];
  hbox.pack_start[ button,   expand=%t,fill= %t,padding=5];

  vbox.pack_start[ val_label,   expand=%t,fill= %t,padding=0];
  val_label.set_text["0"];

  hbox = gtkhbox_new (homogeneous=%f,spacing= 0);
  main_vbox.pack_start[ hbox,    expand=%f,fill= %t,padding=0];
  
  button = gtkbutton_new(label="Close");
  button.connect[ "clicked",do_quit,list(window)]; 
  hbox.pack_start[ button,    expand=%t,fill= %t,padding=5];
  window.show_all[]
  //gtk_main()
endfunction
