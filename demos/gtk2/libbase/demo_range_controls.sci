
//------------------------------------------
// Range Control Test 
// OK and updated 

function demo_range_controls()
  win = gtkwindow_new()
  win.connect["delete_event", hide];
  win.set_title["range_controls"];
  box1 = gtkvbox_new(homogeneous=%f,spacing=0)
  win.add[box1]
  box1.show[]
  box2 = gtkvbox_new(homogeneous=%f,spacing=10)
  box2.set_border_width[10]
  box1.pack_start[box2]
  box2.show[]
  //adjustment = GtkAdjustment(0,expand= 0,fill= 101,padding= 0.1, 1, 1)
  adjustment =gtkadjustment_new(value=0,lower=0,upper=101,step_incr=0.1,page_incr=1,page_size=1)
  
  scale = gtkhscale_new(adjustment=adjustment)
  scale.set_size_request[150,-1]
  scale.set_update_policy[GTK.UPDATE_DELAYED];
  scale.set_digits[1];
  scale.set_draw_value[%t];
  box2.pack_start[scale]
  scale.show[]

  scrollbar = gtkhscrollbar_new(adjustment=adjustment)
  scrollbar.set_update_policy[GTK.UPDATE_CONTINUOUS]
  box2.pack_start[scrollbar]
  scrollbar.show[]

  scale = gtkhscale_new (adjustment=adjustment);
  scale.set_draw_value[%t];
  scale.connect[ "format_value", reformat_value];
  box2.pack_start[scale]
  scale.show[];
  
  hbox = gtkhbox_new(homogeneous=%f,spacing=0);
  
  scale = gtkvscale_new (adjustment=adjustment)
  scale.set_size_request[ -1, 200]
  scale.set_digits[2];
  scale.set_draw_value[%t];
  hbox.pack_start[ scale];
  scale.show[];

  scale = gtkvscale_new (adjustment=adjustment)
  scale.set_size_request[  -1, 200]
  scale.set_digits[2];
  scale.set_draw_value[%t];
  scale.set_inverted[%t];
  hbox.pack_start[ scale];
  scale.show[];
  
  scale = gtkvscale_new (adjustment=adjustment) 
  scale.set_draw_value[%t];
  scale.connect[   "format_value", reformat_value];
  hbox.pack_start[ scale];
  scale.show[];
        
  box2.pack_start[hbox];
  hbox.show[];
      
  separator = gtkhseparator_new ();
  box1.pack_start[separator];
  separator.show[];
  
  box2 = gtkvbox_new(homogeneous=%f,spacing=10);
  box2.set_border_width[10];
  box1.pack_start[box2];
  box2.show[];
  
  button = gtkbutton_new(label="close")
  button.connect["clicked", win_hide,list(win)];
  box2.pack_start[button]
  button.set_flags[GTK.CAN_DEFAULT]
  button.grab_default[]
  button.show[]
  win.show[]
  //gtk_main()
endfunction

function y=reformat_value(scale,value) 
  y=sprintf("-->%0.*g<--",scale.get_digits[],value);
endfunction
