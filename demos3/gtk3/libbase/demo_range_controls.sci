// Range Control Test

function demo_range_controls()

  function y=reformat_value(scale,value)
    y=sprintf("-->%0.*g<--",scale.get_digits[],value);
  endfunction

  win = gtk_window_new()
  win.connect["delete_event", demo_delete];
  win.set_title["range_controls"];
  box1 = gtk_box_new("vertical",spacing=0)
  win.add[box1]
  box1.show[]
  box2 = gtk_box_new("vertical",spacing=10)
  box2.set_border_width[10]
  box1.pack_start[box2]
  box2.show[]
  //adjustment = GtkAdjustment(0,expand= 0,fill= 101,padding= 0.1, 1, 1)
  adjustment =gtk_adjustment_new(value=0,lower=0,upper=101,step_incr=0.1,page_incr=1,page_size=1)

  function show_value(wid,data)
    x=wid.get_value[];
    printf("adjustment value [%f]\n",x);
  endfunction
  adjustment.connect["value-changed",show_value]

  scale = gtk_scale_new("horizontal",adjustment=adjustment)
  scale.set_size_request[150,-1]
  scale.set_digits[1];
  scale.set_draw_value[%t];
  box2.pack_start[scale]
  scale.show[]

  scrollbar = gtk_scrollbar_new("horizontal",adjustment=adjustment)
  box2.pack_start[scrollbar]
  scrollbar.show[]

  scale = gtk_scale_new ("horizontal",adjustment=adjustment);
  scale.set_draw_value[%t];
  scale.connect[ "format_value", reformat_value];
  box2.pack_start[scale]
  scale.show[];

  hbox = gtk_box_new("horizontal",spacing=0);

  scale = gtk_scale_new ("vertical",adjustment=adjustment)
  scale.set_size_request[ -1, 200]
  scale.set_digits[2];
  scale.set_draw_value[%t];
  hbox.pack_start[ scale];
  scale.show[];

  scale = gtk_scale_new ("vertical",adjustment=adjustment)
  scale.set_size_request[  -1, 200]
  scale.set_digits[2];
  scale.set_draw_value[%t];
  scale.set_inverted[%t];
  hbox.pack_start[ scale];
  scale.show[];

  scale = gtk_scale_new ("vertical",adjustment=adjustment)
  scale.set_draw_value[%t];
  scale.connect[   "format_value", reformat_value];
  hbox.pack_start[ scale];
  scale.show[];

  box2.pack_start[hbox];
  hbox.show[];

  separator = gtk_separator_new("horizontal");
  box1.pack_start[separator];
  separator.show[];

  box2 = gtk_box_new("vertical",spacing=10);
  box2.set_border_width[10];
  box1.pack_start[box2];
  box2.show[];

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
