// GtkCursor 
// handlers to be done  
//-------------------------------------------------------


function demo_cursor() 

  function ct_expose_event(darea, event)
    drawable = darea.window;
    style = darea.style; 
    white_gc =style.white_gc;
    black_gc = style.black_gc;
    grey_gc = style.get_bg_gc[GTK.STATE_NORMAL]
    wh = drawable.get_size[]
    drawable.draw_rectangle[ white_gc,%t, 0, 0, wh(1),wh(2)/2]
    drawable.draw_rectangle[black_gc, %t, 0,wh(2)/2 ,wh(1), wh(2)/2 ]; 
    drawable.draw_rectangle[grey_gc, %t, wh(1)/3, wh(2)/3, wh(1)/3, wh(2)/3];
  endfunction 
  
  function ct_button_press(widget, event, args) 
    if event.type == GDK.BUTTON_PRESS then 
      if event.button == 1 then 
	args(1).spin[GTK.SPIN_STEP_FORWARD, 0]
      elseif event.button == 3 then 
	args(1).spin[GTK.SPIN_STEP_BACKWARD, 0];
      end 
    end
  endfunction 

  function set_cursor(spinner, args) 
    widget= args(1)
    cur_name = args(2)
    //c = spinner.get_value_as_int()
    c= min(max(0,spinner.get_value_as_int[]),152);
    c = iand(c,254);  // 0xfe
    printf("valeur de c %d\n",c);  
    cursor = gdkcursor_new(c) 
    widget.window.set_cursor[cursor];
    // IL faut implementer la methode name XXXXXXXX 
    // cur_name.set_text(cursor.name)
    // args(2).set_text[cursor.name]
  endfunction 
  
  win = gtkwindow_new()
  win.connect["delete_event", hide];
  win.set_title["Cursor Test"];
  main_vbox = gtkvbox_new(homogeneous=%f,spacing=5)
  main_vbox.set_border_width[0]
  win.add[main_vbox]
  main_vbox.show[]
  vbox = gtkvbox_new(homogeneous=%f,spacing=5)
  vbox.set_border_width[10]
  main_vbox.pack_start[vbox]
  vbox.show[]
  hbox=	gtkhbox_new(homogeneous= %f,spacing= 5)
  vbox.pack_start[hbox,expand= %f,fill=%t,padding=0]
  hbox.show[]
  label = gtklabel_new(str='Cursor value: ')
  label.set_alignment[0,0.5]
  hbox.pack_start[label,expand= %f,fill=%t,padding=0]
  label.show[]
  adj =  gtkadjustment_new (value=0,lower=0,upper=152,step_incr=2,page_incr=10, page_size=0);
  spinner = gtkspinbutton_new (adjustment=adj,climb_rate= 0,digits= 0);
  hbox.pack_start[spinner]
  spinner.show[]
  frame = gtkframe_new(label="Cursor Area")
  frame.set_border_width[10]
  frame.set_label_align[0.5,0]
  vbox.pack_start[frame]
  frame.show[]
  darea = gtkdrawingarea_new() 
  darea.set_size_request[80,80]
  frame.add[darea]
  darea.show[]
  cur_name = gtklabel_new()
  vbox.pack_start[cur_name,expand= %f,fill=%t,padding=0]
  cur_name.show[]
  darea.connect["expose_event",ct_expose_event];
  darea.add_events[ior(GDK.EXPOSURE_MASK,GDK.BUTTON_PRESS_MASK)]
  darea.connect["button_press_event", ct_button_press,list( spinner)];
  spinner.connect["changed", set_cursor,list( darea, cur_name)];
  hsep = gtkhseparator_new()
  main_vbox.pack_start[hsep,expand= %f,fill=%t,padding=0]
  hsep.show[]
  hbox=	gtkhbox_new(homogeneous= %f,spacing= 5)
  hbox.set_border_width[10]
  main_vbox.pack_start[hbox,expand= %f,fill=%t,padding=0]
  hbox.show[]
  button = gtkbutton_new(label="Close")
  button.connect["clicked", win_hide,list(win)];
  hbox.pack_start[button]
  button.show[]
  win.show[]
  //gtk_main()
endfunction
