// GtkCursor
//
// Demonstrates a useful set of available cursors.

function demo_cursor()

  function y=cursor_draw (widget,cr,data)
    width = widget.get_allocated_width[];
    height = widget.get_allocated_height[];
    cairo_set_fill_rule(cr, CAIRO.FILL_RULE_EVEN_ODD);
    cairo_rectangle (cr, 0, 0, width, height);
    cairo_rectangle (cr, width / 3, height / 3, width / 3, height / 3);
    cairo_clip (cr);

    cairo_set_source_rgb (cr, 1, 1, 1);
    cairo_rectangle (cr, 0, 0, width, height / 2);
    cairo_fill (cr);

    cairo_set_source_rgb (cr, 0, 0, 0);
    cairo_rectangle (cr, 0, height / 2, width, height / 2);
    cairo_fill (cr);
    y=%t;
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
    //cursor = gdkcursor_new(c);
    cursor = gdk_cursor_new_for_display(spinner.get_display[],c);
    window=widget.get_window[];
    window.set_cursor[cursor=cursor];
    // IL faut implementer la methode name XXXXXXXX
    // cur_name.set_text(cursor.name)
    // args(2).set_text[cursor.name]
  endfunction

  win = gtk_window_new()
  // win.connect["delete_event",demo_delete];
  win.set_title["Cursor Test"];
  main_vbox = gtk_box_new("vertical",spacing=5)
  main_vbox.set_border_width[0]
  win.add[main_vbox]
  main_vbox.show[]
  vbox = gtk_box_new("vertical",spacing=5)
  vbox.set_border_width[10]
  main_vbox.pack_start[vbox]
  vbox.show[]
  hbox=	gtk_box_new("horizontal",spacing= 5)
  vbox.pack_start[hbox,expand= %f,fill=%t,padding=0]
  hbox.show[]
  label = gtk_label_new(str='Cursor value: ')
  //label.set_alignment[0,0.5]
  hbox.pack_start[label,expand= %f,fill=%t,padding=0]
  label.show[]
  adj =  gtk_adjustment_new (value=0,lower=0,upper=152,step_incr=2,page_incr=10, page_size=0);
  spinner = gtk_spin_button_new (adjustment=adj,climb_rate= 0,digits= 0);
  hbox.pack_start[spinner]
  spinner.show[]
  frame = gtk_frame_new(label="Cursor Area")
  frame.set_border_width[10]
  frame.set_label_align[0.5,0]
  vbox.pack_start[frame]
  frame.show[]
  darea = gtk_drawing_area_new()
  darea.set_size_request[80,80]
  frame.add[darea]
  darea.show[]
  cur_name = gtk_label_new()
  vbox.pack_start[cur_name,expand= %f,fill=%t,padding=0]
  cur_name.show[]
  darea.connect["draw", cursor_draw];
  darea.add_events[ior(GDK.EXPOSURE_MASK,GDK.BUTTON_PRESS_MASK)]
  darea.connect["button_press_event", ct_button_press,list( spinner)];
  spinner.connect["changed", set_cursor,list( darea, cur_name)];
  hsep = gtk_separator_new("horizontal")
  main_vbox.pack_start[hsep,expand= %f,fill=%t,padding=0]
  hsep.show[]
  hbox=	gtk_box_new("horizontal",spacing= 5)
  hbox.set_border_width[10]
  main_vbox.pack_start[hbox,expand= %f,fill=%t,padding=0]
  hbox.show[]
  button = gtk_button_new(label="Close")
  // button.connect["clicked", button_destroy_win,list(win)];
  hbox.pack_start[button]
  button.show[]
  win.show[]
  //gtk_main()
endfunction
