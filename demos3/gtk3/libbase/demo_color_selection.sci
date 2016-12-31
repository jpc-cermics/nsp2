// Color Chooser
//
// A GtkColorChooser lets the user choose a color. There are several
// implementations of the GtkColorChooser interface in GTK+. The
// GtkColorChooserDialog is a prebuilt dialog containing a
// GtkColorChooserWidget.

function window=demo_color_selection (do_widget)
  
  function y = draw_callback (da,cr,data)
    window=data(1);
    color=window.get_data['color'];
    CHECK_SIZE=30
    SPACING=2
    xcount = 0;
    width = da.get_allocated_width[];
    height = da.get_allocated_height[];
    i = SPACING;
    while (i < width)
      j = SPACING;
      ycount = modulo(xcount,2); // start with even/odd depending on row */
      while (j < height)
	if modulo(ycount,2)==0 then
	  cairo_set_source_rgb (cr, color.red,color.green,color.blue);
	else
	  cairo_set_source_rgb (cr, 1, 1, 1);
	end
	// If we're outside the clip, this will do nothing.
	cairo_rectangle (cr, i, j, CHECK_SIZE, CHECK_SIZE);
	cairo_fill (cr);
	j = j+ CHECK_SIZE + SPACING;
	ycount=ycount+1;
      end
      i = i+ CHECK_SIZE + SPACING;
      xcount = xcount+1;
    end
    y=%t;
  endfunction
  
  function change_color_callback (button,args)
    window=args(1);
    da=args(2);
    color=window.get_data['color'];
    dialog = gtk_color_chooser_dialog_new("Changing color",window);
    dialog.set_transient_for[args(1)]
    dialog.set_rgba[color];
    response = dialog.run[];
    if response == GTK.RESPONSE_OK
      color= dialog.get_rgba[];
      window.set_data[color=color];
    end
    dialog.destroy[];
  endfunction

  window = gtk_window_new ();
  window.set_title[  "Color Selection"]
  window.set_border_width[  8]
  color= gdk_rgba_new("rgb(0,0,65535)");
  window.set_data[color=color];

  vbox = gtk_box_new("vertical",spacing=8);
  vbox.set_border_width[  8]
  window.add[  vbox]

  frame = gtk_frame_new();
  frame.set_shadow_type[GTK.SHADOW_IN];
  vbox.pack_start[ frame,expand=%t,fill=%t,padding=0]

  darea = gtk_drawing_area_new ();
  darea.connect["draw", draw_callback,list(window)]
  darea.set_size_request[  200, 200]
  frame.add[  darea]

  button = gtk_button_new(mnemonic="_Change the above color");
  vbox.pack_start[ button,expand=%f,fill=%f,padding=0]
  button.connect[  "clicked",change_color_callback,list(window,darea)];
  window.show_all[];
endfunction
