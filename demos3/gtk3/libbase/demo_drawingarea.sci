// Drawing Area
//
// GtkDrawingArea is a blank area where you can draw custom displays
// of various kinds.
//
// This demo has two drawing areas. The checkerboard area shows
// how you can just draw something; all you have to do is write
// a signal handler for expose_event, as shown here.
//
// The "scribble" area is a bit more advanced, and shows how to handle
// events such as button presses and mouse motion. Click the mouse
// and drag in the scribble area to draw squiggles. Resize the window
// to clear the area.

// Draw a rectangle on the screen

function demo_drawingarea_draw_brush (widget,x, y)
  surface = widget.get_data['surface'];
  rect=gdk_rectangle_new( x - 3,y - 3, 6, 6);
  // Paint to the surface, where we store our state */
  cr = cairo_create (surface);
  cairo_rectangle (cr, rect.x,rect.y,rect.width, rect.height);
  cairo_fill (cr);
  cairo_destroy (cr);
  /// Now invalidate the affected region of the drawing area. */
  window = widget.get_window[];
  window.invalidate_rect[rect=rect, invalidate_children=%f];
endfunction


function window = demo_drawingarea(do_widget)
  window = gtk_window_new (type=GTK.WINDOW_TOPLEVEL);
  if nargin >=1 then  window.set_screen[do_widget.get_screen[]];end
  
  window.set_title["Drawing Area"];
  
  function close_window ()
    if (surface)
      cairo_surface_destroy (surface);
      surface = NULL;
    end
  endfunction
  
  // window.connect["destroy", close_window];

  window.set_border_width[8];
  vbox = gtk_box_new (GTK.ORIENTATION_VERTICAL,spacing= 8);
  vbox.set_border_width[8];
  window.add[vbox];
  ///
  // Create the checkerboard area
  ///
  label = gtk_label_new();
  label.set_markup["<u>Checkerboard pattern</u>"];
  vbox.pack_start[label, expand=%f, fill=%f,padding= 0];
  frame = gtk_frame_new ();
  frame.set_shadow_type[GTK.SHADOW_IN];
  vbox.pack_start[frame, expand=%t, fill=%t,padding= 0];
  da = gtk_drawing_area_new ();
  /// set a minimum size
  da.set_size_request[100, 100];
  frame.add[da];
  
  function y=checkerboard_draw (da,cr, data)
    CHECK_SIZE=10
    SPACING=2
    // At the start of a draw handler, a clip region has been set on
    // the Cairo context, and the contents have been cleared to the
    // widget's background color. The docs for
    // gdk_window_begin_paint_region() give more details on how this
    // works.
    xcount = 0;
    width = da.get_allocated_width[];
    height = da.get_allocated_height[];
    i = SPACING;
    while (i < width)
      j = SPACING;
      ycount = modulo(xcount,2); // start with even/odd depending on row */
      while (j < height)
	if modulo(ycount,2)==0 then
	  cairo_set_source_rgb (cr, 0.45777, 0, 0.45777);
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
    //return TRUE because we've handled this event, so no
    // further processing is required.
    //
    y=%t;
  endfunction
  
  da.connect["draw",checkerboard_draw];
  ///
  // Create the scribble area
  ///
  label = gtk_label_new ();
  label.set_markup["<u>Scribble area</u>"];
  vbox.pack_start[label,expand= %f,fill= %f,padding= 0];
  frame = gtk_frame_new ();
  frame.set_shadow_type[GTK.SHADOW_IN];
  vbox.pack_start[frame,expand= %t,fill= %t,padding= 0];
  da = gtk_drawing_area_new ();
  /// set a minimum size ///
  da.set_size_request[100, 100];
  frame.add[da];
  /// Signals used to handle backing surface ///
  
  // Redraw the screen from the surface
  function y=scribble_draw (widget, cr,data)
    surface = widget.get_data['surface'];
    cairo_set_source_surface (cr, surface, 0, 0);
    cairo_paint (cr);
    y=%f
  endfunction
  
  da.connect["draw",scribble_draw];
  
  // Create a new surface of the appropriate size to store our scribbles
  function y=scribble_configure_event (widget,event, data)
  // if (surface)  cairo_surface_destroy (surface);
    allocation= widget.get_allocation[];
    window = widget.get_window[];
    surface = window.create_similar_surface[CAIRO.CONTENT_COLOR,...
		    allocation.width,...
		    allocation.height];
    widget.set_data[surface=surface];
    // Initialize the surface to white */
    cr = cairo_create (surface);
    cairo_set_source_rgb (cr, 1, 1, 1);
    cairo_paint (cr);
    // cairo_destroy (cr); XXXX
    // We've handled the configure event, no need for further processing. */
    y=%t;
  endfunction;
  
  function y=scribble_button_press_event (widget,event, data)
    surface = widget.get_data['surface'];
    if event.button == 1 // GDK.BUTTON_PRIMARY
      demo_drawingarea_draw_brush (widget, event.x, event.y);
    end
    y=%t;
  endfunction

  function y=scribble_motion_notify_event (widget, event, data)
  // paranoia check, in case we haven't gotten a configure event
    surface = widget.get_data['surface'];
    /// This call is very important; it requests the next motion event.
    // If you don't call gdk_window_get_pointer() you'll only get
    // a single motion event. The reason is that we specified
    // GDK_POINTER_MOTION_HINT_MASK to gtk_widget_set_events().
    // If we hadn't specified that, we could just use event->x, event->y
    // as the pointer location. But we'd also get deluged in events.
    // By requesting the next event as we handle the current one,
    // we avoid getting a huge number of events faster than we
    // can cope.
    //
    // there's a pb here we obtain for event->window a GdkX11Window
    // type in gtype failed for gtype GdkX11Window
    // Maybe in that case we can check if the parent can give a proper nsp
    // type ?
    device=event.device;
    window=event.window;
    rep=window.get_device_position[device];
    if iand(rep(3), GDK.BUTTON1_MASK) then
      demo_drawingarea_draw_brush (widget, rep(1), rep(2));
    end
    /// We've handled it, stop processing */
    y=%t ;
  endfunction
  
  da.connect["configure-event",scribble_configure_event];
  /// Event signals ///
  da.connect["motion-notify-event",scribble_motion_notify_event];
  da.connect["button-press-event", scribble_button_press_event];
  /// Ask to receive events the drawing area doesn't normally
  // subscribe to
  ///
  masks= [GDK.LEAVE_NOTIFY_MASK;
	  GDK.BUTTON_PRESS_MASK;
	  GDK.POINTER_MOTION_MASK;
	  GDK.POINTER_MOTION_HINT_MASK];
  da.set_events[ior(masks)];
  window.show_all[];
endfunction
