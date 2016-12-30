//  Event Axes
//
// Demonstrates advanced handling of event information from exotic
// input devices.
//
// On one hand, this snippet demonstrates management of input axes,
// those contain additional information for the pointer other than
// X/Y coordinates.
//
// Input axes are dependent on hardware devices, on linux/unix you
// can see the device axes through xinput list <device>. Each time
// a different hardware device is used to move the pointer, the
// master device will be updated to match the axes it provides,
// these changes can be tracked through GdkDevice::changed, or
// checking gdk_event_get_source_device().
//
// On the other hand, this demo handles basic multitouch events,
// each event coming from an specific touchpoint will contain a
// GdkEventSequence that's unique for its lifetime, so multiple
// touchpoints can be tracked.

// AxesInfo:
//   last_source;
//   axes; //  axis label atom -> value  
//   GdkRGBA color;
//   gdouble x;
//   gdouble y;

// Event_data:
//   AxesInfo *pointer_info;
//   touch_info; //  GdkEventSequence -> AxesInfo  


function info= axes_info_new ()
  colors=[ "black",
	   "orchid",
	   "fuchsia",
	   "indigo",
	   "thistle",
	   "sienna",
	   "azure",
	   "plum",
	   "lime",
	   "navy",
	   "maroon",
	   "burlywood"];
  persistent(cur_color = 0);
  info = hash(10);
  info.color = gdk_rgba_new (colors(cur_color));
  cur_color = mod((cur_color + 1), size(colors,'*'));
endfunction 

function axes_info_free (info)
  
endfunction

function y = axes_info_lookup (info,axis_label,value)
  atom = gdk_atom_intern (axis_label, %f);
  if (atom == GDK.NONE) then y = %f;return;end 
  val = g_hash_table_lookup (info.axes, GDK.ATOM_TO_POINTER (atom));
  if (~val) then y = %f;return;end 
  value = val;
  y=%t;
endfunction 

function data = event_data_new ()
  // data = g_new0 (EventData, 1);
  // data->touch_info = g_hash_table_new_full (NULL, NULL, NULL,
  // (GDestroyNotify) axes_info_free);
    data = hash(10);
endfunction 

function event_data_free (data)
  if (data.pointer_info)
    axes_info_free (data.pointer_info);
  end
  g_hash_table_destroy (data.touch_info);
  g_free (data);
endfunction 

function update_axes_from_event (event, data)
  device = event.get_device[];
  source_device = event.get_source_device[];
  // XXXX sequence = event.get_event_sequence[];

  if event.type == GDK.TOUCH_END then 
    data.touch_info.remove[sequence];
    return;
  elseif (event.type == GDK.LEAVE_NOTIFY)
    if data.iskey['pointer_info'];
      data.remove['pointer_info'];
      return;
    end
  end
  
  if ~sequence then 
    if ~data.iskey['pointer_info'];
      data.pointer_info = axes_info_new ();
      info = data.pointer_info;
    end
  else
    info = data.touch_info.find[sequence,def='void'];
    if info.equal["void"];
      info = axes_info_new ();
      data.touch_info(sequence)= info;
    end
  end
  
  if info.last_source <> source_device then 
    //g_hash_table_remove_all (info.axes);
    info.last_source = source_device;
  end
  
  if (event.type == GDK.TOUCH_BEGIN ||
    event.type == GDK.TOUCH_UPDATE ||
    event.type == GDK.MOTION_NOTIFY ||
    event.type == GDK.BUTTON_PRESS ||
    event.type == GDK.BUTTON_RELEASE) then 
    
    axes = device.list_axes[];
    if (sequence && event.touch.emulating_pointer)
      if (data.pointer_info)
	axes_info_free (data.pointer_info);
      end
      data.pointer_info = NULL;
    end
    
    for l = axes
      if ~gdk_device_get_axis_value (device, event.motion.axes,	l.data, ...
				     value) then 
	continue;
      end
      ptr = g_new0 (gdouble, 1);
      ptr = value;
      g_hash_table_insert (info.axes, GDK.ATOM_TO_POINTER (l.data), ptr);
    end
    g_list_free (axes);
  end
    
  if (gdk_event_get_coords (event, x, y))
    ////////////////////{
    info.x = x;
    info.y = y;
  end
endfunction

function y = event_cb (widget, event, user_data)
  // update_axes_from_event (event, user_data);
  widget.queue_draw[];
  y= %f
endfunction 

function render_arrow (cr, x_diff, y_diff, label)
  cairo_save (cr);

  cairo_set_source_rgb (cr, 0, 0, 0);
  cairo_new_path (cr);
  cairo_move_to (cr, 0, 0);
  cairo_line_to (cr, x_diff, y_diff);
  cairo_stroke (cr);

  cairo_move_to (cr, x_diff, y_diff);
  cairo_show_text (cr, label);

  cairo_restore (cr);
endfunction 

function draw_axes_info (cr, info, allocation)
    
  pause draw_axes_info
  cairo_save (cr);
  
  cairo_set_line_width (cr, 1);
  gdk_cairo_set_source_rgba (cr, info.color);

  cairo_move_to (cr, 0, info.y);
  cairo_line_to (cr, allocation.width, info.y);
  cairo_move_to (cr, info.x, 0);
  cairo_line_to (cr, info.x, allocation.height);
  cairo_stroke (cr);

  cairo_translate (cr, info.x, info.y);

  if (axes_info_lookup (info, "Abs Pressure", pressure))
    
    pattern = cairo_pattern_create_radial (0, 0, 0, 0, 0, 100);
    cairo_pattern_add_color_stop_rgba (pattern, pressure, 1, 0, 0, pressure);
    cairo_pattern_add_color_stop_rgba (pattern, 1, 0, 0, 1, 0);
    
    cairo_set_source (cr, pattern);

    cairo_arc (cr, 0, 0, 100, 0, 2 * G_PI);
    cairo_fill (cr);

    cairo_pattern_destroy (pattern);
  end

  if (axes_info_lookup (info, "Abs Tilt X", tilt_x) &&
    axes_info_lookup (info, "Abs Tilt Y", tilt_y))
    render_arrow (cr, tilt_x * 100, tilt_y * 100, "Tilt");
  end
  
  if (axes_info_lookup (info, "Abs Wheel", wheel))
    cairo_save (cr);
    cairo_set_line_width (cr, 10);
    cairo_set_source_rgba (cr, 0, 0, 0, 0.5);

    cairo_new_sub_path (cr);
    cairo_arc (cr, 0, 0, 100, 0, wheel * 2 * G_PI);
    cairo_stroke (cr);
    cairo_restore (cr);
  end
  cairo_restore (cr);
endfunction 

function draw_device_info (widget,cr, sequence, y, info)

  pause draw_device_info
  cairo_save (cr);

  string = g_string_new (NULL);
  g_string_append_printf (string, "Source: %s",
  gdk_device_get_name (info.last_source));

  if (sequence)
    g_string_append_printf (string, "\nSequence: %d",
    _TO_UINT (sequence));
  end
  
  cairo_move_to (cr, 10, y);
  layout = gtk_widget_create_pango_layout (widget, string.str);
  pango_cairo_show_layout (cr, layout);
  cairo_stroke (cr);

  pango_layout_get_pixel_size (layout, NULL, height);

  gdk_cairo_set_source_rgba (cr, info.color);
  cairo_set_line_width (cr, 10);
  cairo_move_to (cr, 0, y);

  y = y + height;
  cairo_line_to (cr, 0, y);
  cairo_stroke (cr);

  cairo_restore (cr);

  g_object_unref (layout);
  g_string_free (string, %t);
endfunction

function y= draw_cb (widget, cr, user_data)
    
  printf("In draw\n");
  data = widget.get_data[ "gtk_demo_event_data"];
  data = user_data;
  y = 0;
  allocation = widget.get_allocation[];
  
  //  Draw Abs info 
  if data.iskey['pointer_info'] then 
    draw_axes_info(cr, data.pointer_info, allocation);
  end
    
  if data.iskey['touch_info'] then 
    // loop on elements 
    H = data('touch_info');
    for i=1:size(H.__keys,'*') do
      draw_axes_info (cr, H(H.__keys(i)), allocation);
    end
  end
  
  //  Draw name, color legend and misc data  
  if data.iskey['pointer_info'] then 
    draw_device_info (widget, cr, "", y, data.pointer_info);
  end
  
  if data.iskey['touch_info'] then 
    // loop on elements 
    H = data('touch_info');
    for i=1:size(H.__keys,'*') do
      draw_device_info (widget, cr, key, y, H(H.__keys(i)) );
    end
  end
  y=%f
endfunction 

function window = do_event_axes (toplevel)

  window = gtk_window_new (type=GTK.WINDOW_TOPLEVEL);
  window.set_title[ "Event Axes"];
  window.set_default_size[400, 400];
  
  // window.connect[ "destroy", gtk_widget_destroyed, window];
  
  box = gtk_event_box_new ();
  window.add[box];
  box.add_events[ior([ GDK.POINTER_MOTION_MASK, GDK.BUTTON_PRESS_MASK, ...
		    GDK.BUTTON_RELEASE_MASK, GDK.SMOOTH_SCROLL_MASK, GDK.TOUCH_MASK])];
  
  event_data = event_data_new ();
  box.set_data[ gtk_demo_event_data= event_data];
  box.connect[ "event", event_cb, event_data];
  box.connect[ "draw", draw_cb, event_data];
  window.show_all[];
endfunction 
