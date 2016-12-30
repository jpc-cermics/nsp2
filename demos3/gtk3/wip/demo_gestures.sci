//  Gestures
//
// Perform gestures on touchscreens and other input devices. This
// demo reacts to long presses and swipes from all devices, plus
// multi-touch rotate and zoom gestures.

function swipe_gesture_swept (gesture, velocity_x, velocity_y, widget)
  widget.set_data[ swipe_x = velocity_x / 10];
  widget.set_data[ swipe_y = velocity_y / 10];
  widget.queue_draw[];
endfunction 

function long_press_gesture_pressed (gesture, x, y, widget)
  widget.set_data[long_pressed = %t];
  widget.queue_draw[];
endfunction 

function long_press_gesture_end (gesture,sequence, widget)
  widget.set_data[long_pressed = %f];
  widget.queue_draw[];
endfunction

function rotation_angle_changed (gesture, angle, delta, widget)
  widget.queue_draw[];
endfunction

function zoom_scale_changed (gesture, scale, widget)
  widget.queue_draw[];
endfunction

function y=drawing_area_draw (widget, cr)

  allocation = widget.get_allocation[];
  swipe_x = widget.get_data[ "swipe_x"];
  swipe_y = widget.get_data[ "swipe_y"];
  long_pressed= widget.get_data[ "long_pressed"];
  
  if (swipe_x <> 0 || swipe_y <> 0) then 

    cairo_save (cr);
    cairo_set_line_width (cr, 6);
    cairo_move_to (cr, allocation.width / 2, allocation.height / 2);
    cairo_rel_line_to (cr, swipe_x, swipe_y);
    cairo_set_source_rgba (cr, 1, 0, 0, 0.5);
    cairo_stroke (cr);
    cairo_restore (cr);
  end

  if gtk_gesture_is_recognized (rotate) || gtk_gesture_is_recognized (zoom) then 

    matrix = cairo_get_matrix (cr);
    cairo_matrix_translate (matrix, allocation.width / 2, allocation.height / 2);
    cairo_save (cr);
    angle = rotate.get_angle_delta [];
    cairo_matrix_rotate (matrix, angle);

    scale = zoom.get_scale_delta [];
    cairo_matrix_scale (matrix, scale, scale);

    cairo_set_matrix (cr, matrix);
    cairo_rectangle (cr, -100, -100, 200, 200);

    pat = cairo_pattern_create_linear (-100, 0, 200, 0);
    cairo_pattern_add_color_stop_rgb (pat, 0, 0, 0, 1);
    cairo_pattern_add_color_stop_rgb (pat, 1, 1, 0, 0);
    cairo_set_source (cr, pat);
    cairo_fill (cr);

    cairo_restore (cr);
    cairo_pattern_destroy (pat);
  end

  if long_pressed then 
    cairo_save (cr);
    cairo_arc (cr, allocation.width / 2, allocation.height / 2, 50, 0, 2 * %pi);
    cairo_set_source_rgba (cr, 0, 1, 0, 0.5);
    cairo_stroke (cr);
    cairo_restore (cr);
  end
  y=%t;
endfunction

function window = demo_gestures (do_widget)

  window = gtk_window_new (type=GTK.WINDOW_TOPLEVEL);
  window.set_default_size[400, 400];
  window.set_title[ "Gestures"];
  // window.connect[ "destroy",gtk_widget_destroyed, &window);

  drawing_area = gtk_drawing_area_new ();
  window.add[drawing_area];
  drawing_area.add_events[ior([GDK.BUTTON_PRESS_MASK, GDK.BUTTON_RELEASE_MASK, ...
		    GDK.POINTER_MOTION_MASK, GDK.TOUCH_MASK])];
  drawing_area.connect[ "draw", drawing_area_draw];

  //  Swipe  
  gesture = gtk_gesture_swipe_new (drawing_area);
  gesture.connect[ "swipe", swipe_gesture_swept, drawing_area];
  gesture.set_propagation_phase[ GTK.PHASE_BUBBLE];
  // g_object_weak_ref (drawing_area, (GWeakNotify) g_object_unref, gesture);

  //  Long press  
  gesture = gtk_gesture_long_press_new (drawing_area);
  gesture.connect[ "pressed", long_press_gesture_pressed, drawing_area];
  gesture.connect[ "end", long_press_gesture_end, drawing_area];
  gesture.set_propagation_phase[ GTK.PHASE_BUBBLE];
  // g_object_weak_ref (drawing_area, (GWeakNotify) g_object_unref, gesture);

  //  Rotate  
  gesture = gtk_gesture_rotate_new (drawing_area);
  gesture.connect[ "angle-changed", rotation_angle_changed, drawing_area];
  gesture.set_propagation_phase[ GTK.PHASE_BUBBLE];
  totate = gesture;
  // g_object_weak_ref (drawing_area, (GWeakNotify) g_object_unref, gesture);

  //  Zoom  
  gesture = gtk_gesture_zoom_new (drawing_area);
  gesture.connect[ "scale-changed", zoom_scale_changed, drawing_area];
  gesture.set_propagation_phase[ GTK.PHASE_BUBBLE];
  // g_object_weak_ref (drawing_area, (GWeakNotify) g_object_unref,
  // gesture);
  zoom = gesture;
  
  drawing_area.set_data[zoom= zoom];
  drawing_area.set_data[rotate= rotate];
  drawing_area.set_data[swipe_x = 0];
  drawing_area.set_data[swipe_y = 0];
  drawing_area.set_data[long_pressed = %f];
    
  window.show_all[];
endfunction
