// Pango/Text Mask
//
// This demo shows how to use PangoCairo to draw text with more than
// just a single color.
  

function window = demo_textmask (do_widget)

  function y=draw_text (da, cr, data)

  cairo_save (cr);
  layout = da.create_pango_layout[ "Pango power!\nPango power!\nPango power!"];
  desc = pango_font_description_new("sans bold 34");
  layout.set_font_description[desc];
  
  cairo_move_to (cr, 30, 20);
  pango_cairo_layout_path (cr, layout);
  w=da.get_allocated_width[];
  h=da.get_allocated_height[];
  pattern = cairo_pattern_create_linear (0.0, 0.0,w,h);
  cairo_pattern_add_color_stop_rgb (pattern, 0.0, 1.0, 0.0, 0.0);
  cairo_pattern_add_color_stop_rgb (pattern, 0.2, 1.0, 0.0, 0.0);
  cairo_pattern_add_color_stop_rgb (pattern, 0.3, 1.0, 1.0, 0.0);
  cairo_pattern_add_color_stop_rgb (pattern, 0.4, 0.0, 1.0, 0.0);
  cairo_pattern_add_color_stop_rgb (pattern, 0.6, 0.0, 1.0, 1.0);
  cairo_pattern_add_color_stop_rgb (pattern, 0.7, 0.0, 0.0, 1.0);
  cairo_pattern_add_color_stop_rgb (pattern, 0.8, 1.0, 0.0, 1.0);
  cairo_pattern_add_color_stop_rgb (pattern, 1.0, 1.0, 0.0, 1.0);

  cairo_set_source (cr, pattern);
  cairo_fill_preserve (cr);

  cairo_pattern_destroy (pattern);

  cairo_set_source_rgb (cr, 0.0, 0.0, 0.0);
  cairo_set_line_width (cr, 0.5);
  cairo_stroke (cr);

  cairo_restore (cr);
  y = %t;
  
endfunction 

  window = gtk_window_new (type = GTK.WINDOW_TOPLEVEL);
  window.set_resizable[%t];
  window.set_size_request[400, 200];
  window.set_title[ "Text Mask"];
  // window.connect[ "destroy",  gtk_widget_destroyed, &window);
  da = gtk_drawing_area_new ();
  window.add[da];
  da.connect[ "draw", draw_text];
  window.show_all[];
endfunction 

